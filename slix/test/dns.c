/* mdns — exercise musl's getaddrinfo() over slirp's stub DNS.
 *
 * Phase 4 chunk 1c of the SLIX netstack-quality plan: with
 * /etc/resolv.conf populated by chunk 1b's dhclient, drive the
 * full musl resolver path — open a UDP socket, send the DNS
 * query packet to the nameserver, recvfrom the answer, parse
 * the A record, return through getaddrinfo().
 *
 * Slirp's user-mode networking provides a stub DNS forwarder at
 * 10.0.2.3:53 that proxies to whatever the host's resolver is
 * configured to use. So this is a real DNS query: guest → slirp
 * stub → host's upstream → answer back. Requires a working host
 * DNS at test time.
 *
 * example.com is RFC 2606 reserved with stable A records, so
 * the test asserts only the format ("mdns: example.com -> ...")
 * and that getaddrinfo did not fail — we don't pin a specific
 * IP that could rotate.
 *
 * Internally retries up to 3 times with 2 s backoff on
 * EAI_AGAIN-style transients: macOS mDNSResponder occasionally
 * returns SERVFAIL on cold lookups, surfacing as rc=-3 to musl.
 * The retry suppresses intermediate "rc="/"failed" output so the
 * Scala test's success-path assertions still hold across hiccups.
 * On exhaustion the final attempt's rc is reported.
 *
 * Output (success):
 *   mdns: example.com -> A.B.C.D
 *   mdns: ok
 *
 * Output (final failure):
 *   mdns: getaddrinfo rc=<errno>
 *   mdns: failed
 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>
#include <unistd.h>
#include <string.h>

static void wstr(const char *s) {
    size_t n = 0;
    while (s[n]) n++;
    write(1, s, n);
}

static void wint(long v) {
    char buf[24];
    int i = 0, neg = 0;
    if (v < 0) { neg = 1; v = -v; }
    if (v == 0) buf[i++] = '0';
    while (v > 0) { buf[i++] = '0' + (v % 10); v /= 10; }
    if (neg) buf[i++] = '-';
    char out[24];
    for (int j = 0; j < i; j++) out[j] = buf[i - 1 - j];
    write(1, out, i);
}

static void wip(unsigned int ip_be) {
    unsigned char *p = (unsigned char *)&ip_be;
    wint(p[0]); wstr(".");
    wint(p[1]); wstr(".");
    wint(p[2]); wstr(".");
    wint(p[3]);
}

int main(void) {
    struct addrinfo hint;
    struct addrinfo *res = 0;

    memset(&hint, 0, sizeof(hint));
    hint.ai_family = AF_INET;
    hint.ai_socktype = SOCK_STREAM;

    int rc = 0;
    for (int attempt = 0; attempt < 3; attempt++) {
        if (attempt > 0) {
            struct timespec ts = { .tv_sec = 2, .tv_nsec = 0 };
            nanosleep(&ts, 0);
        }
        rc = getaddrinfo("example.com", "443", &hint, &res);
        if (rc == 0 && res) break;
        if (res) { freeaddrinfo(res); res = 0; }
    }
    if (rc != 0 || !res) {
        wstr("mdns: getaddrinfo rc=");
        wint(rc);
        wstr("\n");
        wstr("mdns: failed\n");
        return 1;
    }

    struct sockaddr_in *sa = (struct sockaddr_in *)res->ai_addr;
    wstr("mdns: example.com -> ");
    wip((unsigned int)sa->sin_addr.s_addr);
    wstr("\n");

    if (sa->sin_addr.s_addr == 0) {
        wstr("mdns: failed\n");
        freeaddrinfo(res);
        return 2;
    }

    freeaddrinfo(res);
    wstr("mdns: ok\n");
    return 0;
}
