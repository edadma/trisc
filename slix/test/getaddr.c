/* mgetaddr — exercise musl's getaddrinfo() against /etc/hosts.
 *
 * Phase 4 chunk 1a of the SLIX netstack-quality plan: verify that
 * with /etc/hosts pre-populated by the ramdisk maker, musl's
 * __lookup_name reaches name_from_hosts() and returns 127.0.0.1
 * for "localhost". No dhclient and no DNS round-trip; this chunk
 * is purely the file-read path through VFS.
 *
 * Status (2026-04-28): ramdisk + brk syscall landed, but
 * `getaddrinfo` itself returns EAI_MEMORY (-10) because musl's
 * mallocng requires `mmap` for slot allocation — `brk` alone is
 * insufficient. See project_slix_musl_heap_gap.md. The NshTests
 * entry is marked `ignore` until mmap (or a slix-specific malloc
 * replacement) lands.
 *
 * Output (success):
 *   mgetaddr: localhost -> 127.0.0.1
 *   mgetaddr: ok
 *
 * Output (failure):
 *   mgetaddr: getaddrinfo rc=<errno>
 *   mgetaddr: failed
 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
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

    int rc = getaddrinfo("localhost", 0, &hint, &res);
    if (rc != 0 || !res) {
        wstr("mgetaddr: getaddrinfo rc=");
        wint(rc);
        wstr("\n");
        wstr("mgetaddr: failed\n");
        return 1;
    }

    struct sockaddr_in *sa = (struct sockaddr_in *)res->ai_addr;
    wstr("mgetaddr: localhost -> ");
    wip((unsigned int)sa->sin_addr.s_addr);
    wstr("\n");

    if ((unsigned int)sa->sin_addr.s_addr != htonl(0x7f000001)) {
        wstr("mgetaddr: failed\n");
        freeaddrinfo(res);
        return 2;
    }

    freeaddrinfo(res);
    wstr("mgetaddr: ok\n");
    return 0;
}
