/* mmsg: validate sendmsg / recvmsg through musl's libc wrappers.
 *
 * UDP loopback round-trip with a 2-iov gather (sendmsg) and a
 * single-iov scatter (recvmsg). If the shim's msghdr offset
 * parsing drifts from musl's struct layout this fails loudly:
 * the gathered string would be wrong, recvmsg would return
 * garbage, or the source-address writeback would mis-target.
 */
#include <sys/socket.h>
#include <sys/uio.h>
#include <netinet/in.h>
#include <arpa/inet.h>
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

int main(void) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    wstr("mmsg: socket=");
    wint(fd);
    wstr("\n");
    if (fd < 0) return 1;

    struct sockaddr_in baddr = {0};
    baddr.sin_family = AF_INET;
    baddr.sin_port = htons(7790);
    baddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int br = bind(fd, (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("mmsg: bind=");
    wint(br);
    wstr("\n");
    if (br < 0) return 2;

    struct sockaddr_in dst = {0};
    dst.sin_family = AF_INET;
    dst.sin_port = htons(7790);
    dst.sin_addr.s_addr = inet_addr("127.0.0.1");

    struct iovec siov[2];
    siov[0].iov_base = (void *)"hello ";
    siov[0].iov_len  = 6;
    siov[1].iov_base = (void *)"msghdr";
    siov[1].iov_len  = 6;

    struct msghdr smsg = {0};
    smsg.msg_name    = &dst;
    smsg.msg_namelen = sizeof(dst);
    smsg.msg_iov     = siov;
    smsg.msg_iovlen  = 2;

    long sent = sendmsg(fd, &smsg, 0);
    wstr("mmsg: sendmsg=");
    wint(sent);
    wstr("\n");

    char rbuf[64];
    struct iovec riov;
    riov.iov_base = rbuf;
    riov.iov_len  = sizeof(rbuf);
    struct sockaddr_in src = {0};
    struct msghdr rmsg = {0};
    rmsg.msg_name    = &src;
    rmsg.msg_namelen = sizeof(src);
    rmsg.msg_iov     = &riov;
    rmsg.msg_iovlen  = 1;
    long got = recvmsg(fd, &rmsg, 0);
    wstr("mmsg: recvmsg=");
    wint(got);
    wstr(" data='");
    if (got > 0)
        write(1, rbuf, got);
    wstr("'\n");
    wstr("mmsg: src_port=");
    wint(ntohs(src.sin_port));
    wstr("\n");

    close(fd);
    wstr("mmsg: done\n");
    return 0;
}
