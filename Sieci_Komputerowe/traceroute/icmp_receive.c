#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include <assert.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#define TRUE 1
#define FALSE 0
#define ICMP_ECHO__NO_CODE 0

// Program flags
#define DEBUG 1


uint16_t compute_icmp_checksum (const void *buff, int length)
{
    uint32_t sum;
    const uint16_t* ptr = buff;
    assert (length % 2 == 0);
    for (sum = 0; length > 0; length -= 2)
        sum += *ptr++;
    sum = (sum >> 16) + (sum & 0xffff);
    return (uint16_t)(~(sum + (sum >> 16)));
}

void send_icmp_request(int sockfd, int ttl, const char *ip_addr) {
    static int count;

    struct icmphdr icmp_header;
    icmp_header.type = ICMP_ECHO;
    icmp_header.code = ICMP_ECHO__NO_CODE;
    icmp_header.un.echo.id = getpid();
    icmp_header.un.echo.sequence = count++;
    icmp_header.checksum = 0;
    icmp_header.checksum = compute_icmp_checksum((u_int16_t*) &icmp_header,
                                                 sizeof(icmp_header));

    struct sockaddr_in recipient;
    bzero(&recipient, sizeof(recipient));
    recipient.sin_family = AF_INET;
    inet_pton(AF_INET, ip_addr, &recipient.sin_addr);

    setsockopt(sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

    ssize_t bytes_sent = sendto(
        sockfd, &icmp_header,
        sizeof(icmp_header), 0,
        (struct sockaddr*) &recipient,
        sizeof(recipient)
    );
    if (DEBUG) printf("Sent %ld bytes to %s\n", bytes_sent, ip_addr);
}

void print_as_bytes (unsigned char* buff, ssize_t length)
{
    for (ssize_t i = 0; i < length; i++, buff++)
        printf ("%.2x ", *buff);
}

int main()
{
    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sockfd < 0) {
        fprintf(stderr, "socket error: %s\n", strerror(errno));
        return EXIT_FAILURE;
    }
    int i = 0;

    for (;;) {

        if (i < 5) {
            send_icmp_request(sockfd, i++, "8.8.8.8");
        }

        struct sockaddr_in  sender;
        socklen_t sender_len = sizeof(sender);
        u_int8_t buffer[IP_MAXPACKET];

        ssize_t packet_len = recvfrom (sockfd, buffer, IP_MAXPACKET, 0, (struct sockaddr*)&sender, &sender_len);
        if (packet_len < 0) {
            fprintf(stderr, "recvfrom error: %s\n", strerror(errno));
            return EXIT_FAILURE;
        }

        char sender_ip_str[20];
        inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));
        printf ("Received IP packet with ICMP content from: %s\n", sender_ip_str);

        struct iphdr*   ip_header = (struct iphdr*) buffer;
        ssize_t     ip_header_len = 4 * ip_header->ihl;

        printf ("IP header: ");
        print_as_bytes (buffer, ip_header_len);
        printf("\n");

        printf ("IP data:   ");
        print_as_bytes (buffer + ip_header_len, packet_len - ip_header_len);
        printf("\n\n");
    }

    return EXIT_SUCCESS;
}
