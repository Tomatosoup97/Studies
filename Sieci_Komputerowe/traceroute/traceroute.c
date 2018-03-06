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
#define NO_CODE 0

/* Program flags */

#define TTL_THRESHOLD 30   // Max TTL for sent packet
#define MAX_SECS_TO_WAIT 1 // Max seconds to wait for incoming packet
#define PACKETS_PER_TTL 3  // How many packets are sent for each TTL

#define DEBUG 0

void handle_error(char *err_msg) {
    const int BUFF_SIZE = 50;
    char err_str[BUFF_SIZE];
    strerror_r(errno, err_str, BUFF_SIZE);
    printf("Error occured. msg: %s, error: %s\n", err_str, err_msg);
    exit(EXIT_FAILURE);
}

uint16_t compute_icmp_checksum (const void *buff, int length) {
    uint32_t sum;
    const uint16_t* ptr = buff;
    assert (length % 2 == 0);
    for (sum = 0; length > 0; length -= 2)
        sum += *ptr++;
    sum = (sum >> 16) + (sum & 0xffff);
    return (uint16_t)(~(sum + (sum >> 16)));
}


void print_as_bytes(unsigned char* buff, ssize_t length) {
    for (ssize_t i = 0; i < length; i++, buff++)
        printf("%.2x ", *buff);
}


void send_icmp_request(int sockfd, int ttl, const char *ip_addr) {
    static int count = 1;

    struct icmphdr icmp_header;
    icmp_header.type = ICMP_ECHO;
    icmp_header.code = NO_CODE;
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

ssize_t receive_icmp_packet(int sockfd, u_int8_t *buffer, struct sockaddr_in *sender) {
    socklen_t sender_len = sizeof(sender);

    ssize_t packet_len = recvfrom(
            sockfd, buffer,
            IP_MAXPACKET, 0,
            (struct sockaddr*) sender,
            &sender_len
    );

    if (packet_len < 0)
        handle_error("recvfrom error");

    return packet_len;
}


int select_icmp_packet(int sockfd) {
    /* Wait up to MAX_SECS_TO_WAIT for receiving icmp packet
     * */
    fd_set descriptors;
    FD_ZERO(&descriptors);
    FD_SET(sockfd, &descriptors);

    struct timeval tv;
    tv.tv_sec = MAX_SECS_TO_WAIT;
    tv.tv_usec = 0;

    return select(sockfd+1, &descriptors, NULL, NULL, &tv);
}

void extract_sender_ip_str(struct sockaddr_in *sender, char *ip_str_buff, ssize_t buff_size) {
    inet_ntop(AF_INET, &(sender->sin_addr), ip_str_buff, buff_size);
}

int is_ip_addr_valid(const char *ip_addr) {
    // TODO
    if (!ip_addr[0]) return FALSE;
    return TRUE;
}

int packet_reached_destination(struct icmphdr *icmp_header) {
    return icmp_header->type == ICMP_ECHOREPLY && \
           icmp_header->code == NO_CODE && \
           icmp_header->un.echo.id == getpid();
}

void output_icmp_packet(u_int8_t *buffer, struct icmphdr *icmp_header,
                        ssize_t ip_header_len, ssize_t packet_len) {
    printf ("IP header: ");
    print_as_bytes(buffer, ip_header_len);
    printf("\n");

    printf ("IP data:   ");
    print_as_bytes(buffer + ip_header_len, packet_len - ip_header_len);
    printf("\n");

    printf("ICMP header: type=%d, code=%d, id=%d %d\n",
            icmp_header->type,
            icmp_header->code,
            icmp_header->un.echo.id,
            icmp_header->un.echo.sequence);

    printf("ICMP data: ");

    printf("\n\n");

}

int handle_icmp_packet(int sockfd, u_int8_t *buffer, int TTL,
                       const char *ip_addr, struct sockaddr_in *sender) {

    send_icmp_request(sockfd, TTL, ip_addr);
    int is_ready = select_icmp_packet(sockfd);
    ssize_t packet_len;

    switch (is_ready) {
        case 0:
            // timeout
            packet_len = -1;
            break;
        case 1:
            packet_len = receive_icmp_packet(sockfd, buffer, sender);
            break;
        default:
            handle_error("select_icmp_packet");
    }
    return packet_len;
}

void help() {
    printf("usage: ./traceroute <ip-address>\n");
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    if (argc != 2)
        help();

    // TODO: accept domain name and run `host` on the argument
    const char *ip_addr = argv[1];
    if (!is_ip_addr_valid(ip_addr))
        handle_error("Invalid ip address");

    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sockfd < 0)
        handle_error("socket");

    for (int TTL = 1; TTL <= TTL_THRESHOLD; TTL++) {
        u_int8_t buffer[IP_MAXPACKET];
        ssize_t packet_len;
        struct sockaddr_in sender;

        /* for (int i=0; i<PACKETS_PER_TTL; i++) { */
        /*     // Send 3 packets with same TTL */
        /* } */

        packet_len = handle_icmp_packet(sockfd, buffer, TTL, ip_addr, &sender);

        if (packet_len == -1) {
            // timeout
            printf("%d. *\n", TTL);
            continue;
        }

        char sender_ip_str[20];
        extract_sender_ip_str(&sender, sender_ip_str, 20);

        // TODO: how can i tell if incoming ICMP packet is mine?
        printf("%d. %s\n", TTL, sender_ip_str);

        struct iphdr *ip_header = (struct iphdr*) buffer;
        ssize_t ip_header_len = 4 * ip_header->ihl;
        u_int8_t *icmp_packet = buffer + ip_header_len;
        struct icmphdr *icmp_header = (struct icmphdr*) icmp_packet;
        struct icmp *icmp_content = (struct icmp*) (buffer + ip_header_len + sizeof(struct icmphdr));

        // TODO: print latency for each ip_addr

        if (DEBUG)
            output_icmp_packet(buffer, icmp_header, ip_header_len, packet_len);

        if (packet_reached_destination(icmp_header))
            break;
    }

    return EXIT_SUCCESS;
}
