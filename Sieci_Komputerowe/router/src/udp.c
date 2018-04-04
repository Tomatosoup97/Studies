#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "udp.h"

ip_addr_t translate_to_network_addr(ip_addr_t *addr, int mask_len) {
    ip_addr_t network;
    uint32_t router_addr_host = ntohl(addr->s_addr);
    uint32_t network_addr_host = get_network_addr(router_addr_host,
                                                  mask_len);
    network.s_addr = htonl(network_addr_host);
    return network;
}

void send_udp_packet(int sockfd, ip_addr_t ip_addr, uint8_t *buffer, ssize_t buff_len) {
    struct sockaddr_in recipent_addr;
    bzero(&recipent_addr, sizeof(recipent_addr));
    recipent_addr.sin_family = AF_INET;
    recipent_addr.sin_port = htons(LISTENING_PORT);
    recipent_addr.sin_addr.s_addr = ip_addr.s_addr;

    // send through localhost:
    // inet_pton(AF_INET, "127.0.0.1", &server_address.sin_addr);

    ssize_t message_len = sendto(
        sockfd, buffer, buff_len, 0,
        (struct sockaddr*) &recipent_addr,
        sizeof(recipent_addr)
    );
    if (message_len != buff_len) handle_error("sendto error");
    if (DEBUG) {
        printf("UDP packet of size %ld ", buff_len);
        printf_ip_addr("was sent to %s. Payload: ", ip_addr);
        print_buff(buffer, buff_len);
    }
}

ip_addr_t receive_udp_packet(int sockfd, uint8_t *buffer, size_t buff_len) {
    /* Receive UDP packet from socket and return sender ip addr */
    struct sockaddr_in sender;
    socklen_t sender_len = sizeof(sender);

    ssize_t datagram_len = recvfrom(
        sockfd, buffer, buff_len, MSG_DONTWAIT,
        (struct sockaddr*)&sender, &sender_len
    );
    if (datagram_len < 0) handle_error("recvfrom error");

    char sender_ip_str[20];
    inet_ntop(AF_INET, &(sender.sin_addr), sender_ip_str, sizeof(sender_ip_str));

    if (DEBUG) {
        printf("Received UDP packet from IP addr: %s, %ld-byte message: ",
               sender_ip_str, datagram_len);
        print_buff(buffer, datagram_len);
    }
    fflush(stdout);
    return sender.sin_addr;
}

void encode_udp_payload(node_t *node, uint8_t *buffer) {
    uint32_t distance = htonl(node->distance);
    memcpy(buffer, &node->network_addr.s_addr, 4);
    memcpy(buffer+4, &node->subnet_mask_len, 1);
    memcpy(buffer+5, &distance, 4);
}

void decode_udp_payload(node_t *node, uint8_t *buffer) {
    uint32_t ndistance;
    memcpy(&ndistance, buffer+5, 4);
    memcpy(&node->subnet_mask_len, buffer+4, 1);
    memcpy(&node->network_addr.s_addr, buffer, 4);
    node->distance = ntohl(ndistance);
}

int select_socket(int sockfd, int secs, int milisecs) {
    fd_set descriptors;
    FD_ZERO(&descriptors);
    FD_SET(sockfd, &descriptors);

    struct timeval tv;
    tv.tv_sec = secs;
    tv.tv_usec = milisecs * 1000;

    return select(sockfd+1, &descriptors, NULL, NULL, &tv);
}

int bind_socket() {
    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) handle_error("socket error");

    struct sockaddr_in server_address;
    bzero(&server_address, sizeof(server_address));
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(LISTENING_PORT);
    server_address.sin_addr.s_addr = htonl(INADDR_ANY);

    int bind_res = bind(
        sockfd,
        (struct sockaddr*) &server_address,
        sizeof(server_address)
    );
    if (bind_res < 0) handle_error("bind error");
    return sockfd;
}

int create_bcast_socket() {
    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) handle_error("socket error");
    int broadcastEnable = 1;
    int set_result = setsockopt(sockfd, SOL_SOCKET, SO_BROADCAST,
                                &broadcastEnable, sizeof(broadcastEnable));
    if (set_result < 0) handle_error("setsockopt error");

    return sockfd;
}

