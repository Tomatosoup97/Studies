#ifndef _UDP_H_
#define _UDP_H_

#include "common.h"
#include "table.h"

#define LISTENING_PORT 54321

#define get_network_addr(addr, mask_len) (addr & last_k_bits_mask(mask_len))

#define calc_broadcast_addr(network_addr, mask_len)\
    (network_addr | first_k_bits_mask(32 - mask_len))

#define printf_ip_addr(str, addr) ({\
    ip_addr_v ip_addr;\
    inet_ntop(AF_INET, &addr, ip_addr.ip, 32);\
    printf(str, ip_addr.ip);\
})

void send_udp_packet(int sockfd, ip_addr_t ip_addr, uint8_t *buffer, ssize_t buff_len);
ip_addr_t receive_udp_packet(int sockfd, uint8_t *buffer, size_t buff_len);

void encode_udp_payload(node_t *node, uint8_t *buffer);
void decode_udp_payload(node_t *node, uint8_t *buffer);

int select_socket(int sockfd, int secs, int usecs);
int create_bcast_socket();
int bind_socket();

#endif

