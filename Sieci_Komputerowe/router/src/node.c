#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "table.h"
#include "node.h"

void show_node(node_t *node) {
    ip_addr_v router_addr, network_addr;
    char connection_msg[64];
    char distance_msg[64];

    inet_ntop(AF_INET, &node->router_addr.s_addr, router_addr.ip, 32);
    inet_ntop(AF_INET, &node->network_addr.s_addr, network_addr.ip, 32);

    sprintf(connection_msg,
            node->conn_type == CONN_DIRECT ? "connected directly" : "via %s",
            router_addr.ip);
    sprintf(distance_msg,
            node->distance == UNREACHABLE ? "unreachable" : "distance %u",
            node->distance);


    printf("%s/%d %s %s", network_addr.ip, node->subnet_mask_len,
                            distance_msg, connection_msg);
    if (VERBOSE) printf(" reachability %d", node->reachability);
    printf("\n");
}

void read_node(node_t *node) {
    ip_addr_v ip_addr;
    scanf("%s %*s %u", ip_addr.ip, &node->distance);

    char *subnet_mask_str = strchr(ip_addr.ip, '/') + 1;
    char *router_addr = strtok(ip_addr.ip, "/");

    sscanf(subnet_mask_str, "%" SCNu16, &node->subnet_mask_len);
    inet_pton(AF_INET, router_addr, &node->router_addr);

    ip_addr_t network_addr = translate_to_network_addr(&node->router_addr,
                                                       node->subnet_mask_len);

    node->network_addr.s_addr = network_addr.s_addr;
    node->conn_type = CONN_DIRECT;
    node->reachability = INITIAL_REACHABILITY;
}

void read_node_from_socket(int sockfd, node_t *node) {
    uint8_t buffer[IP_MAXPACKET+1];
    ip_addr_t sender_ip_addr;

    sender_ip_addr = receive_udp_packet(sockfd, buffer, IP_MAXPACKET);

    decode_udp_payload(node, buffer);
    node->router_addr = sender_ip_addr;
    node->reachability = node->distance == UNREACHABLE ? 0 : INITIAL_REACHABILITY;
}

