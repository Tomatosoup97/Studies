#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>
#include "table.h"
#include "udp.h"
#include "common.h"


void init_routing_table(routing_table_t *table, int max_size) {
    table->max_size = max_size;
    table->nodes = malloc(max_size * sizeof(node_t));
}

void show_routing_table(routing_table_t *table) {
    for (int i=0; i<table->nodes_count; i++)
        show_node(&table->nodes[i]);
}

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
            node->reachability == UNREACHABLE ? "unreachable" : "distance %d",
            node->distance);

    printf("%s %s %s\n", network_addr.ip, distance_msg, connection_msg);
}

void read_node(node_t *node) {
    ip_addr_v ip_addr;
    scanf("%s %*s %d", ip_addr.ip, &node->distance);

    char *subnet_mask_str = strchr(ip_addr.ip, '/')+1;
    char *router_addr = strtok(ip_addr.ip, "/");

    sscanf(subnet_mask_str, "%" SCNu16, &node->subnet_mask_len);
    inet_pton(AF_INET, router_addr, &node->router_addr);

    uint32_t router_addr_host = ntohl(node->router_addr.s_addr);
    uint32_t network_addr_host = get_network_addr(router_addr_host,
                                                  node->subnet_mask_len);

    node->network_addr.s_addr = htonl(network_addr_host);
    node->conn_type = CONN_DIRECT;
    node->reachability = INITIAL_REACHABILITY;
}

void read_node_from_socket(int sockfd, node_t *node) {
    uint8_t buffer[IP_MAXPACKET+1];
    ip_addr_t sender_ip_addr;

    sender_ip_addr = receive_udp_packet(sockfd, buffer, IP_MAXPACKET);
    decode_udp_payload(node, buffer);
    node->router_addr = sender_ip_addr;
    node->conn_type = CONN_INDIRECT;
    node->reachability = INITIAL_REACHABILITY;
}

int find_fst_free_slot(routing_table_t *table) {
    for (int i=0; i<table->max_size; i++)
        if (table->nodes[i].conn_type == CONN_UNDEFINED)
            return i;
    return -1;
}

int find_node_by_network_addr(routing_table_t *table, ip_addr_t *addr) {
    for (int i=0; i<table->max_size; i++)
        if (table->nodes[i].network_addr.s_addr == addr->s_addr)
            return i;
    return -1;
}

void append_node_to_table(routing_table_t *table, node_t *node) {
    int free_slot = find_fst_free_slot(table);
    if (free_slot == -1) handle_error("no free slots in table");
    table->nodes[free_slot] = *node;
    table->nodes_count++;
}

void update_node_in_table(routing_table_t *table, node_t *new_node) {
    int curr_node_idx = find_node_by_network_addr(table, &new_node->network_addr);

    if (curr_node_idx == -1) {
        append_node_to_table(table, new_node);
    } else if (table->nodes[curr_node_idx].distance > new_node->distance &&
               table->nodes[curr_node_idx].conn_type != CONN_DIRECT) {
        table->nodes[curr_node_idx] = *new_node;
    }
}

