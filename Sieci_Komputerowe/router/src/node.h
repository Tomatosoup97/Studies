#ifndef _NODE_H_
#define _NODE_H_

#include "common.h"
#include "table.h"
#include "udp.h"

typedef struct {
    char ip[32];
} ip_addr_v;

typedef struct in_addr ip_addr_t;

typedef struct {
    uint16_t subnet_mask_len;
    ip_addr_t router_addr;
    ip_addr_t network_addr;
    int conn_type;
    int32_t reachability;
    uint32_t distance;
} node_t;

void show_node(node_t *node);

void read_node(node_t *node);

void read_node_from_socket(int sockfd, node_t *node);

#endif

