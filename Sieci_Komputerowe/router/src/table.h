#ifndef _TABLE_H_
#define _TABLE_H_

#include <stdint.h>
#include <netinet/in.h>
#include <limits.h>

#define CONN_UNDEFINED 0
#define CONN_DIRECT 1
#define CONN_INDIRECT 2

#define INFINITE_DISTANCE 16
#define UNREACHABLE INT_MAX
#define INITIAL_REACHABILITY 5

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

typedef struct {
    node_t *nodes;
    int nodes_count;
    int max_size;
} routing_table_t;

void init_routing_table(routing_table_t *table, int size);

void show_routing_table(routing_table_t *table);

void show_node(node_t *node);

void read_node(node_t *node);
void read_node_from_socket(int sockfd, node_t *node);

int find_node_by_network_addr(routing_table_t *table, ip_addr_t *addr);
int find_fst_free_slot(routing_table_t *table);

void append_node_to_table(routing_table_t *table, node_t *node);
void update_node_in_table(routing_table_t *table, node_t *new_node);

#endif

