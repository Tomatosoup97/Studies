#ifndef _TABLE_H_
#define _TABLE_H_

#define DIRECT_CONN 1
#define INFINITE_DISTANCE 32
#define TRUE 1
#define FALSE 0

typedef struct ip_addr {
    char ip[32];
} ip_addr_t;

typedef struct node {
    ip_addr_t ip_addr;
    ip_addr_t through;
    int distance;
    int conn_type;
    int is_reachable;
} node_t;

typedef struct routing_table {
    node_t *nodes;
    int nodes_count;
    int size;
} routing_table_t;

void init_routing_table(routing_table_t *table, int size);

void propagate_distance_vector(routing_table_t *table);

void update_distance_vector(routing_table_t *table);

void show_node(node_t *node);

void show_routing_table(routing_table_t *table);

#endif

