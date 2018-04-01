#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "table.h"

void init_routing_table(routing_table_t *table, int size) {
    table->size = size;
    table->nodes = malloc(size * sizeof(node_t));
}

void propagate_distance_vector(routing_table_t *table) {

}

void update_distance_vector(routing_table_t *table) {

}

void show_routing_table(routing_table_t *table) {
    for (int i=0; i<table->nodes_count; i++)
        show_node(&table->nodes[i]);
}

void show_node(node_t *node) {
    assert((node->conn_type == DIRECT_CONN) == (node->through.ip[0] == '\0'));
    char connection_msg[64];
    char distance_msg[64];
    sprintf(connection_msg,
            node->conn_type == DIRECT_CONN ? "connected directly" : "via %s",
            node->through.ip);
    sprintf(distance_msg,
            node->is_reachable ? "distance %d" : "unreachable",
            node->distance);
    printf("%s %s %s\n",
            node->ip_addr.ip,
            distance_msg,
            connection_msg);
}

