#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "common.h"
#include "table.h"


void init_routing_table(routing_table_t *table, int max_size) {
    table->max_size = max_size;
    table->nodes = malloc(max_size * sizeof(node_t));
}

void show_routing_table(routing_table_t *table) {
    for (int i=0; i<table->nodes_count; i++)
        show_node(&table->nodes[i]);
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

int find_node_by_router_addr(routing_table_t *table, ip_addr_t *addr) {
    for (int i=0; i<table->max_size; i++)
        if (table->nodes[i].router_addr.s_addr == addr->s_addr)
            return i;
    return -1;
}

void append_node_to_table(routing_table_t *table, node_t *node) {
    int free_slot = find_fst_free_slot(table);
    if (free_slot == -1) handle_error("no free slots in table");
    table->nodes[free_slot] = *node;
    table->nodes_count++;
}

int match_ip_with_network(routing_table_t *table, ip_addr_t *addr) {
    int longest_subnet = -1;
    int best_match = -1;
    ip_addr_t network_addr;

    for (int i=0; i<table->nodes_count; i++) {
        node_t node = table->nodes[i];
        if (node.conn_type != CONN_DIRECT) continue;
        network_addr = translate_to_network_addr(addr, node.subnet_mask_len);
        if (node.network_addr.s_addr == network_addr.s_addr &&
                longest_subnet < node.subnet_mask_len)
            best_match = i;
    }
    return best_match;
}

void calc_node_actual_distance(routing_table_t *table, node_t *new_node) {
    int network_match_idx = match_ip_with_network(table, &new_node->router_addr);
    if (network_match_idx == -1) handle_error("router does not match any network");
    node_t router_node = table->nodes[network_match_idx];
    new_node->distance += router_node.distance;
}

void update_node_in_table(routing_table_t *table, node_t *new_node) {
    int curr_node_idx = find_node_by_network_addr(table, &new_node->network_addr);

    if (new_node->conn_type == CONN_INDIRECT)
        calc_node_actual_distance(table, new_node);

    if (curr_node_idx == -1) {
        append_node_to_table(table, new_node);
    } else if (table->nodes[curr_node_idx].distance > new_node->distance &&
               table->nodes[curr_node_idx].conn_type != CONN_DIRECT) {
        table->nodes[curr_node_idx] = *new_node;
    }
}

int determine_conn_type(routing_table_t *table, node_t *node) {
    int curr_node_idx = find_node_by_network_addr(table, &node->network_addr);
    if (curr_node_idx == -1) return CONN_INDIRECT;
    return table->nodes[curr_node_idx].conn_type;
}

