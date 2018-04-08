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
    int i, available_nodes=0;
    FOR_EACH_AVAILABLE_NODE(table, i) {
        show_node(&table->nodes[i]);
        available_nodes++;
    }
    if (available_nodes == 0)
        printf("No valid connections, routing table is empty\n");
}

int find_fst_free_slot(routing_table_t *table) {
    for (int i=0; i<table->max_size; i++)
        if (table->nodes[i].conn_type == CONN_UNDEFINED)
            return i;
    return -1;
}

int find_node_by_network_addr(routing_table_t *table, ip_addr_t *addr) {
    int i;
    FOR_EACH_NODE(table, i)
        if (table->nodes[i].network_addr.s_addr == addr->s_addr)
            return i;
    return -1;
}

int find_node_by_router_addr(routing_table_t *table, ip_addr_t *addr) {
    int i;
    FOR_EACH_NODE(table, i)
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
    int i;
    ip_addr_t network_addr;

    FOR_EACH_NODE(table, i) {
        node_t node = table->nodes[i];
        if (node.conn_type != CONN_DIRECT) continue;
        network_addr = translate_to_network_addr(addr, node.subnet_mask_len);
        if (node.network_addr.s_addr == network_addr.s_addr &&
                longest_subnet < node.subnet_mask_len)
            best_match = i;
    }
    return best_match;
}

void calc_node_transitive_distance(routing_table_t *table, node_t *new_node) {
    int network_match_idx = match_ip_with_network(table, &new_node->router_addr);
    if (network_match_idx == -1) handle_error("router does not match any network");
    node_t *router_node = &table->nodes[network_match_idx];
    if (new_node->distance ==  UNREACHABLE || router_node->distance == UNREACHABLE)
        new_node->distance = UNREACHABLE;
    else
        new_node->distance += router_node->distance;
}

int is_node_connection_source(routing_table_t *table, node_t *old_node,
                                   node_t *new_node) {
    int sender_network_idx = match_ip_with_network(table, &new_node->router_addr);
    node_t *sender_network = &table->nodes[sender_network_idx];

    return (
        (old_node->conn_type == CONN_DIRECT &&
        sender_network->network_addr.s_addr == old_node->network_addr.s_addr)
        ||
        (old_node->conn_type == CONN_INDIRECT &&
        new_node->router_addr.s_addr == old_node->router_addr.s_addr)
    );
}

int is_new_node_superior(node_t *old_node, node_t *new_node) {
    return old_node->distance > new_node->distance &&\
           old_node->conn_type != CONN_DIRECT;
}

void process_update_from_source(routing_table_t *table, node_t *curr_node, node_t *new_node) {
    if (new_node->conn_type == CONN_DIRECT && curr_node->distance == UNREACHABLE)
        curr_node->distance = new_node->distance;

    if (new_node->distance >= INFINITE_DISTANCE ||
            curr_node->distance >= INFINITE_DISTANCE)
        set_node_unreachable(table, curr_node);
    else
        curr_node->reachability = MAX_REACHABILITY;
}

void update_node_in_table(routing_table_t *table, node_t *new_node) {
    int curr_node_idx = find_node_by_network_addr(table, &new_node->network_addr);

    if (new_node->conn_type == CONN_INDIRECT)
        calc_node_transitive_distance(table, new_node);

    if (curr_node_idx == -1 && new_node->distance != UNREACHABLE) {
        append_node_to_table(table, new_node);
        return;
    }

    node_t *curr_node = &table->nodes[curr_node_idx];

    if (is_new_node_superior(curr_node, new_node))
        table->nodes[curr_node_idx] = *new_node;

    if (is_node_connection_source(table, curr_node, new_node))
        process_update_from_source(table, curr_node, new_node);
}

int determine_conn_type(routing_table_t *table, node_t *node) {
    int curr_node_idx = find_node_by_network_addr(table, &node->network_addr);
    if (curr_node_idx == -1) return CONN_INDIRECT;
    return table->nodes[curr_node_idx].conn_type;
}

void set_node_unreachable(routing_table_t *table, node_t *node) {
    node->distance = UNREACHABLE;
    if (node->reachability > 0) node->reachability = 0;

    if (node->reachability <= MIN_REACHABILITY)
        remove_node(table, node);
}

void remove_node(routing_table_t *table, node_t *node) {
    if (node->conn_type == CONN_DIRECT)
        return;

    int node_idx = find_node_by_network_addr(table, &node->network_addr);
    node_t empty_node = {0};
    table->nodes[node_idx] = empty_node;

    if (node_idx == (table->nodes_count - 1))
        table->nodes_count--;
}

int is_node_proprietary(routing_table_t *table, node_t *node) {
    int node_idx = find_node_by_router_addr(table, &node->router_addr);
    return table->nodes[node_idx].conn_type == CONN_DIRECT && node_idx != -1;
}

