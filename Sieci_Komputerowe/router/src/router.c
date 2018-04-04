#include <netinet/ip.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include "common.h"
#include "udp.h"
#include "table.h"

#define MAX_TABLE_SIZE 32
#define PROPAGATE_INTERVAL 5
#define DIST_VECTOR_EL_SIZE 9
#define MAX_PACKET_WAIT_MS 150

void propagate_distance_vector(int sockfd, routing_table_t *table) {
    uint8_t buffer[DIST_VECTOR_EL_SIZE];
    ip_addr_t broadcast_addr;

    for (int i=0; i<table->nodes_count; i++) {
        if (table->nodes[i].conn_type != CONN_DIRECT) continue;
        node_t direct_node = table->nodes[i];
        uint32_t network_addr_host = ntohl(direct_node.network_addr.s_addr);
        broadcast_addr.s_addr = htonl(calc_broadcast_addr(network_addr_host,
                                                          direct_node.subnet_mask_len));

        for (int j=0; j<table->nodes_count; j++) {
            node_t node = table->nodes[j];
            encode_udp_payload(&node, buffer);
            send_udp_packet(sockfd, broadcast_addr, buffer, DIST_VECTOR_EL_SIZE);
        }
    }
}

void update_distance_vector(int sockfd, routing_table_t *table) {
    node_t new_node;
    int is_socket_ready;

    while ((is_socket_ready = select_socket(sockfd, 0, MAX_PACKET_WAIT_MS))) {
        if (is_socket_ready == -1) handle_error("select");
        read_node_from_socket(sockfd, &new_node);
        update_node_in_table(table, &new_node);
    }
}

void read_direct_nodes(routing_table_t *table) {
    scanf("%d", &table->nodes_count);

    for (int i=0; i<table->nodes_count; i++) {
        node_t node;
        read_node(&node);
        table->nodes[i] = node;
    }
}

int main() {
    routing_table_t routing_table;
    init_routing_table(&routing_table, MAX_TABLE_SIZE);
    read_direct_nodes(&routing_table);

    int bcast_sockfd = create_bcast_socket();
    int receive_sockfd = bind_socket();

    while (TRUE) {
        show_routing_table(&routing_table);
        propagate_distance_vector(bcast_sockfd, &routing_table);
        update_distance_vector(receive_sockfd, &routing_table);
        printf("\n\n");
        sleep(PROPAGATE_INTERVAL);
    }

    close(bcast_sockfd);
    close(receive_sockfd);

    return EXIT_SUCCESS;
}

