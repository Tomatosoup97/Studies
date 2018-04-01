#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include "table.h"

#define MAX_TABLE_SIZE 32
#define LISTENING_PORT 54321
#define PROPAGATE_INTERVAL 5

routing_table_t routing_table;

void read_data() {
    scanf("%d", &routing_table.nodes_count);

    for (int i=0; i<routing_table.nodes_count; i++) {
        node_t node;
        scanf("%s", node.ip_addr.ip);
        scanf("%*s %d", &node.distance);
        node.conn_type = DIRECT_CONN;
        routing_table.nodes[i] = node;
    }
}

int main() {
    init_routing_table(&routing_table, MAX_TABLE_SIZE);
    read_data();

    while (TRUE) {
        propagate_distance_vector(&routing_table);
        update_distance_vector(&routing_table);
        show_routing_table(&routing_table);
        printf("\n\n\n");
        sleep(PROPAGATE_INTERVAL);
    }

    printf("\nI am alive! %s\n", getenv("MACHINE_NAME"));
    return 0;
}

