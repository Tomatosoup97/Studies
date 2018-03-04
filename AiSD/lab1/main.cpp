#include <stdio.h>
#include <stdint.h>
#include <stack>

#define TRUE 1
#define FALSE 0
#define ROOT 1

// Program flags
#define DEBUG 0

int q, n;
int *time_out;
int *time_in;
int node_num_counter=0;

int first_child[1000000+1];
int neighbours[1000000+1];

void input_graph() {
    /* Input graph into memory */
    int parent_num = 0;

    time_out = new int [n+1];
    time_in = new int [n+1];

    time_in[1] = -1;

    for (int i=2; i<=n; i++) {
        scanf("%d", &parent_num);

        time_in[i] = -1;
        time_out[parent_num]++;  // temp. store children count in time_out

        neighbours[i] = first_child[parent_num];
        first_child[parent_num] = i;
    }

    if (DEBUG) printf("DONE! graph stored in memory\n");
}

void DFS_iterative(int v) {
    /* Perform iterative depth first search on vertex v */
    std::stack<int> stack;
    stack.push(v);
    time_in[v] = node_num_counter++;

    while (!stack.empty()) {
        v = stack.top();

        if (!time_out[v]) {
            stack.pop();
            time_out[v] = node_num_counter++;  // reflect actual time_out
            continue;
        }

        int u = first_child[v];
        while (u != 0) {
            if (time_in[u] == -1) {
                time_in[u] = node_num_counter++;
                stack.push(u);
                time_out[v]--;  // time_out temporarily as children counter
                break;
            }
            u = neighbours[u];
        }
    }
}

int is_ancestor(int u, int v) {
    /* Return true if u is ancestor of v */
    return time_in[v] > time_in[u] &&\
           time_in[v] < time_out[u];
}

void output_verbose_bool(int bool_val) {
    if (bool_val) printf("TAK\n");
    else printf("NIE\n");
}

void answer_queries() {
    int query_a, query_b, result;

    for (int i=0; i<q; i++) {
        scanf("%d %d", &query_a, &query_b);
        result = is_ancestor(query_a, query_b);
        output_verbose_bool(result);
    }
}

void output_array(int *ptr, const char *label) {
    printf("%s: ", label);
    for (int i=1; i<=n; i++)
        printf("%d ", ptr[i]);
    printf("\n");
}

void output_tree_state() {
    output_array(time_out, "time_out");
    output_array(time_in, "time_in");
}

int main() {
    scanf("%d %d", &n, &q);
    input_graph();

    DFS_iterative(ROOT);

    if (DEBUG) printf("DONE! DFS tree created\n");
    if (DEBUG) output_tree_state();

    answer_queries();
    return 0;
}
