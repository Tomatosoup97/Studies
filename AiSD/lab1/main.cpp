#include <iostream>
#include <stdio.h>
#include <vector>
#include <stack>

#define TRUE 1
#define FALSE 0
#define ROOT 1

// Program flags
#define DEBUG 0
#define RECURSIVE 0

int q, n;
int *sizes;
int *nodes_nums;
int *visited;
int *parents;
int node_num_counter;

std::vector<int> *adjacency_lists;

void input_graph() {
    /* Input graph into memory */
    int parent_num = 0;
    adjacency_lists = new std::vector<int> [n+1];
    sizes = new int [n+1];
    nodes_nums = new int [n+1];
    visited = new int [n+1];
    parents = new int [n+1];

    for (int i=1; i<n; i++) {
        scanf("%d", &parent_num);
        adjacency_lists[parent_num].push_back(i+1);
    }
}

void DFS_recursive(int v) {
    /* Perform recursive depth first search on vertex v */
    sizes[v] = 1;
    nodes_nums[v] = node_num_counter++;

    // for each neighbour(v)
    std::vector<int>::iterator u = adjacency_lists[v].begin();
    for (; u != adjacency_lists[v].end(); ++u) {
        if (visited[*u]) continue;
        visited[*u] = TRUE;
        parents[*u] = v;
        DFS_recursive(*u);
        sizes[v] += sizes[*u];
    }
}

void DFS_iterative(int v) {
    /* Perform iterative depth first search on vertex v */
    std::stack<int> stack;
    stack.push(v);

    while (!stack.empty()) {
        v = stack.top();
        stack.pop();
        nodes_nums[v] = node_num_counter++;
        sizes[v] = 1;

        if (!visited[v]) {
            visited[v] = TRUE;

            // propagate sizes
            int node = v;
            while (node != ROOT) {
                sizes[parents[node]] += sizes[v];
                node = parents[node];
            }
        }

        std::vector<int>::iterator u = adjacency_lists[v].begin();
        for (; u != adjacency_lists[v].end(); ++u) {
            if (!visited[*u]) {
                parents[*u] = v;
                stack.push(*u);
            }
        }
    }
}

int is_ancestor(int u, int v) {
    /* Return true if u is ancestor of v */
    return nodes_nums[v] > nodes_nums[u] &&\
           nodes_nums[v] < nodes_nums[u] + sizes[u];
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
    output_array(sizes, "sizes");
    output_array(parents, "parents");
    output_array(nodes_nums, "nodes_nums");
    output_array(visited, "visited");
}

int main() {
    scanf("%d %d", &n, &q);
    input_graph();

    if (RECURSIVE)
        DFS_recursive(ROOT);
    else
        DFS_iterative(ROOT);

    if (DEBUG)
        output_tree_state();

    answer_queries();
    return 0;
}

