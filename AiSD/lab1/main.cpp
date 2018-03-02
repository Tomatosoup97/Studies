#include <stdio.h>
#include <stdint.h>
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
int8_t *visited;
int *parents;
int node_num_counter;

int **adjacency_lists;

void quicksort_graph(int l, int p) {
    /* Quicksort graph list based on parent value */
    if (p <= l) return;
    int i = l, j = p;
    int s = (i + j) / 2;
    int pivot = adjacency_lists[s][0];

    while (TRUE) {
        while (pivot > adjacency_lists[i++][0]);
        while (pivot < adjacency_lists[j--][0]);

        if (i <= j) {
            std::swap(adjacency_lists[i][0], adjacency_lists[j][0]);
            std::swap(adjacency_lists[i][1], adjacency_lists[j][1]);
        } else {
            break;
        }
    }

    if (j > l) quicksort_graph(l, j);
    if (i < p) quicksort_graph(i, p);
}

int binsearch_graph(int l, int p, int parent) {
    /* binary search for first occurence of given parent in graph */
    int s;

    while (l <= p) {
        s = (l + p) / 2;
        if (adjacency_lists[s][0] == parent) {
            if (s == 1) return s;
            while (adjacency_lists[--s][0] == parent);
            return ++s;
        }

        if (adjacency_lists[s][0] > parent)
            p = s - 1;
        else
            l = s + 1;
    }
    return -1;
}

void input_graph() {
    /* Input graph into memory */
    int parent_num = 0;

    adjacency_lists = new int* [n+1];
    for (int i=0; i<=n; i++)
        adjacency_lists[i] = new int [2];


    sizes = new int [n+1];
    nodes_nums = new int [n+1];
    visited = new int8_t [n+1];
    parents = new int [n+1];

    for (int i=1; i<n; i++) {
        scanf("%d", &parent_num);
        adjacency_lists[i][0] = parent_num;
        adjacency_lists[i][1] = i+1;
    }

    quicksort_graph(1, n-1);
}
/*
void DFS_recursive(int v) {
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
*/
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

        int child = binsearch_graph(1, n-1, v);
        if (child == -1) continue;  // node is leaf
        while (adjacency_lists[child][0] == v) {
            int u = adjacency_lists[child++][1];
            if (!visited[u]) {
                parents[u] = v;
                stack.push(u);
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
    /* output_array(visited, "visited"); */
}

int main() {
    scanf("%d %d", &n, &q);
    input_graph();

    if (RECURSIVE)
        /* DFS_recursive(ROOT); */
        return 1;
    else
        DFS_iterative(ROOT);

    if (DEBUG)
        output_tree_state();

    answer_queries();
    return 0;
}

