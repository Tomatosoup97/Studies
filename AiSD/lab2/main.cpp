#include <stdio.h>
#include <queue>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

#define MOD_NUM 999979
#define DEBUG 1

int n, m, t;

typedef std::pair<int, int> coords;

typedef std::pair<
    coords,
    coords
> math_vector;

typedef math_vector tunnel_t;

struct vertex_meta_t {
    int tunnels_index;
    int paths;
    int parents_count;
    bool was_visited;
};

std::queue<coords> BFSQ;

std::vector<tunnel_t> tunnels;

std::map<
    coords,
    vertex_meta_t
> places;

void output_coords(const coords *c) {
    printf("%d %d", c->first, c->second);
}

void output_tunnels() {
    printf("Tunnels:\n");
    for (auto const& tunnel: tunnels) {
        output_coords(&tunnel.first);
        printf("  ");
        output_coords(&tunnel.second);
        printf("\n");
    }
}

void input_tunnels() {
    scanf("%d %d %d", &n, &m, &t);

    for (int i=0; i<t; i++) {
        coords from_place, to_place;
        tunnel_t tunnel;

        scanf("%d %d", &from_place.first, &from_place.second);
        scanf("%d %d", &to_place.first, &to_place.second);
        tunnel = std::make_pair(from_place, to_place);
        tunnels.push_back(tunnel);
    }
}

void sort_tunnels() {
    std::sort(tunnels.begin(), tunnels.end());
}

void create_places_map() {
    coords prev_place = std::make_pair(-1, -1);
    int index = 0;

    for (auto const& tunnel: tunnels) {
        coords from_place = tunnel.first;
        coords to_place = tunnel.second;

        vertex_meta_t vertex_meta = {-1, 0, 0, false};

        try {
            places.at(from_place);
        } catch (...) {
            places[from_place] = vertex_meta;
        }

        try {
            places.at(to_place);
        } catch (...) {
            places[to_place] = vertex_meta;
        }

        places[to_place].parents_count++;

        if (from_place != prev_place) {
            places[from_place].tunnels_index = index;
        }
        prev_place = from_place;
        index++;
    }
}

void output_places_map() {
    printf("Map of places:\n");

    for (auto const& place: places)
        printf("<%d, %d>: index: %d, paths: %d, parents: %d, was_visited: %d\n",
               place.first.first, place.first.second,
               place.second.tunnels_index,
               place.second.paths,
               place.second.parents_count,
               place.second.was_visited);
}

bool was_coords_visited(coords v) {
    vertex_meta_t v_meta = places[v];
    return v_meta.was_visited;
}

void mark_coords_visited(coords v) {
    places[v].was_visited = true;
}

void bfs_count_paths() {
    coords v;

    while (BFSQ.empty() == false) {
        coords child, from_tunnel;

        v = BFSQ.front();
        BFSQ.pop();

        vertex_meta_t v_meta = places[v];
        int tunnels_index = v_meta.tunnels_index;

        while (tunnels[tunnels_index].first == v) {
            child = tunnels[tunnels_index].second;

            if (places[child].parents_count > 0) {
                BFSQ.push(child);

                if (places[v].parents_count == 0) {
                    places[child].paths += places[v].paths % MOD_NUM;
                    places[child].paths %= MOD_NUM;
                    places[child].parents_count--;
                }
            }

            tunnels_index++;
        }
    }
}

int main() {

    input_tunnels();
    if (DEBUG) output_tunnels();

    sort_tunnels();
    if (DEBUG) output_tunnels();

    create_places_map();
    if (DEBUG) output_places_map();

    coords S = std::make_pair(0, 0);
    BFSQ.push(S);
    mark_coords_visited(S);
    places[S].paths = 1;
    bfs_count_paths();
    if (DEBUG) output_places_map();

    printf("%d\n", places.rbegin()->second.paths);
    if (DEBUG) printf("\n\n");
    return 0;
}

