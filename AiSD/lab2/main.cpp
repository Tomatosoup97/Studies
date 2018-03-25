#include <stdio.h>
#include <queue>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

#define MOD_NUM 999979
#define DEBUG 0

int n, m, t;

typedef std::pair<int, int> coords;

typedef std::pair<
    coords,
    coords
> math_vector;

typedef math_vector tunnel_t;

struct vertex_meta_t {
    int tunnels_index;
    int paths = 0;
};

std::queue<coords> BFSQ;

std::vector<tunnel_t> tunnels;

std::map<
    coords,
    vertex_meta_t
> places;

void input_tunnels() {
    scanf("%d %d %d", &m, &n, &t);

    for (int i=0; i<t; i++) {
        coords from_place, to_place;
        tunnel_t tunnel;

        scanf("%d %d", &from_place.first, &from_place.second);
        scanf("%d %d", &to_place.first, &to_place.second);
        // Store tunnels reversed
        tunnel = std::make_pair(to_place, from_place);
        tunnels.push_back(tunnel);
    }
}

void sort_tunnels() {
    std::sort(tunnels.begin(), tunnels.end());
}

vertex_meta_t get_or_create_meta(coords v) {
    vertex_meta_t vertex_meta = {-1, 0};
    try {
        places.at(v);
    } catch (...) {
        places[v] = vertex_meta;
    }
    return places[v];
}

void create_places_map() {
    coords prev_place = std::make_pair(-1, -1);
    int index = 0;

    for (auto const& tunnel: tunnels) {
        coords from_place = tunnel.first;
        coords to_place = tunnel.second;

        get_or_create_meta(from_place);
        get_or_create_meta(to_place);

        if (from_place != prev_place)
            places[from_place].tunnels_index = index;
        prev_place = from_place;
        index++;
    }
}

void output_places_map() {
    printf("Map of places:\n");

    for (auto const& place: places)
        printf("<%d, %d>: index: %d, paths: %d\n",
               place.first.first, place.first.second,
               place.second.tunnels_index,
               place.second.paths);
}

void dynamic_calc_paths() {
    for (auto const& place : places) {
        coords parent;
        coords v = place.first;
        vertex_meta_t v_meta = place.second;
        int tunnels_index = v_meta.tunnels_index;

        while (tunnels[tunnels_index].first == v) {
            parent = tunnels[tunnels_index].second;

            places[v].paths += places[parent].paths % MOD_NUM;
            places[v].paths %= MOD_NUM;

            tunnels_index++;
        }
    }

}

int main() {
    input_tunnels();
    sort_tunnels();

    create_places_map();
    if (DEBUG) output_places_map();

    coords S = std::make_pair(0, 0);
    places[S].paths = 1;
    dynamic_calc_paths();

    if (DEBUG) output_places_map();

    coords F = std::make_pair(m, n);
    vertex_meta_t F_meta = get_or_create_meta(F);
    printf("%d\n", F_meta.paths);

    if (DEBUG) printf("\n\n");
    return 0;
}

