#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <deque>
#include <stdbool.h>
#include <limits.h>

#define DEBUG 0
#define MAX_N (1000000+2) // 1e6 + A, B stations

#define IS_COST_LTE_LAST(i, Queue)\
    (stations[(i)].cost <= stations[Queue.back()].cost)

typedef struct {
    long long cost;
    int distance;
} station_t;

int n, l, b;

station_t stations[MAX_N];

void read_data() {
    scanf("%d %d %d", &n, &l, &b);

    stations[0].cost = 0;
    stations[0].distance = 0;

    for (int i=1; i<n+1; i++)
        scanf("%d %lld", &stations[i].distance, &stations[i].cost);

    stations[n+1].cost = -1;
    stations[n+1].distance = l;
}

int window_last_el(int starting_dist, int starting_idx) {
    int k=starting_idx;
    while (k < n+2 && (stations[k].distance - starting_dist) <= b)
        k++;
    return --k;
}

int window_fst_el(int ending_dist, int starting_idx) {
    int k=starting_idx;
    while ((ending_dist - stations[k].distance) > b)
        k++;
    return k;
}

void show_station(int idx) {
    station_t *s = &stations[idx];
    if (DEBUG) printf("station %d: d=%d, c=%lld\n", idx, s->distance, s->cost);
}

int main() {
    read_data();
    std::deque<int> Queue(MAX_N);

    int fst_el = 0;
    int last_el = window_last_el(0, 0);
    int i = 0;
    int unreachable = false;

    if (b >= l) {
        printf("0\n");
        exit(EXIT_SUCCESS);
    }

    // process first window
    for ( ; i<last_el+1; i++) {
        while ((!Queue.empty()) && IS_COST_LTE_LAST(i, Queue))
            Queue.pop_back();

        Queue.push_back(i);
    }
    if (DEBUG) printf("\n");

    for ( ; i<n+2; i++) {
        fst_el = window_fst_el(stations[i].distance, fst_el);
        if (DEBUG) printf("window: [%d, %d]\n", fst_el, i);

        if (fst_el == i) {
            unreachable = true;
            break;
        }

        // discard elements out of this window
        while ((!Queue.empty()) && Queue.front() < fst_el)
            Queue.pop_front();

        // get min station in current window
        station_t *min_station = &stations[Queue.front()];
        stations[i].cost += min_station->cost;
        if (DEBUG) show_station(i);
        if (DEBUG) show_station(Queue.front());

        // discard greater elements
        while ((!Queue.empty()) && IS_COST_LTE_LAST(i, Queue))
            Queue.pop_back();

        Queue.push_back(i);
        if (DEBUG) printf("\n");
    }

    // discard stations which can't reach final destination
    while ((!Queue.empty()) && (l - stations[Queue.front()].distance) > b)
        Queue.pop_front();

    stations[Queue.front()].cost++; // intial B cost equals -1
    if (Queue.empty() || unreachable) {
        printf("NIE\n");
        exit(EXIT_SUCCESS);
    }
    station_t *last_s = &stations[Queue.front()];
    printf(last_s->cost == -1 ? "NIE" : "%lld\n", last_s->cost);

    return 0;
}

