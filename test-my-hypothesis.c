/**
 * @file test-my-hypothesis.c
 * @author Brett Schreiber (schreiberbrett@gmail.com)
 * @brief Are all graphs G, 3-verifiable in V^2 proof steps?
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// An adjacency matrix graph
struct Graph {
    int number_of_vertices;
    bool *has_edge;
};

struct Graph new_Graph(int number_of_vertices) {
    struct Graph graph;
    graph.number_of_vertices = number_of_vertices;
    graph.has_edge = calloc(sizeof(bool), number_of_vertices * number_of_vertices);
}

void add_edge(struct Graph *g, int u, int v) {
    g->has_edge[u * g->number_of_vertices + v] = true;
    g->has_edge[v * g->number_of_vertices + u] = true;
}

bool has_edge(struct Graph g, int u, int v) {
    return g.has_edge[u * g.number_of_vertices + v];
}

// This is getting complicated. Maybe it would be easier to make a prototype in, say, Haskell. (Although that means there will be less opportunity for parallelism later).

int main() {
    printf("Hello, world!\n");
    return 0;
}