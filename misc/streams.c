#include<stdlib.h>
#include<stdio.h>

struct Stream {
    enum { EMPTY, CONS, DELAY } kind;
    union {
        struct {
            int first;
            struct Stream *rest;
        };
        struct {
            struct Stream* (*pull)(void*);
            void *data;
        };
    };
};

int* heap_int(int value) {
    int *x = malloc(sizeof(int));
    if (x == NULL) {
        return NULL;
    }

    *x = value;
    return x;
}

struct Stream *heap_stream(struct Stream value) {
    struct Stream *x = malloc(sizeof(struct Stream));
    if (x == NULL) {
        return NULL;
    }
    
    *x = value;
    return x;
}

struct Stream *empty() {
    return heap_stream((struct Stream){
        .kind = EMPTY
    });
}

struct Stream *cons(int n, struct Stream *rest) {
    return heap_stream((struct Stream){
        .kind = CONS,
        .first = n,
        .rest = rest
    });
}

struct Stream *delay(struct Stream* (*pull)(void*), void *data) {
    return heap_stream((struct Stream){
        .kind = DELAY,
        .pull = pull,
        .data = data
    });
}

struct Stream *pull(struct Stream *stream) {
    struct Stream *result = stream->pull(stream->data);
    free(stream->data);
    return result;
}

struct Stream *count_up(void *data) {
    int n = ((int*)data)[0];
    int *new_data = malloc(sizeof(int));
    new_data[0] = n + 1;

    return cons(
        n,
        delay(
            count_up,
            new_data
        )
    );
}

struct Stream *helper(void*);

struct Stream *cube_stream(struct Stream *stream) {
    switch (stream->kind) {
        case EMPTY:
            return empty();
        case CONS:
            return cons(
                stream->first * stream->first * stream->first,
                cube_stream(stream->rest)
            );
        case DELAY:
            return delay(
                helper,
                heap_stream(*stream)
            );
                
    }
}

struct Stream *helper(void *data) {
    return cube_stream(pull((struct Stream *)data));
}

void print_n(int n, struct Stream *stream) {
    if (n == 0) {
        return;
    }

    switch (stream->kind) {
        case EMPTY:
            return;
        case CONS:
            printf("%d\n", stream->first);
            print_n(n - 1, stream->rest);
            return;
        case DELAY:
            print_n(n, pull(stream));
            return;
    }
}

int main(void) {
    print_n(10, cube_stream(count_up(heap_int(1))));
    return 0;
}
