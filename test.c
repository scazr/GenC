generic T {
    typedef struct List {
        T val;
        int quantity;
        T* next;
    } List;
    
    T* get_next(T* list) {
        return list->next;
    }

    typedef struct List {
        T val;
        int quantity;
        T* next;
    } List;
}

int test() {}

generic A, B {
    typedef struct Tuple {
        A val1;
        B val2;
    } Tuple;
}

typedef struct NormalNode {
    int val;
    int* next;
} NormalNode;

int main() {
    List<int> numbers = List<int> {
        .val = 1,
        .quantity = 1,
        .next = &List<int> {
            .val = 3,
            .quantity = 2,
            .next = NULL
        }
    };

    get_next<List<int>>(&numbers);
    test2<int>();
    return 0;
}

