#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

typedef struct test {
    int Number;
    union {
        char Letter;
        long AnotherNumber;
    };
} test;

void *threadFunction(void *Vargp) {
    printf("Thread\n");
    sleep(3);
    return 0;
}

int main(int ArgCount, char *ArgVal[]) {
    // NOTE(nox): Thread test
    pthread_t ThreadId;
    printf("Before Thread\n");
    pthread_create(&ThreadId, 0, threadFunction, 0);
    pthread_join(ThreadId, 0);
    printf("After Thread\n");

    // NOTE(nox): Variable tests
    char SingleLetter = 'a';
    SingleLetter = 'z';

    test TestVariable = {12, .Letter = 'a'};
    TestVariable.Number = 3;
    TestVariable.AnotherNumber = 1891898;

    test *Pointer = 0;
    Pointer = &TestVariable;

    return 0;
}