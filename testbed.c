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
    int Id = *(int*)Vargp;
    printf("Thread %d\n", Id);
    test TestVariable = {2, 3};
    TestVariable.Number = 10;
    sleep(2);
    return 0;
}

void functionToCall(char *Arg1, int Arg2) {
    if(Arg1) {
        printf("String: %s\nInteger: %d\n", Arg1, Arg2);
    }
}

int factorial(int N) {
    if(N > 1) {
        return N*factorial(N-1);
    } else {
        return 1;
    }
}

int main(int ArgCount, char *ArgVal[]) {
    // NOTE(nox): Arguments test
    if(ArgCount > 1) {
        printf("%d arguments were passed to this program.\n", ArgCount-1);
        for(int I = 1; I < ArgCount; ++I) {
            printf("Argument %d: %s\n", I, ArgVal[I]);
        }
    }

    // NOTE(nox): Threads test
    pthread_t ThreadId;

    pthread_create(&ThreadId, 0, threadFunction, &(int){0});
    pthread_join(ThreadId, 0);

    pthread_create(&ThreadId, 0, threadFunction, &(int){1});
    pthread_join(ThreadId, 0);

    // NOTE(nox): Variable tests
    char SingleLetter = 'a';
    SingleLetter = 'z';

    test TestVariable = {12, .Letter = 'a'};
    TestVariable.Number = 3;
    TestVariable.AnotherNumber = 1891898;

    test *Pointer = 0;
    Pointer = &TestVariable;

    // NOTE(nox): Loop tests
    for(int TestIndex = 0; TestIndex < 10; ++TestIndex) {
        printf("Index: %d\n", TestIndex);
    }

    // NOTE(nox): Function call test
    functionToCall("Hello world", 4);

    // NOTE(nox): Recursive function tests
    printf("4! = %d\n", factorial(4));
    printf("5! = %d\n", factorial(5));

    return 0;
}
