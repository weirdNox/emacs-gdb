// cc -g3 multithread.c -o multithread.out -lpthread

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void *threadFunction(void *vargp)
{
    printf("Thread\n");
    return 0;
}

int main()
{
    pthread_t thread_id;
    printf("Before Thread\n");
    pthread_create(&thread_id, 0, threadFunction, 0);
    pthread_join(thread_id, 0);
    printf("After Thread\n");
    exit(0);
}
