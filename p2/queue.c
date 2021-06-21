#include <stdlib.h>
#include <semaphore.h>
#include <pthread.h>
#include <stdio.h>
#include "queue.h"
#define THREAD_ONLY 0

// circular array
typedef struct _queue {
    int size;
    int used;
    int first;
    void **data;
	// conditional variables
	pthread_cond_t* q_full; 
	pthread_cond_t* q_empty;
	pthread_mutex_t *q_mutex; // this mutex will block the writing or reading from the queue
} _queue;



queue q_create(int size) {
    queue q = malloc(sizeof(_queue));
    
    q->size  = size;
    q->used  = 0;
    q->first = 0;
    q->data  = malloc(size*sizeof(void *));
	if((q->q_full = malloc(sizeof(pthread_cond_t))) == NULL) {printf("Couldn't locate memory\n"); exit(EXIT_FAILURE);}
	pthread_cond_init(q->q_full, NULL);
	if((q->q_empty = malloc(sizeof(pthread_cond_t))) == NULL) {printf("Couldn't locate memory\n"); exit(EXIT_FAILURE);}
	pthread_cond_init(q->q_empty, NULL);
	if((q->q_mutex = malloc(sizeof(pthread_mutex_t))) == NULL) {printf("Couldn't locate memory for mutex\n"); exit(EXIT_FAILURE);}
	pthread_mutex_init(q->q_mutex, NULL); 
    return q;
}

int q_elements(queue q) {
	int used;
	used = q->used;
    return used;
}

int q_insert(queue q, void *elem) {
	// theoretically, it shoud never enter when the queue is full

	pthread_mutex_lock(q->q_mutex);
	while (q->used == q->size) // the thread wait for espace to insert if needed
		pthread_cond_wait(q->q_full, q->q_mutex);
   	q->data[(q->first+q->used) % q->size] = elem;    
   	q->used++;
	if (q->used == 1) pthread_cond_broadcast(q->q_empty); // if the queue isn't empty, it awakes the consumers
   	pthread_mutex_unlock(q->q_mutex);
    return 1;
}

void *q_remove(queue q) {
    void *res;
	// theoretically, it shoud never enter when the queue is empty

	pthread_mutex_lock(q->q_mutex); // locks the queue modification
	while (q->used == 0)
		pthread_cond_wait(q->q_empty, q->q_mutex);

    res = q->data[q->first];
    q->first = (q->first+1) % q->size;
    q->used--;
	if (q->used == (q->size - 1)) pthread_cond_broadcast(q->q_full);
	pthread_mutex_unlock(q->q_mutex); // unlocks the queue modification
    
    return res;
}

void q_destroy(queue q) {
    free(q->data);
	pthread_cond_destroy(q->q_full);
	free(q->q_full);
	pthread_cond_destroy(q->q_empty);
	free(q->q_empty);
	pthread_mutex_destroy(q->q_mutex);
	free(q->q_mutex);
    free(q);
}
