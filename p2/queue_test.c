#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include "queue.h"

struct _args {
	queue q;
	int value;
} _args;

void * insert (void * insert_arguments) {
	struct _args * args = (struct _args *) insert_arguments;
	q_insert(args->q,(void*) &args->value);
	printf("Inserted --------------> %d in the queue\n", args->value);
	return NULL;
}

void * th_remove (void* remove_arguments) {
	queue * q = (queue *) remove_arguments;
	int * res = (int *) q_remove(*q);
	printf("Removed %d from the queue\n", *res);
	return NULL;
}


int main(int argc, char* argv[]) {
	int q_length;
	int num_threads;
	if (argc != 3) {printf("ERROR: wrong number of parameters ($ ./qtest <queue_length> <threads>). Exiting...\n"); return 1;}
	q_length = atoi(argv[1]);
	num_threads = atoi(argv[2]);
	queue Q = q_create(q_length);
	if (Q == NULL) {printf("ERROR: couldn't create queue. Exiting...\n'"); return 1;}
	
	struct _args data_array[num_threads];
	pthread_t insert_thread_array[num_threads];
	pthread_t remove_thread_array[num_threads];
	for (int i = 0; i < num_threads; i++) {
		data_array[i].q = Q;
		data_array[i].value = i;
		pthread_create(&insert_thread_array[i], NULL, insert, &data_array[i]);
	}
	
	for (int j = 0; j < num_threads; j++) { 
		pthread_create(&remove_thread_array[j], NULL, th_remove, (void*) &Q);
	}
	
	for (int i = 0; i < num_threads; i++) pthread_join(insert_thread_array[i], NULL);
	for (int j = 0; j < num_threads; j++) pthread_join(remove_thread_array[j], NULL);

	q_destroy(Q);
	return 0;
}
