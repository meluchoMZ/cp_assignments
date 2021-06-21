#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "options.h"

struct buffer {
	int *data;
	int size;
};

struct thread_info {
	pthread_t       thread_id;        // id returned by pthread_create()
	int             thread_num;       // application defined thread #
};

struct args {
	int 		thread_num;       // application defined thread #
	int 	        delay;			  // delay between operations
	int*		iterations;
	struct buffer   *buffer;		  // Shared buffer
	pthread_mutex_t **mutex;		// engadese o campo para que os threads compartan o mutex array
	pthread_mutex_t *mutex_iter; // engades o campo par que os threads compartan o mutex
};

void *swap(void *ptr)
{

	struct args *args =  ptr;
	while (args->iterations > 0) {
		pthread_mutex_lock(args->mutex_iter);
		if (*args->iterations <= 0) {pthread_mutex_unlock(args->mutex_iter);return NULL;}
		else {*args->iterations = *args->iterations - 1;}
		pthread_mutex_unlock(args->mutex_iter);
			int a,b,i,j, tmp;

			do {
				a=rand() % args->buffer->size;
				b=rand() % args->buffer->size;
			} while (a == b);
			if (a <= b) {i=a; j=b;} else {i=b;j=a;}
			/* bloquease o acceso a seccion critica */
			while (1) {
				pthread_mutex_lock(args->mutex[i]);
				if (pthread_mutex_trylock(args->mutex[j]) == 0) {break;}
				pthread_mutex_unlock(args->mutex[i]);
			}

			printf("Thread %d swapping positions %d (== %d) and %d (== %d)\n",
				args->thread_num, i, args->buffer->data[i], j, args->buffer->data[j]);

			tmp = args->buffer->data[i];
			if(args->delay) usleep(args->delay); // Force a context switch

			args->buffer->data[i] = args->buffer->data[j];
			if(args->delay) usleep(args->delay);

			args->buffer->data[j] = tmp;
			if(args->delay) usleep(args->delay);
			/* desbloquease o acceso a seccion critica */
			pthread_mutex_unlock(args->mutex[i]);
			pthread_mutex_unlock(args->mutex[j]);
	}
	return NULL;
}

void print_buffer(struct buffer buffer) {
	int i;

	for (i = 0; i < buffer.size; i++)
		printf("%i ", buffer.data[i]);
	printf("\n");
}

void start_threads(struct options opt)
{
	int i;
	struct thread_info *threads;
	struct args *args;
	struct buffer buffer;
	// definese unha variable (no heap) a cal ten o número restante de iteracións
	int* iter = malloc(sizeof(int));
	if (iter == NULL) {printf("ERROR: Not able to allocate memory\n"); return;}
	*iter = opt.iterations;
	/* reservase memoria para o mutex das iteraccions */
	pthread_mutex_t* mutex_iteracions = malloc(sizeof(pthread_mutex_t));
	if (mutex_iteracions == NULL) {printf("ERROR: Cannot allocate mutex memory\n"); return;}
	if (pthread_mutex_init(mutex_iteracions, NULL) == -1) {printf("ERROR: Cannot start mutex\n"); free(mutex_iteracions); return;}
	/* reservase memoria para os mutex, seran tantos como elementos teña o vector */
	pthread_mutex_t* mutex_array[opt.buffer_size];
	for (int j = 0; j < opt.buffer_size; j++) {
		mutex_array[j] = malloc(sizeof(pthread_mutex_t));
		/* comprobase se se puido reservar memoria */
		if (mutex_array[j] == NULL) {printf("ERROR: Cannot allocate mutex memory\n"); return;}
		/* un por un, comprobanse se se poden inicializar */
		if (pthread_mutex_init(mutex_array[j], NULL) == -1) {printf("ERROR: Cannot start mutex\n"); return;}
	}

	srand(time(NULL));

	if((buffer.data=malloc(opt.buffer_size*sizeof(int)))==NULL) {
		printf("Out of memory\n");
		exit(1);
	}
	buffer.size = opt.buffer_size;

	for(i=0; i<buffer.size; i++)
		buffer.data[i]=i;

	printf("creating %d threads\n", opt.num_threads);
	threads = malloc(sizeof(struct thread_info) * opt.num_threads);
	args = malloc(sizeof(struct args) * opt.num_threads);

	if (threads == NULL || args==NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	printf("Buffer before: ");
	print_buffer(buffer);


	// Create num_thread threads running swap()
	for (i = 0; i < opt.num_threads; i++) {
		threads[i].thread_num = i;

		args[i].thread_num = i;
		args[i].buffer     = &buffer;
		args[i].delay      = opt.delay;
		args[i].iterations = iter;
		/* asignaselle o mesmo mutex a todos os threads*/
		args[i].mutex = mutex_array;
		args[i].mutex_iter = mutex_iteracions;

		if ( 0 != pthread_create(&threads[i].thread_id, NULL,
					 swap, &args[i])) {
			printf("Could not create thread #%d", i);
			exit(1);
		}
	}

	// Wait for the threads to finish
	for (i = 0; i < opt.num_threads; i++)
		pthread_join(threads[i].thread_id, NULL);

	// Print the buffer
	printf("Buffer after:  ");
	print_buffer(buffer);

	/* desfanse os mutex un por un */
	for (int j = 0; j < opt.buffer_size; j++){
		pthread_mutex_destroy(mutex_array[j]);
		free(mutex_array[j]);
	}
	/* liberase a memoria ocupada polo vector de mutex */
	free(args);
	free(threads);
	free(buffer.data);
	free(iter);
	pthread_mutex_destroy(mutex_iteracions);
	free(mutex_iteracions);

	pthread_exit(NULL);
}

int main (int argc, char **argv)
{
	struct options opt;

	// Default values for the options
	opt.num_threads = 10;
	opt.buffer_size = 10;
	opt.iterations  = 100;
	opt.delay       = 10;

	read_options(argc, argv, &opt);

	start_threads(opt);

	exit (0);
}
