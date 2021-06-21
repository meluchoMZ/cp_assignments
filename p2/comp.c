#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <pthread.h>
#include "compress.h"
#include "chunk_archive.h"
#include "queue.h"
#include "options.h"

#define CHUNK_SIZE (1024*1024)
#define QUEUE_SIZE 20

#define COMPRESS 1
#define DECOMPRESS 0

typedef struct _reader_args {
	queue q_in; // input queue
	struct options opt; 
	int chunks; // number of chunks of the file
	int file_descriptor; // file descriptor of the file to compress
} reader_args;

typedef struct _worker_args {
	queue q_in; // input queue  
	queue q_out; // output queue
	chunk (*function)(chunk); // zcompress function
	int chunks;
	int* iterations;
	pthread_mutex_t* m_iterations;
} worker_args;

typedef struct _writer_args {
	queue q_out; // output queue
	archive arch; // archive file
	int chunk_number; //number of chunks of the file
} writer_args;

// take chunks from queue in, run them through process (compress or decompress), send them to queue out
void worker(queue in, queue out, chunk (*process)(chunk)) {
    chunk ch, res;

        ch = q_remove(in);
        res = process(ch);
        free_chunk(ch);

        q_insert(out, res);

}

// this function reads from the file to the input queue
void* reader_compression(void* reader_arguments) {
	reader_args* args = (reader_args*) reader_arguments;
	chunk ch;
	int offset;	
	for(int i = 0; i < args->chunks; i++) {
		ch = alloc_chunk(args->opt.size);
		offset = lseek(args->file_descriptor, 0, SEEK_CUR);

		ch->size = read(args->file_descriptor, ch->data, args->opt.size);
		ch->num = i;
		ch->offset = offset;

		q_insert(args->q_in, ch);	

	}
	return NULL;
}

// this function is used as interface between pthread_create *func to worker
void* worker_wrapper (void* void_arguments) {
	worker_args* args = (worker_args*) void_arguments;
	while (1) {
		pthread_mutex_lock(args->m_iterations);
		if (*args->iterations >= args->chunks) {pthread_mutex_unlock(args->m_iterations); break;}

		*args->iterations = *args->iterations + 1;
		pthread_mutex_unlock(args->m_iterations);
		worker(args->q_in, args->q_out, args->function);
	}
	return NULL;
}

void* writer_compression (void* writer_arguments) {
	writer_args* args = (writer_args*) writer_arguments;
	chunk ch;
	for(int i=0; i<args->chunk_number; i++) {
			
        	ch = q_remove(args->q_out);

        	add_chunk(args->arch, ch);
        	free_chunk(ch);
    	}	
	return NULL;
}

void* reader_decomp (void* arguments) {
	writer_args* args = (writer_args*) arguments;
	chunk ch;
	for (int i = 0; i < args->chunk_number; i++) {
		ch = get_chunk(args->arch, i);
        q_insert(args->q_out, ch);
	}
	return NULL;
}

void* writer_decomp (void* arguments) {
	reader_args* args = (reader_args*) arguments;
	chunk ch;
	for (int i = 0; i < args->chunks; i++) {
		ch=q_remove(args->q_in);
        lseek(args->file_descriptor, ch->offset, SEEK_SET);
        write(args->file_descriptor, ch->data, ch->size);
        free_chunk(ch);
	}
	return NULL;
}

// Compress file taking chunks of opt.size from the input file,
// inserting them into the in queue, running them using a worker,
// and sending the output from the out queue into the archive file
void comp(struct options opt) {
    int fd, chunks;
    struct stat st;
    char comp_file[256];
    archive ar;
    queue in, out;
	pthread_t thread_array[opt.num_threads];
	reader_args r_args;
	worker_args w_args;
	writer_args wr_args;
	pthread_t thread_reader, thread_writer;
	int it = 0;
	pthread_mutex_t m_it;


    if((fd=open(opt.file, O_RDONLY))==-1) {
        printf("Cannot open %s\n", opt.file);
        exit(0);
    }
    fstat(fd, &st);
    chunks = st.st_size/opt.size+(st.st_size % opt.size ? 1:0);

    if(opt.out_file) {
        strncpy(comp_file,opt.out_file,255);
    } else {
        strncpy(comp_file, opt.file, 255);
        strncat(comp_file, ".ch", 255);
    }
    ar = create_archive_file(comp_file);

    in  = q_create(opt.queue_size);
    out = q_create(opt.queue_size);

	
	// reader arguments struct initialization
	r_args.q_in = in;
	r_args.opt = opt;
	r_args.chunks = chunks;
	r_args.file_descriptor = fd;

	// writer arguments struct initialization
	wr_args.q_out = out;
	wr_args.arch = ar;
	wr_args.chunk_number = chunks;

	// creation of the thread to read from queue
	if (pthread_create(&thread_reader, NULL, reader_compression, (void*) &r_args) != 0) {
		printf("Cannot start thread to read to queue. Exiting...\n");
		exit(EXIT_FAILURE);
	}
	

	// worker arguments struct initialization
	w_args.q_in = in;
	w_args.q_out = out;
	w_args.function = zcompress;
	w_args.chunks = chunks;
	w_args.iterations = &it;
	pthread_mutex_init(&m_it, NULL);
	w_args.m_iterations = &m_it;

	// here opt.num_threads are created to comunicate in and out queue
	for (int j = 0; j < opt.num_threads; j++) {
		if (pthread_create(&thread_array[j], NULL, worker_wrapper, (void*) &w_args) != 0) {
			printf("Cannot start thread %d\n. Exiting", j);
			exit(EXIT_FAILURE);
		}
	}
	
	// creation of the thread to write to archive
	if (pthread_create(&thread_writer, NULL, writer_compression, (void*) &wr_args) != 0) {
		printf("Cannot start thread to write to compressed file. Exiting...\n");
		exit(EXIT_FAILURE);
	}


	pthread_join(thread_reader, NULL);
	for (int j = 0; j < opt.num_threads; j++) pthread_join(thread_array[j], NULL);
	pthread_join(thread_writer, NULL);



    close_archive_file(ar);
    close(fd);
	pthread_mutex_destroy(&m_it);
    q_destroy(in);
    q_destroy(out);
}


// Decompress file taking chunks of opt.size from the input file,
// inserting them into the in queue, running them using a worker,
// and sending the output from the out queue into the decompressed file

void decomp(struct options opt) {
    int fd;
    char uncomp_file[256];
    archive ar;
    queue in, out;
    writer_args wr_args;
	worker_args w_args;
	reader_args r_args;
	int it = 0;
	pthread_mutex_t m_it;
	pthread_t thread_reader;
	pthread_t thread_writer;
	pthread_t thread_array[opt.num_threads];

    if((ar=open_archive_file(opt.file))==NULL) {
        printf("Cannot open archive file\n");
        exit(0);
    };

    if(opt.out_file) {
        strncpy(uncomp_file, opt.out_file, 255);
    } else {
        strncpy(uncomp_file, opt.file, strlen(opt.file) -3);
        uncomp_file[strlen(opt.file)-3] = '\0';
    }

    if((fd=open(uncomp_file, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH))== -1) {
        printf("Cannot create %s: %s\n", uncomp_file, strerror(errno));
        exit(0);
    }

    in  = q_create(opt.queue_size);
    out = q_create(opt.queue_size);

	// writer_args initialization
	wr_args.q_out = in;
	wr_args.arch = ar;
	wr_args.chunk_number = chunks(ar);
	
	if (pthread_create(&thread_reader, NULL, reader_decomp, (void*) &wr_args) != 0) {
		printf("Cannot start thread to start decompression. Exiting...\n");
		exit(EXIT_FAILURE);
	}


    // decompress from in to out
	w_args.q_in = in;
	w_args.q_out = out;
	w_args.function = zdecompress;
	w_args.chunks = chunks(ar);
	w_args.iterations = &it;
	pthread_mutex_init(&m_it, NULL);
	w_args.m_iterations = &m_it;
	
	for (int j = 0; j < opt.num_threads; j++) {
		if (pthread_create(&thread_array[j], NULL, worker_wrapper, (void*) &w_args) != 0) {
			printf("Cannot start thread %d. Exiting...", j);
			exit(EXIT_FAILURE);
		}
	}



	//reader_args initialization (it is used for the writing because the operations in comp and decomp are symmetric and the structs can be shared)
	r_args.q_in = out;
	r_args.opt = opt;
	r_args.chunks = chunks(ar);
	r_args.file_descriptor = fd;
	
	if (pthread_create(&thread_writer, NULL, writer_decomp, (void*) &r_args) != 0) {
		printf("Cannot start thread to write of decompressed file. Exiting...\n");
		exit(EXIT_FAILURE);
	}

    pthread_join(thread_reader, NULL);
	for (int j = 0; j < opt.num_threads; j++) pthread_join(thread_array[j], NULL);
	pthread_join(thread_writer, NULL);
    close_archive_file(ar);
    close(fd);
    q_destroy(in);
    q_destroy(out);
}

int main(int argc, char *argv[]) {
    struct options opt;

    opt.compress    = COMPRESS;
    opt.num_threads = 3;
    opt.size        = CHUNK_SIZE;
    opt.queue_size  = QUEUE_SIZE;
    opt.out_file    = NULL;

    read_options(argc, argv, &opt);

    if(opt.compress == COMPRESS) comp(opt);
    else decomp(opt);
}
