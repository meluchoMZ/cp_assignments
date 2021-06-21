CFLAGS=-g -Wall -Wextra
OBJS=compress.o chunk_archive.o options.o queue.o comp.o
LIBS=-lz -pthread
CC=gcc

all: comp qtest

comp: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

qtest:
	 $(CC)  -pthread -g -o qtest queue.c queue_test.c
clean: 
	rm -f *.o comp
