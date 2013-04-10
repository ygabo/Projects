#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
// Yelnil Gabo
// 70179064
// g3d6

void *thread_1a(void *);
void *thread_1b(void * );
void *thread_1c(void * );
void *thread_1d(void * );
int c3_a();
int c3_d();
int c3_b();
int c3_c();

typedef struct {
  int lo, hi;
} c3s_arg;

typedef struct {
  int lo, hi, i;
} c3_i_arg;

typedef struct {
  int count;
  char pad[60];
} padded_int;

// Global variables
// number of threads
int NUM_THREADS = 32;
int THREADS_MIN = 2;
int THREAD_RANGE = 65;
int THREADS_MAX;

// shared count
int count = 0;
// padded counts
padded_int *padded_counts;
// not padded
int *intcounts_1c;
// lock
pthread_mutex_t count_mutex;

// globals for example
static int print = 1;
int length = 10000000;
int array[10000000];
int length_per;
static int initialized = 0;
typedef struct {
    int     secs;
    int     usecs;
} TIME_DIFF;

TIME_DIFF * my_difftime (struct timeval *, struct timeval *);


// 1a
// this demonstrates race condition
int c3_a(){
  pthread_t threads[NUM_THREADS];
  length_per = length / NUM_THREADS;
  int oldHi = 0, local = 0;
  count = 0;
  
  for (int i = 0; i< NUM_THREADS; i++){
    c3s_arg *lol = (c3s_arg*) (malloc(sizeof(c3s_arg)));
    lol->lo = oldHi;
    lol->hi = oldHi + length_per;
    
    // create threads and call function
    pthread_create( &threads[i], NULL, &thread_1a, (void *) lol );  
    oldHi = lol->hi;
  }
  
  // wait for threads to finish
  for (int i = 0; i < NUM_THREADS; i++)
    pthread_join(threads[i], NULL);
  
  if( print )
  // print count
    printf ("1a fail count: %d\n", count); 

  count = 0;
  local = count;
  return local;
}

// thread for 1a
// demonstrates race condition
void *thread_1a(void *void_arg) {
  // typecast
  c3s_arg *mine = (c3s_arg * ) (void_arg);
  int lo = mine->lo;
  int hi = mine->hi;
  int i;
  
  // no locks just go ahead and add
  for (i=lo; i<hi; i++)
    count = count + (array[i] == 3);
  
  pthread_exit(NULL);
}

// 1b
// uses locks excessively
int c3_b(){

  length_per = length / NUM_THREADS;
  pthread_t threads[NUM_THREADS];
  int oldHi = 0, local = 0;
  count = 0;
  double time;

  for (int i = 0; i< NUM_THREADS; i++){
    c3s_arg *lol = (c3s_arg*) (malloc(sizeof(c3s_arg)));
    lol->lo = oldHi;
    lol->hi = oldHi + length_per;
    
    // create threads and call functions
    pthread_create( &threads[i], NULL, &thread_1b, (void *) lol );  
    oldHi = lol->hi;
  }
  // wait for threads to finish
  for (int i = 0; i < NUM_THREADS; i++) pthread_join(threads[i], NULL);
  
  if( print ) printf ("1b count: %d\n", count);
  local = count;
  count = 0;
  return local;
}

// thread for 1b
// excessive locking
void *thread_1b(void *void_arg) {
  // typecast
  c3s_arg *mine = (c3s_arg * ) (void_arg);
  int lo = mine->lo;
  int hi = mine->hi;
  
  for (int i = lo; i < hi; i++){  
    pthread_mutex_lock(&count_mutex);
    if(array[i] == 3) count += 1;
    pthread_mutex_unlock(&count_mutex);
  }
  
  pthread_exit(NULL);
}

// 1c
// uses array of counts
// each index of the array represents
// the thread's count 
int c3_c(){
  pthread_t threads[NUM_THREADS];
  int oldHi = 0, local = 0;
  length_per = length / NUM_THREADS;
  intcounts_1c = (int*) (malloc(sizeof(int)*NUM_THREADS));
  for (int i = 0; i < NUM_THREADS; i++) intcounts_1c[i] = 0;
  count = 0;
  
  for (int i = 0; i< NUM_THREADS; i++){
    c3_i_arg *lol = (c3_i_arg*) (malloc(sizeof(c3_i_arg)));
    lol->lo = oldHi;
    lol->hi = oldHi + length_per;
    lol->i = i;
 
    // create threads and call functions*
    pthread_create( &threads[i], NULL, &thread_1c, (void *) lol );
    oldHi = lol->hi;
  }
  
  // wait for them to finish
  for (int i = 0; i < NUM_THREADS; i++) pthread_join(threads[i], NULL);

  if( print ) printf ("1c count: %d\n", count);
  local = count;
  count = 0;
  
  return local;
}

// thread for 1c
// for false sharing
void *thread_1c(void *void_arg) {
  // typecast
  c3_i_arg *mine = (c3_i_arg * ) (void_arg);
  int lo = mine->lo;
  int hi = mine->hi;
  int thread = mine->i;
  
  for (int i = lo; i < hi; i++){  
    intcounts_1c[mine->i] += (array[i] == 3) ;
  }
  
  pthread_mutex_lock(&count_mutex);
  count += intcounts_1c[thread];
  pthread_mutex_unlock(&count_mutex);
  
  pthread_exit(NULL);
}

// 1d
// made to get rid of false sharing
// used padded structs so we get 
// whole cache line for a single thread
int c3_d(){
  pthread_t threads[NUM_THREADS];
  int oldHi = 0, local = 0 ; count = 0;
  double time = 0; 
  length_per = length / NUM_THREADS;

  
  padded_counts = (padded_int*) (malloc(sizeof(padded_int)*NUM_THREADS));
  for (int i = 0; i < NUM_THREADS; i++) padded_counts[i].count = 0;
 
  for (int i = 0; i< NUM_THREADS; i++){
    c3_i_arg *lol = (c3_i_arg*) (malloc(sizeof(c3_i_arg)));
    lol-> i = i;
    lol->lo = oldHi;
    lol->hi = oldHi + length_per;
    
    // create threads and call functions
    pthread_create( &threads[i], NULL, &thread_1d, (void *) lol );  
    oldHi = lol->hi;
  }

  // wait for them to finish, then add to count
  for (int i = 0; i < NUM_THREADS; i++) pthread_join(threads[i], NULL);
  
  if( print ) printf ("1d count: %d\n", count);
  local = count;
  count = 0;
  
  return local;
}

// thread for 1d
// optimized by using padded struct
void *thread_1d(void *void_arg) {
  // typecast
  c3_i_arg *mine = (c3_i_arg * ) (void_arg);
  int lo = mine->lo;
  int hi = mine->hi;
  int pos = mine->i;
  
  for (int i = lo; i < hi; i++){  
    if(array[i] == 3) padded_counts[pos].count++ ;
  }  
  
  pthread_mutex_lock(&count_mutex); 
  count += padded_counts[pos].count;
  pthread_mutex_unlock(&count_mutex);  
  
  pthread_exit(NULL);
}

// MAIN
int main(int argc, char *argv[]) {
  clock_t t3,t4 ;
  double time;
  struct timeval myTVstart, myTVend;
  TIME_DIFF *diffd, *diffc, *diffb;
  
  // initialize
  if (!initialized){
    count = 0, time = 0;
    THREADS_MAX = THREADS_MIN + THREAD_RANGE + 1;
    pthread_mutex_init(&count_mutex, NULL);
    // pseudo random
    for (int i = 0; i < length; i++) array[i] = rand()%10;
    initialized = 1;
  }
  
  // Sequential
  gettimeofday (&myTVstart, NULL);
  for (int i = 0; i < length ; i++)
    if(array[i] == 3) count++ ;
  gettimeofday (&myTVend, NULL);
  
  diffb = my_difftime (&myTVstart, &myTVend);
  printf("Sequential: %d.%06d secs.\n", diffb->secs, diffb->usecs);
  printf("Correct count:  %d\n",count);
  count = 0;
  
  // Parallel
  c3_a();
  
  gettimeofday (&myTVstart, NULL);
  c3_b();
  gettimeofday (&myTVend, NULL);
  diffb = my_difftime (&myTVstart, &myTVend);
  
  gettimeofday (&myTVstart, NULL);
  c3_c();
  gettimeofday (&myTVend, NULL);
  diffc = my_difftime (&myTVstart, &myTVend);
  
  gettimeofday (&myTVstart, NULL);
  c3_d();
  gettimeofday (&myTVend, NULL);
  diffd = my_difftime (&myTVstart, &myTVend);

  printf("1b: %3d.%06d s\n", diffb->secs, diffb->usecs);
  printf("1c: %3d.%06d s\n", diffc->secs, diffc->usecs);
  printf("1d: %3d.%06d s\n", diffd->secs, diffd->usecs);
  
  pthread_mutex_destroy(&count_mutex);
  pthread_exit(NULL);
}

// helper to get run time
// got this from 
// http://cboard.cprogramming.com/cplusplus-programming/101085-how-measure-time-multi-core-machines-pthreads.html
// on how to measure run time of pthreads
TIME_DIFF * my_difftime (struct timeval * start, struct timeval * end){
    TIME_DIFF * diff = (TIME_DIFF *) malloc ( sizeof (TIME_DIFF) );
 
    if (start->tv_sec == end->tv_sec) {
        diff->secs = 0;
        diff->usecs = end->tv_usec - start->tv_usec;
    }
    else {
        diff->usecs = 1000000 - start->tv_usec;
        diff->secs = end->tv_sec - (start->tv_sec + 1);
        diff->usecs += end->tv_usec;
        if (diff->usecs >= 1000000) {
            diff->usecs -= 1000000;
            diff->secs += 1;
        }
    }
     
    return diff;
}
