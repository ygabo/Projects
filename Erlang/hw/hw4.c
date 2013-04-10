#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define RootProcess 0
#define RATE 0.02
#define ARRAY_SIZE(array) (sizeof((array))/sizeof((array[0])))

typedef struct { 
    int first,last; 
} First_last; 

typedef struct { 
    int day;
    double balance;
} Transaction; 

extern int gethostname(const char *buf, int len);
void myFunc( Transaction *, Transaction *, int *, MPI_Datatype * );
void myProd( First_last *, First_last *, int *, MPI_Datatype * );
void previous_M( double *, int , int , int , double * );


// MPI op function for first last
void myProd( First_last *in, First_last *inout, int *len, MPI_Datatype *dptr ) 
{ 
  int i; 
  First_last c = {-1,-1}; 
	
	for (i=0; i< *len; ++i){  
    // Get the minimum of the 2 firsts 
    if( inout->first == -1 ) c.first = in->first;
		else if( in->first == -1) c.first = inout->first;
		else if( in->first < inout->first) c.first = in->first;
		else c.first = inout->first;
		
    // Get the maximum of the 2 lasts
		if( inout->last == -1) c.last = in->last; 
		else if ( in->last == -1) c.last = inout->last;
		else if( in->last > inout->last) c.last = in->last; 
		else c.last = inout->last;
		
    *inout = c; 
    in++; inout++; 
  } 
};

// ~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_
// 1 a
// first_last problem
// 
void first_last( int *src, int count, int q, int *dst, MPI_Comm comm)
{
  // variables
  int myID, myCount, value, length_p;
  int *myArray, *rootArray;
  MPI_Status status;
  int Q                               = q;
  First_last f_l                      = {-1,-1};
  First_last f_l_global               = {-1,-1};
  int tag                             = 1;
  int stride                          = 0;
  int root_length                     = 0;
	
  // initialize MPI stuff
  MPI_Comm_rank(comm, &myID);
  MPI_Op myOp;
  MPI_Datatype ctype;
  int dtype_count = 2;
  int lengths[2] = {1, 1};
  MPI_Aint offsets[2] = {0, sizeof(int)};
  MPI_Datatype types[2] = {MPI_INT, MPI_INT};
  MPI_Type_struct(dtype_count, lengths, offsets, types, &ctype);
  MPI_Type_commit(&ctype);
  MPI_Op_create( (MPI_User_function *)myProd, 1, &myOp ); 
  
  // if root process, distribute the list to everyone
  // some of this was taken from Mark's code (count 3's)
  if(myID == RootProcess) {
    int length=0, length_div_numProcs, length_remainder;
    int numProcs=1;
    length = count;
    
    MPI_Comm_size(comm, &numProcs);
    
    length_div_numProcs = length/numProcs;
    length_remainder = length % numProcs;
    myArray = (int *)malloc((length_div_numProcs+1)*sizeof(int));	
    rootArray = (int *) malloc((length_div_numProcs+1)*sizeof(int));
    
    // save root's array for later
    length_p = length_div_numProcs + (length_remainder != 0);
    for(int i = 0; i < length_p; i++) rootArray[i] = src[stride+i];
    root_length = length_p;
    stride += length_p;
    
    for(int p=1; p < numProcs; p++){
      length_p = length_div_numProcs + (p < length_remainder);
      // send length and stride
      MPI_Send(&length_p, 1, MPI_INT, p, tag+1, comm);
      MPI_Send(&stride, 1, MPI_INT, p, tag+2, comm);
      for(int i = 0; i < length_p; i++) myArray[i] = src[stride+i];
      // send the list
      MPI_Send(myArray, length_p, MPI_INT, p, tag, comm);
      stride += length_p;
    }
    stride = 0;
  } else {
    // im not root, recieve the stuff
    MPI_Recv(&length_p, 1, MPI_INT, RootProcess, tag+1,
    	comm, &status);	
    MPI_Recv(&stride, 1, MPI_INT, RootProcess, tag+2,
    	comm, &status);
    myArray = (int *)malloc(length_p*sizeof(int));
    MPI_Recv(myArray, length_p, MPI_INT, RootProcess, tag,
    	comm, &status);
  }
  
  // im root, reassign my local array
  // it was overwritten during the sending phase
  if( myID == RootProcess){
    length_p = root_length;
    myArray = (int *)malloc(length_p*sizeof(int));
    for (int i=0; i<length_p; i++){    
      myArray[i] = rootArray[i];
    }    
  } 
  
  // get local (first, last) indeces 
  for(int i = 0; i < length_p; i++) {
    if ( myArray[i] == Q )
    {
      if( (stride+i < f_l.first ) || (f_l.first == -1 ) )
        f_l.first = stride+i;			
      if( (stride+i > f_l.last ) || (f_l.last == -1 ) )
          f_l.last = stride+i;
    }
  }
  
  // Reduce
  MPI_Reduce(&f_l, &f_l_global, 1, ctype, myOp, RootProcess, comm);
  
  if(myID == RootProcess) {
    dst[0] = f_l_global.first;
    dst[1] = f_l_global.last;
  }
  
  MPI_Op_free( &myOp );
}

// helper 1b
// add the M elements previous to i 
double total_of_previous_M( double * my_array, int i, double *last_m_array, int M )
{
  double total = 0;
  double m_total = 0;
  for( int x = 0; x < i; x++ ) total += my_array[x];
  if (i-M < 0) for( int x = i; x < M; x++ ) m_total += last_m_array[x];
  return total + m_total;
}

// helper 1b
// given a list
// output a list of its M rolling average
void rolling_average_array( double * my_array, int length, int M, double * last_m_array, double *output_array )
{
  for( int i = 0; i < length; i++ ) {
    output_array[i] = my_array[i] + total_of_previous_M( my_array, i, last_m_array, M );
    output_array[i] /= M+1;
  }
}

// helper 1b
// put last M elements of array into outarray
void last_M( double *array, int arraylength, int M, double * outarray)
{
  if ( arraylength < M )
    for ( int i = 0; i < arraylength; i ++ ) outarray[i] = array[i];
  else
    for ( int i = 0; i < M; i ++ ) outarray[i] = array[arraylength - M + i ];
}

// ~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_
// 1 b
// Rolling average problem
// 
void rolling_average( double *src, double *dst, int count, int m,  MPI_Comm comm)
{
  // LOCALS
  double * mine;
  int myID, myCount, value, length_p = 0;
  MPI_Status status;
  int numProcs=1;
  int M = m-1;
  int tag=1;
  int stride=0, root_length=0;
  int left_length = 0;
  int both_length = 0;
  double *my_array, *root_array;
  double *appended_array, *left_array;
  double *output_array, *last_m_array;
  double * left_rolling_average;
  
  // MPI initialize
  // not needed for implementation
  MPI_Comm_rank(comm, &myID);
  MPI_Comm_size(comm, &numProcs);
  MPI_Op myOp;
  MPI_Datatype mType;
  MPI_Datatype fTypes[] = { MPI_INT, MPI_INT, MPI_DOUBLE };
  int blocklens[] = { 1, 1, count };
  MPI_Aint offsets[3], extent;
  offsets[0] = 0;
  MPI_Type_extent(MPI_INT, &extent);
  offsets[1] = offsets[0] + blocklens[0]*extent;
  offsets[2] = offsets[1] + blocklens[1]*extent;
  
  /*
  count -number of blocks (integer) -- also number of entries in arrays array_of_types , array_of_displacements and array_of_blocklengths
  blocklens -number of elements in each block (array)
  indices -byte displacement of each block (array)
  old_types -type of elements in each block (array of handles to datatype objects)
  */
  MPI_Type_struct(2, blocklens, offsets, fTypes, &mType);
  //MPI_Datatype mType;
  //MPI_Type_contiguous(M, MPI_DOUBLE, &mType); 
  
  MPI_Type_commit(&mType); 
  MPI_Op_create( (MPI_User_function *)myProd, 0, &myOp ); 
  
  // DISTRIBUTE LIST
  if(myID == RootProcess) {
    int length=0, length_div_numProcs, length_remainder;
    length = count;    
    length_div_numProcs = length/numProcs;
    length_remainder = length % numProcs;
    
    // save the root's part of the list
    my_array = (double *)malloc((length_div_numProcs+1)*sizeof(double));	
    root_array = (double *) malloc((length_div_numProcs+1)*sizeof(double));
    length_p = length_div_numProcs + (length_remainder != 0);
    for(int i = 0; i < length_p; i++) root_array[i] = src[stride+i];
    root_length = length_p;
    stride += length_p;
    
    for(int p=1; p < numProcs; p++){
      length_p = length_div_numProcs + (p < length_remainder);
      // send the proper lengths
      MPI_Send(&length_p, 1, MPI_INT, p, tag+1, comm);
      MPI_Send(&M, 1, MPI_INT, p, tag+2, comm);
      
      // send the stuff
      for(int i = 0; i < length_p; i++) my_array[i] = src[stride+i];
      MPI_Send(my_array, length_p, MPI_DOUBLE, p, tag, comm);
      stride += length_p;
    }    
    stride = 0;
  } else { 
    // I'm not root, recieve the stuff
    MPI_Recv(&length_p, 1, MPI_INT, RootProcess, tag+1, comm, &status);	
    MPI_Recv(&M, 1, MPI_INT, RootProcess, tag+2, comm, &status);
 
    my_array = (double *)malloc(length_p*sizeof(double));
    MPI_Recv(my_array, length_p, MPI_DOUBLE, RootProcess, tag, comm, &status);
  }
  
  // copy back root process' sub list
  // (it was over written when we were sending stuff
  // to other guys)
  if( myID == RootProcess){
    length_p = root_length;
    my_array = (double *)malloc(length_p*sizeof(double));
    for (int i=0; i<length_p; i++){    
      my_array[i] = root_array[i];
    }    
  }

  // Pretend Scan
  // send my array + everything to my left to the next node :p
  // let Root go first, when I recieved left side 
  // append mine to it, then send that over to the next guy
  if ( myID == RootProcess && numProcs > 1) {
    MPI_Send(&length_p, 1, MPI_INT, myID+1, tag+3, comm);
    MPI_Send(my_array, length_p, MPI_DOUBLE, myID+1, tag+4, comm);  
  }
  else { 
    // recieve from left neighbour
    MPI_Recv(&left_length, 1, MPI_INT, myID-1, tag+3, comm, &status);
    
    left_array = (double *)malloc(left_length*sizeof(double));
    MPI_Recv(left_array, left_length, MPI_DOUBLE, myID-1, tag+4, comm, &status);
    
    // new length of list
    both_length = left_length + length_p; 
    
    // put the left array and my array together
    // send this to the next node
    appended_array = (double *)malloc(both_length*sizeof(double));
    
    for( int i = 0 ; i < left_length; i++ ) appended_array[i] = left_array[i];
    for( int i = 0 ; i < length_p; i++ ) appended_array[left_length+i] = my_array[i];
    
    // last node doesn't have anyone to send to
    if( myID != numProcs-1 ){
      MPI_Send(&both_length, 1, MPI_INT, myID+1, tag+3, comm);
      MPI_Send(appended_array, both_length, MPI_DOUBLE, myID+1, tag+4, comm);
    }
  }
 
  last_m_array = (double *)malloc(M*sizeof(double));
  for(int i = 0; i < M; i ++ ) last_m_array[i] = 0;
 
  // get last M elements of the array you recieved
  last_M( left_array, left_length, M, last_m_array);
  
  output_array = (double *)malloc(length_p*sizeof(double));
  
  // calculate rolling average array
  // each node will have their local rolling average
  rolling_average_array( my_array, length_p, M, last_m_array, output_array );
 
  // im the last node, copy to output
  if( myID == numProcs-1 ){
    left_rolling_average = (double *)malloc(both_length*sizeof(double));
    for(int i = 0; i < M; i ++ ) last_m_array[i] = 0;
    rolling_average_array( appended_array, both_length, M, last_m_array, left_rolling_average );
    for(int i = 0; i < both_length; i ++ )
      dst[i] = left_rolling_average[i] ;
  }
  
  MPI_Op_free( &myOp );
}

// helper 1c
/* Function to calculate x raised to the power y */
//http://www.geeksforgeeks.org/archives/28
double power(double x, unsigned int y)
{
    if( y == 0)
        return 1;
    else if (y%2 == 0)
        return power(x, y/2)*power(x, y/2);
    else
        return x*power(x, y/2)*power(x, y/2);
}

// helper 1c
// calculate balance after so many days with a daily rate
double previous_balance_to_current_day( double balance, int days, double rate ){
  return balance * power((1 + rate), days);
}; 

// helper 1c
// given a list of transactions
// get the final balance and the final day
void calculate_local_Transactions( Transaction * local, int length, double *finalbalance, int *lastday)
{
  double lolbalance = local[0].balance;
  int lolday = local[0].day;
  
  for (int i = 1 ; i < length; i++ ){
    lolbalance = previous_balance_to_current_day( lolbalance, local[i].day - lolday, RATE) + local[i].balance;
    lolday = local[i].day;
  }
  
  *finalbalance = lolbalance;
  *lastday = lolday;
};

// MPI op function for credit card problem
void myFunc( Transaction *in, Transaction *inout, int *len, MPI_Datatype *dptr ) 
{ 
    int days=0; 
    Transaction c = {-1,-1}; 
	
	for (int i=0; i< *len; ++i) {
    //printf("%f", c.balance);
    days = inout[i].day - in[i].day;
    c.day = inout[i].day;
    c.balance = previous_balance_to_current_day( in[i].balance, days, RATE ) + inout[i].balance;
    *inout = c;
    in++; inout++; 
  } 
};

// ~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_
// 1 c
// credit card problem
// 
void balance( Transaction *src, double * dst, int count,  MPI_Comm comm)
{
  // variables
  int myID, myCount, value, length_p;
  MPI_Status status;
  Transaction f_l = {-1,-1};
  Transaction f_l_global = {-1,-1};
  int tag=1;
  int stride=0, root_length=0;
  Transaction *myArray, *rootArray;

  // initialize MPI stuff
  MPI_Comm_rank(comm, &myID);  
  MPI_Op myOp; 
  MPI_Datatype ctype; 
  MPI_Aint extent;
  int dtype_count = 2;
  int lengths[2] = {1, 1};
  
  MPI_Type_extent(MPI_DOUBLE, &extent);
  MPI_Aint offsets[2] = {0, extent};
  MPI_Datatype types[2] = {MPI_INT, MPI_DOUBLE};
  MPI_Type_struct(dtype_count, lengths, offsets, types, &ctype);
  MPI_Type_commit(&ctype);
 
  MPI_Op_create( (MPI_User_function *)myFunc, 1, &myOp ); 
  
  // if you are the root process
  // send to others their part of the list
  // most of this process was from Mark's sample code
  // added stride 
  if(myID == RootProcess) {
    int length=0, length_div_numProcs, length_remainder;
    int numProcs=1;
    length = count;
    
    MPI_Comm_size(comm, &numProcs);
    
    length_div_numProcs = length/numProcs;
    length_remainder = length % numProcs;
    myArray = (Transaction *)malloc((length_div_numProcs+1)*sizeof(Transaction));	
    rootArray = (Transaction *) malloc((length_div_numProcs+1)*sizeof(Transaction));
	
    length_p = length_div_numProcs + (length_remainder != 0);
    for(int i = 0; i < length_p; i++) { 
      rootArray[i] = src[stride+i];	  
    }
    root_length = length_p;
    stride += length_p;
    for(int p=1; p < numProcs; p++)
    {
      // read data on behalf of each of the other processes 
      length_p = length_div_numProcs + (p < length_remainder);
	  
      MPI_Send(&length_p, 1, MPI_INT, p, tag+1, comm);
      MPI_Send(&stride, 1, MPI_INT, p, tag+2, comm);
      for(int i = 0; i < length_p; i++)
      {
        myArray[i] = src[stride+i];
      }
      MPI_Send(myArray, length_p, ctype, p, tag, comm);
      stride += length_p;
    }
    stride = 0;
    
    } else { 
      MPI_Recv(&length_p, 1, MPI_INT, RootProcess, tag+1,
        comm, &status);	
      MPI_Recv(&stride, 1, MPI_INT, RootProcess, tag+2,
        comm, &status);
   
      myArray = (Transaction *)malloc(length_p*sizeof(Transaction));
      MPI_Recv(myArray, length_p, ctype, RootProcess, tag,
        comm, &status);
    }

  // if root process, re-assign my array
  // since it got overwritten during sending
  if( myID == RootProcess){
    length_p = root_length;
    myArray = (Transaction *)malloc(length_p*sizeof(Transaction));
    for (int i=0; i<length_p; i++){    
      myArray[i] = rootArray[i];
    }    
  } 
   
  int day = 0;
  double lol = 0;
  
  // helper 
  // to calculate local node's final balance
  // and final day
  calculate_local_Transactions( myArray, length_p, &lol, &day );
  
  f_l.day = day;
  f_l.balance = lol;
  
  // SCAN
  MPI_Scan(&f_l, &f_l_global, 1, ctype, myOp, comm);
  
  // assign it to the output
  *dst = f_l_global.balance;
  
  MPI_Op_free( &myOp );
}

// MAIN
int main(int argc, char **argv) {

  int myID, numProcs;
  MPI_Init(&argc, &argv);    
  MPI_Comm_rank(MPI_COMM_WORLD, &myID);
  MPI_Comm_size(MPI_COMM_WORLD, &numProcs);
  char myHost[100];
  gethostname(myHost, 100);
  
  // First,Last example
  int Q           = 4;
  int global[2]   = {-1,-1};  
  int list[9] = {1,2,3,4,5,6,7,4,5};
  
  // call first_last
  first_last( list, 9, Q, global, MPI_COMM_WORLD );
  
  if(myID == RootProcess)
    printf("First and last index of %d: (%d,%d)\n", Q, global[0], global[1]);
  
  // Credit card example
  int count    = 8;
  double total = 0; 
  int day      = 0;
  Transaction array[count];
  
  //{1,17.42},{2,5},{3,-20},{4,1},{4,12.34},(6, -20),(7,10),(10,9.99)
  array[0].day=1;  array[0].balance = 17.42;
  array[1].day=2;  array[1].balance = 5;
  array[2].day=3;  array[2].balance = -20;
  array[3].day=4;  array[3].balance = 1;
  array[4].day=4;  array[4].balance = 12.34;
  array[5].day=6;  array[5].balance = -20;
  array[6].day=7;  array[6].balance = 10;
  array[7].day=10; array[7].balance = 9.99;
  
  // call balance
  balance( array, &total, count, MPI_COMM_WORLD );    
  
  // if last node
  if( myID == numProcs-1 ) printf("Final balance: %f\n", total );
  
   // Rolling average example
  double dst[8];  
  double num_list[8] = {1,4,9,16,25,36,49,64};
  int M = 3;
  
  rolling_average( num_list, dst, 8, M, MPI_COMM_WORLD );    
  
  // last node
  if ( myID == numProcs-1 ){
    printf("%d-rolling average: \n[", M);
    for (int i=0; i<7; i++){ printf("%f, ", dst[i]); }
    printf("%f", dst[7]);
    printf("]\n");
  }  
  MPI_Finalize();
  return(0);
}


