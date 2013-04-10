#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define RootProcess 0
#define ARRAY_SIZE(array) (sizeof((array))/sizeof((array[0])))


typedef struct {
  double length;
  double * list;
} M_struct;

extern int gethostname(const char *buf, int len);
void myProd( M_struct *, M_struct *, int *, MPI_Datatype * ) ;
void previous_M( double *, int , int , int , double * );
double total_M( double *, int, int );


// RELEVANT
void myProd( M_struct *in, M_struct *inout, int *len, MPI_Datatype *dptr ) 
{  
  double new_length = in->length + inout->length ;
  double * list = (double *)malloc(new_length*sizeof(double));
  M_struct a = { in->length, in->list };
  int t = 0;
  
  for (int o = 0; o < (int) in->length; o ++ ){ 
    list[o] = in->list[o];
    t++;
  }
  for (int o = 0; o < (int) inout->length; o ++ ){
    list[t+o] = inout->list[o];
  }
  t = 0;  
  M_struct c = { (double) new_length, list };
  
  *inout = c;
  *in = c ;
    /*  
    M_struct yo = { i, c };
    
    *left = yo;
    *right = yo;
    left++;
    right++;
    printf(" rlength-%f \n", inout->length );*/
   // *in = *inout;
     
};

double total_M( double *array, int i, int M )
{ 
  //printf("--(%d, %d)", i, M);
  if ( M < 1 ) return 0;
  if ( (i-M) < 0 ) return 0 + total_M( array, i, M-1);
  return array[i-M] + total_M( array, i, M-1 );
}

void previous_M( double *array, int last, int M, int olength, double * outarray )
{ 
  if ( M < 1 ) return;
  
  if ( (last-M) < 0 ){
  outarray[olength-M] = 0;
  return previous_M( array, last, M-1, olength, outarray );
  }
  
  outarray[olength-M] = array[last-M];
  return previous_M( array, last, M-1, olength, outarray);
}

void rolling_average( double *src, double *dst, int count, int m,  MPI_Comm comm)
{
  // LOCALS
  double * mine;
  int myID, myCount, value, length_p = 0;
  MPI_Status status;
  int M = m-1;
  int tag=1;
  int stride=0, root_length=0;
  double *myArray, *rootArray, *ans_Array;
  
  // MPI initialize
  MPI_Comm_rank(comm, &myID);
  MPI_Op myOp;
  MPI_Datatype mType;
  MPI_Datatype fTypes[] = { MPI_DOUBLE, MPI_DOUBLE };
  int blocklens[] = { 1, count };
  MPI_Aint offsets[2], extent;
  offsets[0] = 0;
  MPI_Type_extent(MPI_DOUBLE, &extent);
  offsets[1] = offsets[0] + blocklens[0]*extent;
  
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
    int numProcs=1;
    length = count;
    
    MPI_Comm_size(comm, &numProcs);
    
    length_div_numProcs = length/numProcs;
    length_remainder = length % numProcs;
    
    myArray = (double *)malloc((length_div_numProcs+1)*sizeof(double));	
    rootArray = (double *) malloc((length_div_numProcs+1)*sizeof(double));
	
    length_p = length_div_numProcs + (length_remainder != 0);
    //printf("length_p-%d\n", length_p);
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
        MPI_Send(&M, 1, MPI_INT, p, tag+2, comm);
        for(int i = 0; i < length_p; i++)
      {
      myArray[i] = src[stride+i];
        }
        MPI_Send(myArray, length_p, MPI_DOUBLE, p, tag, comm);
      stride += length_p;
      }
    
    stride = 0;
    
  } else { 
    MPI_Recv(&length_p, 1, MPI_INT, RootProcess, tag+1,
    	comm, &status);	
    MPI_Recv(&M, 1, MPI_INT, RootProcess, tag+2,
    	comm, &status);
 
    myArray = (double *)malloc(length_p*sizeof(double));
    ans_Array = (double *)malloc(length_p*sizeof(double));
    MPI_Recv(myArray, length_p, MPI_DOUBLE, RootProcess, tag,
    	comm, &status);
  }
  
  // copy back root process' proper sub list
  // (it was over written when we were sending stuff
  // to other guys)
  if( myID == RootProcess){
    length_p = root_length;
    ans_Array = (double *)malloc(length_p*sizeof(double));
    myArray = (double *)malloc(length_p*sizeof(double));
    for (int i=0; i<length_p; i++){    
      myArray[i] = rootArray[i];
    }    
  }
  
  printf(" -%d- local: ", myID); 
  M_struct m_last = { (double)length_p, myArray }, global = {0,0};
  
  // SCAN
  MPI_Scan(&m_last, &global, 1, mType, myOp, comm);
  
  // print stuff
  for (int i=0; i<length_p; i++){ printf(" %f, ", myArray[i]); }
  printf("\n -%d- globals: ", myID); 
  for (int i=0; i<global.length; i++){ printf(" %f, ", global.list[i]); }
  printf("\n");  
  
 
  MPI_Op_free( &myOp );
}

//http://math.nist.gov/mcsd/savg/auto/v3.00/automap_reduce.html
//http://www.mcs.anl.gov/research/projects/mpi/www/www3/MPI_Scatterv.html
//http://www.mpi-forum.org/docs/mpi-11-html/node80.html 
int main(int argc, char **argv) {

  int M = 0, myID;
  double global[2] = {-1,-1};
  
  double num_list[8] = {1,2,3,4,5,6,7,8};
  int index = 2;
  
  M = 4;
  MPI_Init(&argc, &argv);    
  //MPI_Comm_rank(MPI_COMM_WORLD, &myID);
 
  char myHost[100];
  gethostname(myHost, 100);
 
  rolling_average( num_list, global, 8, M, MPI_COMM_WORLD );    

  MPI_Finalize();
  return(0);
}


