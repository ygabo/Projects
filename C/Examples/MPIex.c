#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"

#define NELTS 50000		/* number of elements in the vector */
#define MASTER 0		/* id of the first process */ 
#define MAXPROCS 1024	/* maximum number of processes */


main(int argc, char **argv) {

   double	v[NELTS],			/* the vector to be summed */
			sum;				/* the overall vector sum */
   int		numProcs,			/* number of processes in virtual machine */
 			rank,				/* our rank */ 
 			i,					/* simple counter */
			count;				/* the number of elements in a partition */



/******** master and slaves *********/

	// initialize MPI, etc.
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numProcs);

	// compute partition size
	count = NELTS/numProcs;


/********* master process ***********/
	if (rank == MASTER) {
		// Initialize the vector
		for (i=0; i<NELTS; i++)
			v[i]= 1;

		// check that numProcs divides NELTS else abort
		if (count*numProcs!=NELTS) {
			printf("Number of processes (%d) must divide evenly into problem size (%d).\n",numProcs,NELTS);
			fflush(stdout);
			MPI_Abort(MPI_COMM_WORLD,1);
		}
	} // end master


/******** master and slaves *********/
	// distribute vector partitions among processes using MPI_Gather
	MPI_Scatter(v,count,MPI_DOUBLE,v,NELTS,MPI_DOUBLE,MASTER,MPI_COMM_WORLD);

	// compute our partial sum (MASTER AND SLAVES)
	sum=0;
	for (i=0;i<count;i++)
		sum+=v[i];

	// gather partial sums from processes using MPI_Gather
	MPI_Reduce(&sum,&sum,1,MPI_DOUBLE,MPI_SUM,MASTER,MPI_COMM_WORLD);


/********* master process ***********/
	if (rank == MASTER) {
		// print result
		printf("Vector sum is %lf.\n", sum);
	}  // end master


/******** master and slaves *********/
	MPI_Finalize();
}  // main