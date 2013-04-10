#include
#include "mpi.h"
#include
using namespace itpp;
int main(int argc, char** argv)
{
	int my_rank;
	int p;
	MPI_Init(&argc,&argv);
	MPI_Comm_rank(MPI_COMM_WORLD,&my_rank);
	MPI_Comm_size(MPI_COMM_WORLD,&p);
	vec test(3),result(3);
	if (my_rank==0)
	{
		test(0)=1; test(1)=2; test(2)=3;
	}
	else
	{
		test(0)=4; test(1)=5; test(2)=6;
	}
	MPI_Reduce(&test(0),&result(0), test.length(), MPI_DOUBLE,
	MPI_SUM, 0, MPI_COMM_WORLD);
	std::cout<<"test from "<<my_rank<<" :"<<test<<"\n";
	std::cout<<"result from "<<my_rank<<" :"<<result<<"\n";
	MPI_Finalize();
}