#include <iostream>
#include <sstream>
#include <cmath>
#include <string>
#include <ctime>

using namespace std;

const int N = 25;

void Jacobi(int n, const double a[][N], const double b[N])
{
	double ti = clock();
	double v[n], sum = 0, RMS;
	double xx[N] = {0,0,0,0,0};
	double x1[N] = {1,1,1,1,1};
	int i = 0,j = 0,k = 0;
	int M, iter = 0;
	string s;
	cout << "The number of iteration: ";
	getline(cin,s);
	if(!s.empty())
	{
		RMS = sqrt(sum / n);
		stringstream(s) >> M;
		for(k=0; k<M; k++)
		{
			for(i=0; i<n; i++)
			{
				int xi = 0;
				for(j=0; j<n; j++)
				{
					if(i!= j)
					{
						xi+= a[i][j]*x1[j];
					}
				}
				x1[i] = (b[i] - xi)/a[i][i];
				if(k==M-2)
				{
					v[i] = x1[i];
				}
				if(k==M-1)
				{
					sum+= pow(x1[i]-v[i],2);
				}
			}
		}
		cout << "iteration " << M << "\n";
		cout << "RMS " << RMS << "\n";
		cout << "x = \n";
		cout << x1[0]<<"\t"<<x1[1]<<"\t"<<x1[2]<<endl;
	}
	else
	{
		double error = 1, tol = 1e-5;
		while(error>tol)
		{
			sum = 0;
			for(i=0; i<n; i++)
			{
				int xi = 0;
				for(j=0; j<n; j++)
				{
					if(i != j)
					{
						xi+= a[i][j]*x1[j];
					}
				}
				x1[i] = (b[i] - xi)/a[i][i];
				sum+= pow(x1[i]-xx[i],2);
				xx[i] = x1[i];
			}
			error = sqrt(sum/n);
			iter += 1;
		}
		cout << "iteration " << iter << "\n";
		cout << "tolerance " << tol << "\n";
		cout << "x = \n";
		cout << x1[0]<<"\t"<<x1[1]<<"\t"<<x1[2]<<endl;
	}
	double tf = clock();
	cout << "Executing time: " << (tf-ti)*1000.0/CLOCKS_PER_SEC << endl;
}

//Gauss_Seidel method
void Gauss_Seidel(int n, double a[][N], double b[N])
{
	double ti = clock();
	double v[n], y[n], sum = 0, RMS;
	double xx[N] = {0,0,0,0,0};
	double x1[N] = {1,1,1,1,1};
	int i = 0,k = 0,j = 0;
	int M, iter = 0;
	string s;
	cout << "The number of iteration: ";
	getline(cin,s);
	if(!s.empty())
	{
		RMS = sqrt(sum / n);
		stringstream(s) >> M;
		for(k=0; k<M; k++)
		{
			for(i=0; i<n; i++)
			{
				y[i] = b[i]/a[i][i];
				for(j=0; j<n; j++)
				{
					if(i != j)
					{
						y[i]-= (a[i][j]/a[i][i])*x1[j];
						x1[i] = y[i];
					}
				}
				if(k==M-2)
				{
					v[i] = x1[i];
				}
				if(k==M-1)
				{
					sum+= pow(x1[i]-v[i],2);
				}
			}
		}
		cout << "iteration " << M << "\n";
		cout << "RMS " << RMS << "\n";
		cout << "x = \n";
		cout << x1[0]<<"\t"<<x1[1]<<"\t"<<x1[2]<<endl;
	}
	else
	{
		double error = 1, tol = 1e-5;
		while(error>tol)
		{
			sum = 0;
			for(i=0; i<n; i++)
			{
				y[i] = b[i]/a[i][i];
				for(j=0; j<n; j++)
				{
					if(i != j)
					{
						y[i] -= (a[i][j]/a[i][i])*x1[j];
						x1[i] = y[i];
					}
				}
				sum+= pow(x1[i]-xx[i],2);
				xx[i] = x1[i];
			}
			error = sqrt(sum/n);
			iter+= 1;
		}
		cout << "iteration " << iter << "\n";
		cout << "tolerance " << tol << "\n";
		cout << "x = \n";
		cout << x1[0]<<"\t"<<x1[1]<<"\t"<<x1[2]<<endl;
	}
	double tf = clock();
	cout << "executing time: " << (tf-ti)*1000.0/CLOCKS_PER_SEC << endl;
}

int main()
{
	double A[N][N], b[N];
	for(int i=0; i<N; i++)
	{
		int j = 0;
		while( j<N)
		{
			if(j == 0)
			{
				A[i][j] = 1;
			}
			else
			{
				A[i][j] = 0;
			}
			j++;
		}
		b[i] = 1.0;
	}
	Jacobi(N,A,b);
	Gauss_Seidel(N,A,b);
}

