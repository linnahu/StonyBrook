#include <iostream>
#include <cmath>

using namespace std;
int main()
{
	int error;
	cout << "input the maximum acceptable error : " << endl;
	cin >> error;

	int N = 1;
	int D;
	D = 1 / N;

	do
	{
		int i;
		int x[N];
		for (i = 1; i <= N; i++) 
		{
			x[i] = i*D;
		}
		
		int fx[N];
		for (i = 1; i <= N; i++) 
		{
			fx[i] = sqrt(1 - x[i] * x[i]);
		}
		
		int Pi = 0;
		for (i = 1; i < N; i++) 
		{
			Pi = Pi + 2 * D*(fx[i - 1] + fx[i]);
		}
		N++;
		while (abs(Pi - M_PI) > error);
	} 
	

	cout << "The pi is " << Pi << endl;
	cout << "The error is " << abs(Pi - M_PI) << endl;
	cout << "The number of Intervals is " << N << endl;
	return 0;
}
