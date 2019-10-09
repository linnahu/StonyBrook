#include <iostream>
#include <cmath>

using namespace std;
int main()
{
	int N;
	cout << "Input the interval:" << endl;
	cin >> N;

	int i;
	int D;
	D = 1 / N;

	int x[N];
	for (i = 1;i <= N;i++)
	{
		x[i] = i*D;
	}

	int fx[N];
	for (i = 1;i <= N;i++)
	{
		fx[i] = sqrt(1 - x[i] * x[i])
	}
	
	int Pi = 0;
	for (i = 1;i < N;i++)
	{
		Pi = Pi + 2 * D*(fx[i - 1] + fx[i]);
	}
	cout << "The pi is " << Pi << endl;
	cout << "The error is " << abs(Pi - M_PI) << endl;
	return 0;
}

