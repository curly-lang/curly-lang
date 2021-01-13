void printf(char*, ...);

long long fib(long long n)
{
	if (n < 2)
		return n;
	else
		return fib(n - 1) + fib(n - 2);
}

int main()
{
	printf("%lli\n", fib(40));
}
