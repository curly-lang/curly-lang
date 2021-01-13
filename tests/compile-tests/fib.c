void printf(char*, ...);

int fib(int n)
{
	if (n < 2)
		return n;
	else
		return fib(n - 1) + fib(n - 2);
}

int main()
{
	printf("%i\n", fib(40));
}
