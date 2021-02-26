#include <stdio.h>

long long divide(long long x, long long y)
{
    long long result = x / y;
    printf("%lli / %lli = %lli\n", x, y, result);
    return result;
}
