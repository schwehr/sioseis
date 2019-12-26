#include <stdio.h>
#include <sys/types.h>

static int64_t q = 10737418230;
int64_t	qq;
int64_t	r;
int32_t s;

void testc_(const int64_t *a, const int32_t *b) {
    r = *a;
    s = *b;
    printf("*a= %d, %x    r= %d, %x\n", *a, *a, b, b);
    printf("*b= %d, %x    s= %d, %x\n", *b, *b, s, s);
    printf("q= %d, %x\n", q, q);
    qq = r / 10;
    printf("qq= %d, %x\n", qq, qq);
}
