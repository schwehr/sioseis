#include <stdio.h>
#include <sys/types.h>
/*   OSX 10.5.8
#include <machine/types.h>
#include <sys/_types.h>
"#include /usr/include/sys/_types.h"
*/
	int64_t	qq,r;
static	int64_t q=10737418230;
	int32_t s;

	void testc_( a, b )
	int64_t	*a;
	int32_t *b;
{
	r = *a;
	s = *b;
	printf("*a= %d, %x    r= %d, %x\n",*a,*a,b,b);
	printf("*b= %d, %x    s= %d, %x\n",*b,*b,s,s);
	printf("q= %d, %x\n",q,q);
	qq = r / 10;
	printf("qq= %d, %x\n",qq,qq);
}
