#include <stdio.h>
#include <sys/types.h>
/*   OSX 10.5.8
#include <machine/types.h>
#include <sys/_types.h>
"#include /usr/include/sys/_types.h"
*/
int	main( void )
{
	off_t	a;
	printf("off_t = %d\n",sizeof(a));
/*	quad_t	aa;
	printf("quad_t = %d\n",sizeof(aa));
*/
	int8_t		z;
	u_int8_t	zz;
	printf("int8 = %d, u_int8 = %d\n",sizeof(z),sizeof(zz));
	int16_t		zzz;
	u_int16_t	zzzz;
	short	c;
	printf("short = %d int16 = %d, u_int16 = %d\n",sizeof(c),sizeof(zzz),sizeof(zzzz));
	int	b;
	printf("int = %d\n",sizeof(b));
	int32_t	bb;
	printf("int32_t = %d\n",sizeof(bb));
	u_int32_t bbb;
	printf("u_int32_t = %d\n",sizeof(bbb));
	long	d;
	printf("long = %d\n",sizeof(d));
	long long	e;
	printf("long long = %d\n",sizeof(e));
	int64_t	f;
	printf("int64_t = %d\n",sizeof(f));
	float	g;
	printf("float = %d\n",sizeof(g));
	double	h;
	printf("double = %d\n",sizeof(h));
}
/*
Linux pahoa.soest.hawaii.edu 2.6.18-164.6.1.el5.centos.plus #1 SMP Wed Nov 4 09:31:39 EST 2009 x86_64 x86_64 x86_64 GNU/Linux
off_t = 8
quad_t = 8
int = 4
int32_t = 4
short = 2
long = 8
long long = 8
int64_t = 8

Darwin paul-henkarts-macbook-pro-156.local 9.8.0 Darwin Kernel Version 9.8.0: Wed Jul 15 16:55:01 PDT 2009; root:xnu-1228.15.4~1/RELEASE_I386 i386
off_t = 8
quad_t = 8
int = 4
int32_t = 4
short = 2
long = 4
long long = 8
int64_t = 8

CYGWIN_NT-6.1-WOW64 sioseis-Dell 1.7.17(0.262/5/3) 2012-10-19 14:39 i686 Cygwin
off_t = 8
*****    quad_t doesn't exist     *****
int8 = 1, u_int8 = 1
short = 2 int16 = 2, u_int16 = 2
int = 4
int32_t = 4
u_int32_t = 4
long = 4
long long = 8
int64_t = 8
float = 4
double = 8


*/
