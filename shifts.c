/*   rshift is right shift with vacated bits set to 0.  arg 1 is short, arg 2 is int, return is int
     lrshift is right shift with vacated bits set to 0.  arg 1 is long, arg 2 is int, return is int
     lshift is left shift with vacated bits set to 0.  arg 1 is short, arg 2 is int, return is int
     llshift is left shift with vacated bits set to 0.  arg 1 is long, arg 2 is int, return is int
*/
/*   mod 27 Jan 2013 to zero fill the vacated bits on right shift.  pch
     mod 28 Jan 2013 change lshift and rshift to lshift_sio and rshift_sio since gfortran
                     now has lshift and rshift.  rshift doesn't set vacated bits to 0 and
                     some sioseis need them set to 0! (ibm2fp.f)
*/
#include <stdio.h>

int lshift_sio_(short *i, int *n) {
    return *i << *n;
}


int rshift_sio_(short *i, int *n) {
    // REMEMBER THAT C standard leaves vacated bit undefined
    static const short x[15] = {32767, 16383, 8191, 4095, 2047, 1023, 511, 255, 125, 63, 31, 15, 7, 3, 1};

    const int c = *i;
    const int d = *n;
    const int d1 = *n -1;
    if (d == 0) return *i;
    const int b = c >> d;
    const int a = b & x[d1];
    return a;
}


int llshift_(long *i, int *n) {
    return *i << *n;
}


int lrshift_(long *i, int *n) {
    // REMEMBER THAT C standard leaves vacated bit undefined
    // int a,b,c,d,d1;
    static const long x[31] = {
        2147483647, 107371823, 536870911, 268435455,
        134217727, 67108863, 33554431, 16777215,
        8388607, 4194303, 2097151, 1048575,
        524287, 262143, 131071, 65535,
        32767, 16383, 8191, 4095,
        2047, 1023, 511, 255,
        125, 63, 31, 15,
        7, 3, 1};
    const int c = *i;
    const int d = *n;
    const int d1 = *n -1;
    if (d == 0) return *i;
    const int b = c >> d;
    const int a = b & x[d1];
    return a;
}
