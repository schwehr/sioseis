#include <stdio.h>
/*void tlann_( ibuf, first_bit, nwrds, vscale, nibs, tline, start_time, 
			nsecs, ncolors, idir, isig )*/
void tlann_( ibuf, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12 )
	unsigned char ibuf[];
	int	*arg2;	/* first_bit; */
	int	*arg3;	/* nwrds; */
	float	*arg4;	/* vscale; */
	float	*arg5;	/* nibs; */
	float	*arg6;	/* tline; */
	float	*arg7;	/* start_time; */
	float	*arg8;	/* nsecs; */
	int		*arg9;	/* ncolors */
	int		*arg10;	/* direction */
	int		*arg11;	/* special signal */
	int		*arg12; /* 0=no decimal, 1=decimal */

/* tlann - time line annotation.
*  Paul Henkart,  Scripps Oceanography,  February 1995
*  (C) 1995, Regents of The University of California
*
* mod 31 March 95 to remove tline from last_bit =  
* mod August 2006 - add dir < 0 annotation
* 9 May 08 - Use abs value so negative time doesn't do something weird.
* 22 Aug 08 - add special signal.  If == 1, then only do 3 digits of the first one.
* 15 Oct 10 - If ltr and > 10, then shift by 1 character
*/

{
	int		last_bit, byte, digit, byte_inc, i, nlines, index, digits[6];
	float	time, temp;
	int		first_bit, nwrds, ndigits, ncolors, idir, this, isig, idecimal;
	float	vscale, nibs, tline, start_time, nsecs;


	first_bit = *arg2;
	nwrds = *arg3;
	vscale = *arg4;
	nibs = *arg5;
	tline = *arg6;
	start_time = *arg7;
	nsecs = *arg8;
	ncolors = *arg9;
	idir = *arg10;
	isig = *arg11;
	idecimal = *arg12;
	byte_inc = nwrds + nwrds;
     last_bit = (int)(vscale * nibs * nsecs) + first_bit;
	time = start_time;
     if( idir > 0 ) {
		while( first_bit <= last_bit ) {
			if( nibs < 150 ) byte = (int)((float)first_bit / 8. + .5) - 3;
			else if( nibs < 250 ) byte = (int)((float)first_bit / 8. + .5) - 4;
			else byte = (int)((float)first_bit / 8. + .5) - 5;
			index = byte;
			if( time < 10. ) {
				ndigits = 5;
				temp = time;
			}
			else {
				ndigits = 6;
				temp = time / 10.;
			}
			for( i=1; i<ndigits; i++ ) {
				if( (isig == 1) && (i == ndigits-2) ) i++;
				if( temp < 0 ) temp = -temp;
				digit = temp + .00005;
				temp = (temp - (float)digit) * 10.;
				nlines = sideann( digit, &ibuf[index], byte_inc, nibs,
						i, ncolors, idir );
				index = index + nlines * byte_inc;
				if( (i == ndigits-4) && (idecimal == 1) ) {	/* put a decimal point in */
/*  11 is a period */
			    	nlines = sideann( 11, &ibuf[index], byte_inc, nibs,
						0, ncolors, idir );
			    	index = index + nlines*2/3 * byte_inc;
				}
			}
/*  10  is a blank  */
			nlines = sideann( 10, &ibuf[index], byte_inc, nibs, i,
					ncolors, idir );
			first_bit = first_bit + tline * vscale * nibs;
			if( isig == 1 ) first_bit = last_bit + 1;
			time = time + tline;
		}
	}else {
		this = first_bit;
		while( this < last_bit ) this = this + tline * vscale * nibs;
		this = last_bit;
		while( this >= first_bit ) {
			if( nibs < 150 ) byte = (int)((float)this / 8. + .5) - 3;
			else if( nibs < 250 ) byte = (int)((float)this / 8. + .5) - 4;
			else byte = (int)((float)this / 8. + .5) - 5;
			index = byte;
			if( time < 10. ) {
				ndigits = 5;
				temp = time;
			} else {
				ndigits = 6; 
				temp = time / 10.;
			}
			if( temp < 0 ) temp = -temp;
			for( i=0; i<ndigits; i++ ) {
				digit = temp + .00005;
				digits[i] = digit;
				temp = (temp - (float)digit) * 10.;
			}
			for( i=ndigits-2; i>=0; i-- ) { 
				if( ndigits == 5 && isig == 1 && i == ndigits-2 ) i--;
				if( ndigits == 6 && i == ndigits-3 ) i--;
				nlines = sideann( digits[i], &ibuf[index], byte_inc, nibs,
						1, ncolors, idir );
				index = index + nlines * byte_inc;
				if( ndigits == 5 && i == ndigits-4 && idecimal == 1 ) {	/* put a decimal point in */
/*  11 is a period */
				    	nlines = sideann( 11, &ibuf[index], byte_inc, nibs,
							0, ncolors, idir );
			    		index = index + nlines*2/3 * byte_inc;
				}
				if( ndigits == 6 && i == 2 && idecimal == 1 ) {	/* put a decimal point in */
/*  11 is a period */
				    	nlines = sideann( 11, &ibuf[index], byte_inc, nibs,
							0, ncolors, idir );
			    		index = index + nlines*2/3 * byte_inc;
				}
			}
/*  10  is a blank  */
			nlines = sideann( 10, &ibuf[index], byte_inc, nibs, 1,
					ncolors, idir );
			this = this - tline * vscale * nibs;
			time = time + tline;
			if( isig == 1 ) this = first_bit - 1;
		}
	}
	return;
}
