#include <stdio.h>
#include <stdlib.h>    /* Add declaration for Malloc ajh */


/* Function that calculates the depth of each channel based on the bird depth info
   from trace 0.  Section 13 of trace 0 (digicon data block) should be passed as
   the first argument(*ddblock).
   Needed info that the calling routine must provide:
   ddblock:  char array         Section 13 of trace 0
   boffset:  float array        offsets of the birds, starting at bird nearest the ship
   nboff:    integer            number of values the above array holds
   coffset:  float array        channel offsets, starting at nearest channel to ship
   nchan:    integer            number of channels
   chan:     float array        this is where the output will go.  The channel depths
                                will be calculated and put into this array.
*/

/* This function calls two additional functions: linterp, which does linear interpolation
   and returns a float value, and locate, which locates a value in an array of values.

mod feb 97 to add compass extraction
mod sep 00 to add message when more birds were recorded than were defined
*/

int getdep_(ddblock,boffset,nboff1,coffset,nchan1,chan,ncomps,cunitno,creading)
char ddblock[];
float *coffset,*boffset, *chan, *creading;
int *nchan1, *nboff1, *ncomps, *cunitno;
/*  0,1 = section id (int)
    2,3 = size of section (int)
    4,5,6,7,8,9,10,11 = hh:mm:ss in ascii
    12,13,14,15,16 = event number mod 32700
    17,18 = number of compasses in ASCII
*/
{
  int nchan,nboff;
  int i,j,ibyte,lastbyte,ndeps,nbirds,ncomp,itemp,size,status;
  char substr[5];            /* define a 2 byte and a 4 byte sub-string */
  float *bdep, *offsets;
  float linterp();
  void locate(),move();

  nchan = *nchan1;
  nboff = *nboff1;
  move(&ddblock[17],&substr,2); substr[2] = '\0';
  ncomp = atoi(substr);    /* number of compasses */
  *ncomps = ncomp;
  if( ncomp < 0 || ncomp > 200 ) return(-1);
  ibyte = 19;     /* index byte for start of compasses */
  j = 0;
  for (i=0;i<ncomp;++i) {
     if( ddblock[ibyte+j] == 'C' ) {
  	    move(&ddblock[ibyte+j+1],&substr,2); substr[2] = '\0';
	    cunitno[i] = atoi(substr);
  	    move(&ddblock[ibyte+j+3],&substr,4); substr[4] = '\0';
         itemp = atoi(substr);
         creading[i] = (float) itemp / 10.;
     }
     j = j + 7;
  }

  ibyte = 19 + 7*ncomp;    /* index byte for start of depth sensors */
  move(&ddblock[ibyte],&substr,2); substr[2] = '\0';
  ndeps = atoi(substr);    /* number of depth sensors */

  ibyte = 21 + 7*ncomp + 12*ndeps;                        /* index byte for start of birds */
  move(&ddblock[ibyte],&substr,2); substr[2] = '\0';
  nbirds = atoi(substr);                                   /* number of birds */

  status = 0;

  if (nbirds > nboff) {
        if( nboff == 0 ) return(status);
	printf("***  WARNING ***  %d birds were recorded, but only %d were defined.\n",nbirds,nboff);
  }

  bdep = (float *) malloc(nbirds*sizeof(float));

  lastbyte = 23 + 7*ncomp + 12*ndeps + 16*nbirds;

  /* grab the depth of each bird */
  for (i=0;i<nbirds;++i) {
    move(&ddblock[ibyte+6+i*16],&substr,4); substr[4] = '\0';

    itemp = atoi(substr);
    bdep[i] = (float) itemp/100.;                           /* bird depth in meters */
/*  assume a 0 depth is bad, then use the previous  pch sep 2000   */
    if( bdep[i] == 0 ) bdep[i] = bdep[i-1];
 /*    fprintf(stdout,"%g\t%g\n",boffset[i],bdep[i]);*/
  }

  /* this is the place to calculate depths of the channels based on bird depths */
  /* if there are channels before the first bird or after the last bird */
  /* this function will not work.  it will probably core dump. */

  for(j=0;j<nchan;++j)
      chan[j] = linterp(boffset,bdep,nboff,coffset[j]);

return(status);
  /* if not totally screwed up, the array chandep now contains the depths of each channel */
  /* chandep[0] == channel 1 depth, chandep[n-1] == channel n depth */

}

float linterp (x,y,ndin,xint)
int ndin;
float *x,*y,xint;
/*
   linear interpolation, just about as basic as you want get.
   given a function y(x), finds y for any desired x value which
   is specified by xint.  ndin is the number of data points in
   the x array
*/
/* x is indexed from 0 to ndin-1 */
{
  float dx,yint;
  int i,j;
  void locate();

  locate(x-1,ndin,xint,&j);
  j -= 1;
  dx = x[j+1] - x[j];
  yint = (y[j]*(x[j+1] - xint)/dx) + (y[j+1]*(xint - x[j])/dx);
  return(yint);
}

void locate(xx,n,x,j)
float xx[],x;
int n,*j;
{
  /* This routine is out of numerical recipes.  Slightly modified to handle the case
     where the desired interpolated value lands on the first point in the x array  */

  int ascnd,ju,jm,jl;

	jl=0;
	ju=n+1;
	ascnd=xx[n] > xx[1];
	if (x == xx[1]) jl=1;
	else {
	   while (ju-jl > 1) {
		   jm=(ju+jl) >> 1;
		   if (x > xx[jm] == ascnd)
		   	jl=jm;
		   else
			ju=jm;
	   }
	}
	*j=jl;
}

 void move(from,to,count)
 char *from;
 char *to;
 int count;
 {
   while(count--) *to++ = *from++;
 }
