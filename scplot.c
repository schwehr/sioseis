/*    scplot.c     entry points scopen, scplot, scclose
   for creating Sun screen seismic plots.  Seismic, because it starts
at the upper right corner, scplot plots down (top to bottom, with
successive calls to scplot to the left. (corresponding to the
direction the Versatec plotter does things).  SCPLOT uses the whole
screen without regard for windows or icons!
     The Sun command "screendump" may be used to save the whole screen
in a file that can then be replotted on the screen with "screenload"
or sent to a printer using "lpr -v" (lpr -v doesn't work on my machine).
     The following info is really meant to be my notes.
1)  The rasterfile.h file is order left to right whereas seismic files
are top to bottom.  Perhaps using bit-fields would be a way to invert
this gigantic bitmap.
2)  check out splot.c for the window attempt using sunview.
  a)  It always creates a new window - How do you open the existing
      window?  In order to get a canvas, you need a window fd, which
      can be obtained by window_create.
  b)  It is slower than molasses because I did the same thing as here
      in pixrect - use put so the plot goes top to bottom without
an intermediate image in memory.  Using mem_pr in essence gets around
the inversion.
3)  I can't find any bitmap calls in SunCore.
4)  I can't make sense out of the BitBlt stuff in SunCGI.
5)  Using the /usr/demo/bounce as a guide might be the way, but the
source code listing in the manual is different from what is actually is!

   Paul Henkart,  12 July 1989
*/

#include <pixrect/pixrect_hs.h>
#include <stdio.h>

	struct	pixrect *pr;
	int	xorig;   /* 0,0 is top left, 1152,0 is top right */
	int	yorig;  /* my console area starts at 90 */

scopen_( xo, yo )
	int	*xo;
	int	*yo;
{
	if( (pr = pr_open("/dev/fb")) == NULL ){
		fprintf( stderr, "Unable to create pixrect.\n");
		exit(1);
	}
	xorig = *xo;
	yorig = *yo;
	return;
}

scplot_( buf, nwrds )
	short	*buf;
	int	*nwrds;
{
	static	x = 0;
	int	i, j, y, value;

	y = 0;
	for( i = 0; i < *nwrds; i++,buf++ ){
		for( j = 15; j>=0; j--){
			value = ( *buf >> j & 1 );
			pr_put( pr, xorig - x, yorig + y, value);
			y++;
		}
	}
	x++;
	return;
}

scclose_()
{
	pr_close(pr);
	return;
}
