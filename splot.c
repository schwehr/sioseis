#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <frame.h>
#include <stdio.h>

	Frame	frame;
	Canvas	canvas;
	int	xorig = 0;   /* 0,0 is top left, 1152,0 is top right */
	int	yorig = 0;  /* my console area starts at 90 */
	static	Pixwin	*pw;
	static	int	width, height;
	static void my_event_proc();

sopen_( )
{
	frame = window_create( 0, FRAME, FRAME_LABEL, "SIOSEIS screen plot", 0 );
	canvas = window_create(frame, CANVAS,
				CANVAS_AUTO_EXPAND,	0,
				CANVAS_AUTO_SHRINK,	0,
				CANVAS_AUTO_CLEAR,	0,
				0 );
	width = (int) window_get( canvas, CANVAS_WIDTH);
	height = (int) window_get( canvas, CANVAS_HEIGHT);
	printf("width=%d, height=%d\n",width,height);
	pw = canvas_pixwin(canvas);
	return;
}


splot_( buf, nwrds )
	short	*buf;
	int	*nwrds;
{
	static	x = 0;
	int	i, j, y, value;

	y = 0;
	if( width - x < 0 ) return;
	for( i = 0; i < *nwrds && yorig + y < height; i++,buf++ ){
		for( j = 15; j>=0; j--){
			value = ( *buf >> j & 1 );
			pw_put( pw, width - x, yorig + y, value);
			y++;
		}
	}
	x++;
	return;
}

sclose_()
{
	window_main_loop(frame);
	return;
}
