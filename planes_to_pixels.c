/*  convert a 3 plane (red, green, blue) plot to an eight bit pixel plot
*/

void planes_to_pixels( r, g, b, pixels, npixels )
	unsigned char	*r, *g, *b, *pixels;
	int	 npixels;


{
	int	i, j;
	unsigned char  red, green, blue;

	for ( i=0; i<npixels; i=i+8 ){
		for ( j=0; j<8; j++ ) {
			red = (*r >> 7-j) & 1;
			green = (*g >> 7-j) & 1;
			blue = (*b >> 7-j) & 1;
			*pixels = red | green << 1 | (blue << 2);
			pixels++;
		}
		r++; g++; b++;
	}
}
