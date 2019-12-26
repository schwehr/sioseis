// convert a 3 plane (red, green, blue) plot to an eight bit pixel plot

void planes_to_pixels(unsigned char *r, unsigned char *g, unsigned char *b, unsigned char *pixels, int npixels)
{
    for (int i = 0; i < npixels; i = i + 8) {
        for (int j = 0; j < 8; j++) {
            const unsigned char red = (*r >> 7-j) & 1;
            const unsigned char green = (*g >> 7-j) & 1;
            const unsigned char blue = (*b >> 7-j) & 1;
            *pixels = red | green << 1 | (blue << 2);
            pixels++;
        }
        r++;
        g++;
        b++;
    }
}
