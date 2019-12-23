/*      sio2sun
    sioseis to sun rasterfile converter
        Usage:   sio2sun  inputfile outputfile

  Paul Henkart,  3 May 1993

mod Feb 1 2001, add gray scale
mod 30 July 2002 - figure out the filesize if the sioseis file didn't
                   get closed properly.
mod 11 Feb 2003 - Add endian so for PC byte swap
mod 15 Feb 2008 - Automate the endian by adding is_big_endian()
mod 2 Feb 2011 - change long to int32_t

*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/*  #include <rasterfile.h>   not on all systems, so do it ourselves! */
/*  /usr/include/rasterfile.h  */
#define   RAS_MAGIC 0x59a66a95
#define RT_OLD      0    /* Raw pixrect image in 68000 byte order */
#define RT_STANDARD 1    /* Raw pixrect image in 68000 byte order */
#define RT_BYTE_ENCODED  2    /* Run-length compression of bytes */
#define RT_FORMAT_RGB    3    /* XRGB or RGB instead of XBGR or BGR */
#define RT_FORMAT_TIFF   4    /* tiff <-> standard rasterfile */
#define RT_FORMAT_IFF    5    /* iff (TAAC format) <-> standard rasterfile */
#define RT_EXPERIMENTAL 0xffff     /* Reserved for testing */
     /* Sun registered ras_maptype's */
#define RMT_RAW          2
     /* Sun supported ras_maptype's */
#define RMT_NONE    0    /* ras_maplength is expected to be 0 */
#define RMT_EQUAL_RGB    1    /* red[ras_maplength/3],green[],blue[] */

	struct rasterfile {
		int32_t  ras_magic;
		int32_t  ras_width;
		int32_t  ras_height;
		int32_t  ras_depth;
		int32_t  ras_length;
		int32_t  ras_type;
		int32_t  ras_maptype;
		int32_t  ras_maplength;
	};
	void exit (int status);

main	(argc, argv)
	int	argc;
	char	*argv[];

{
	int	is_big_endian();
	struct	rasterfile	ras;
	FILE	*fin, *fout;
	int	dpi, bytes_per_line, nplanes, nwords, nlines, nbytes;
	int	endian, i, nread, istat, gray=0;
	long bpl;
	unsigned char	r[10000], g[10000], b[10000];
	unsigned char buf[100000], cms_rgbsetup[24];
	char	header[80];
	char	skip66[66];
	char grey;

	if( argc != 3 ) {
	    fprintf( stderr, "Usage:  sio2sun inputfile outputfile.\n");
	    exit( 1 );
	}
	if( ( fin = fopen( argv[1], "r")) == NULL ) {
	    fprintf( stderr, "***  error  ***  input file could not be opened.\n");
	    exit(1);
	}
	if( ( fout = fopen( argv[2], "w")) == NULL ) {
	    fprintf( stderr, "***  error  ***  output file could not be opened.\n");
	    exit(1);
	}
	endian = is_big_endian();
/*
	READ 44 lines of ASCII header
	The plotter type and file size is formatted at the end of line1
*/
	fscanf( fin, "%66c %d %d %d", &skip66, &dpi, &nwords, &nlines );
	grey = 'g';
     if( strncmp(&grey,&skip66[64],1) == 0 ) gray = 1;
/*  nwords is the number of 32 bit words that are useful in each line */
	nbytes = nwords * 4;
	bytes_per_line = get_nbytes_sio( dpi );
/*   sioseis write the number of raster lines in the header at the
        end of the sioseis job, so if the sioseis job doesn't close
     properly, it doesn't get written.  Figure it out now  */
     if( nlines == 0 ) {
/*   I give up.  fstat doesn't work
          istat = fstat( *argv[1], &stbuf );
          istat = fstat( fin, &stbuf );
          nlines = (stbuf.st_size - 44 *80) / bytes_per_line;
*/
          istat = fseek( fin, 44 * 80,  0 );
          bpl = bytes_per_line;
                i = 1;
          while ( i == 1 ) {
               istat = fread( buf, sizeof(char), bytes_per_line, fin );
               if( istat ==  bytes_per_line )
                    nlines++;
               else
                    i = 0;
          }
          nbytes = bytes_per_line;
          printf("SIOSEIS file was not closed properly - assuming a black and white plot.\n");
          printf("nlines should be %d, nbytes=%d\n",nlines,nbytes);
     }
	istat = fseek( fin, bytes_per_line * nlines *2,  0 );
	nplanes = fread( &istat, sizeof(char), 1, fin ) * 3;
	istat = fseek( fin, 44 * 80,  0 );

/*
	Set up and write the Sun rasterfile 8 word (16 bytes) header
*/
	ras.ras_magic = RAS_MAGIC;
	ras.ras_width = nbytes * 8;	/* width in pixels */
	ras.ras_height = nlines;	/* length or height in pixels */
	if( nplanes == 0 ) {
		ras.ras_depth = 1; /* number of bits per pixel*/
		ras.ras_length = nbytes * nlines; /* the data length in bytes */
		ras.ras_type = RT_STANDARD;
		ras.ras_maptype = RMT_NONE;		/* the type of color map */
		ras.ras_maplength = 0;
		if( endian < 0 ) istat = swap32( &ras, 8);
		istat = fwrite( &ras, sizeof(char), 32, fout );
		if( endian < 0 ) istat = swap32( &ras, 8);
	}
	else {
		ras.ras_depth = 8;
		ras.ras_length = nbytes * nlines * 8;
		ras.ras_type = RT_STANDARD;
		ras.ras_maptype = RMT_EQUAL_RGB;
		ras.ras_maplength = 768;	/* 768 = 256 * 3  */
		if( endian < 0 ) istat = swap32( &ras, 8);
		istat = fwrite( &ras, sizeof(char), 32, fout );
		if( endian < 0 ) istat = swap32( &ras, 8);
		for( i=0; i<256; i++ ) r[i] = 0;
		if( gray == 0 ) {
			cms_rgbsetup[0] = 0;
			cms_rgbsetup[1] = 255;
			cms_rgbsetup[2] = 0;
			cms_rgbsetup[3] = 255;
			cms_rgbsetup[4] = 0;
			cms_rgbsetup[5] = 255;
			cms_rgbsetup[6] = 0;
			cms_rgbsetup[7] = 255;
			istat = fwrite( &cms_rgbsetup[0], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
			cms_rgbsetup[8] = 0;
			cms_rgbsetup[9] = 0;
			cms_rgbsetup[10] = 255;
			cms_rgbsetup[11] = 255;
			cms_rgbsetup[12] = 0;
			cms_rgbsetup[13] = 0;
			cms_rgbsetup[14] = 255;
			cms_rgbsetup[15] = 255;
			istat = fwrite( &cms_rgbsetup[8], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
			cms_rgbsetup[16] = 0;
			cms_rgbsetup[17] = 0;
			cms_rgbsetup[18] = 0;
			cms_rgbsetup[19] = 0;
			cms_rgbsetup[20] = 255;
			cms_rgbsetup[21] = 255;
			cms_rgbsetup[22] = 255;
			cms_rgbsetup[23] = 255;
			istat = fwrite( &cms_rgbsetup[16], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
		} else {
/*
c    0    255 255 255         gray7     white
c    1    219 219 219         gray6
c    2    181 181 181         gray5
c    3    145 145 145         gray4
c    4    110 110 110         gray3
c    5     71  71  71         gray2
c    6     36  36  36         grey1
c    7      0   0   0         gray0     black
*/
			cms_rgbsetup[7] = 255;   /* white  */
			cms_rgbsetup[6] = 219;
			cms_rgbsetup[5] = 181;
			cms_rgbsetup[4] = 145;
			cms_rgbsetup[3] = 110;
			cms_rgbsetup[2] = 71;
			cms_rgbsetup[1] = 36;
			cms_rgbsetup[0] = 0;     /* black  */
			istat = fwrite( &cms_rgbsetup[0], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
			istat = fwrite( &cms_rgbsetup[0], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
			istat = fwrite( &cms_rgbsetup[0], sizeof(char), 8, fout );
			istat = fwrite( r, sizeof(char), 248, fout );
		}
	}
/*
	Copy the SIOSEIS rasterfile to the Sun rasterfile
*/
	for( i=0; i<nlines; i++ ) {
		if( nplanes == 0 ) {
			istat = fread( buf, sizeof(char), bytes_per_line, fin );
			if( istat != bytes_per_line ) {
				printf("input file terminated early, i= %d\n",i);
				exit( 1 );
			}
			istat = fwrite( buf, sizeof(char), nbytes, fout );
			if( istat != nbytes ) {
				printf("output file problem. %d\n",istat);
				exit( 1 );
			}
		} else {
			istat = fread( r, sizeof(char), bytes_per_line, fin );
			istat = fread( g, sizeof(char), bytes_per_line, fin );
			istat = fread( b, sizeof(char), bytes_per_line, fin );
			if( istat != bytes_per_line ) {
				printf("input file terminated early, i= %d\n",i);
				exit( 1 );
			}
			planes_to_pixels( r, g, b, buf, ras.ras_width );
			istat = fwrite( buf, sizeof(char), ras.ras_width, fout );
		}
	}
	fclose( fout );
	exit(0);
}
int is_big_endian()
{
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  return (u.c[sizeof (long) - 1] == 1 ? 1 : -1);
}

