/*	bytes_per_line_sio = get_nbytes_sio( nibs )
*	return the number of bytes per sioseis plot line, given the
*	sioseis nibs parameter on line 1 of the plot file.
*
*  Paul Henkart,  Scripps Oceanography,  November 1994
*  (C) 1994, Regents of The University of California
*
*  mod 14 Jun 10 - Add 2144 and 2124
*  mod 2 Mar 11 - Add 624 and 7225
*/

#include <stdio.h>

int	get_nbytes_sio( nibs )
	int	nibs;

{
	int	bytes_per_line_sio;
	switch ( nibs ) {
		case 75:
			bytes_per_line_sio = 400; 	/* 200/2 * 4 */
			break;
		case 100:
			bytes_per_line_sio = 164; 	/* 41 * 4 */
			break;
		case 200:
			bytes_per_line_sio = 264; 	/* 66 * 4 */
			break;
		case 624:
			bytes_per_line_sio = 1184; 	/* 592 * 2 */
			break;
		case 2124:
			bytes_per_line_sio = 2024; 	/* 1012 * 2 */
			break;
		case 2144:
			bytes_per_line_sio = 3252; 	/* 1626 * 2 */
			break;
		case 2368:
			bytes_per_line_sio = 296; 	/* 74 * 4 */
			break;
		case 2847:
		case 2858:
			bytes_per_line_sio = 900; 	/* 225 * 4 */
			break;
		case 2848:
		case 2859:
			bytes_per_line_sio = 1312; 	/* 328 * 4 */
			break;
		case 7222:
		case 8222:
			bytes_per_line_sio = 528; 	/* 132 * 4 */
			break;
		case 3444:
		case 7444:
			bytes_per_line_sio = 2152; 	/* 538 * 4 */
			break;
		case 7224:
			bytes_per_line_sio = 576; 	/* 144 * 4 */
			break;
		case 5732:
		case 7225:
			bytes_per_line_sio = 588; 	/* 147 * 4 */
			break;
		case 7422:
			bytes_per_line_sio = 1056; 	/* 264 * 4 */
			break;
		case 7424:
			bytes_per_line_sio = 1152; 	/* 288 * 4 */
			break;
		case 7436:
			bytes_per_line_sio = 1760; 	/* 440 * 4 */
			break;
		case 8936:
			bytes_per_line_sio = 1712; 	/* 428 * 4 */
			break;
		default:
			fprintf( stderr, "Can not handle nibs %d\n",nibs);
			exit(0);
	}
	return ( bytes_per_line_sio );
}
