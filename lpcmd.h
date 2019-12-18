/*	@(#)lpcmd.h 1.3 86/11/21 SMI	*/

/*
 * VERSATEC, INC., SANTA CLARA, CALIFORNIA  95051
 * A XEROX COMPANY
 *
 * Copyright (c) 1987 by Xerox Corporation.  All rights reserved.
 */
 
/* 
 * Copyright (c) 1986 by Sun Microsystems, Inc. 
 */

#ifndef _IOCTL_
#include <sys/ioctl.h>
#endif

/*
 * There are three generic-type ioctls:
 */

#define LPCOMMAND	_IOW(v, 3, int)	/* see commands, below */
#define LPSETVERSATEC	_IO(v,4)	/* use the versatec port */
#define LPSETCENTRONICS	_IO(v,5)	/* use the centronics port */

/*
 * The following are all generic commands that may be passed with the
 * LPCOMMAND ioctl.  Not all printer drivers necessarily support all
 * of these commands (in fact, they probably don't support a lot of
 * them).  However, these should be all the commands a parallel
 * printer should ever want (we'll see).  They are converted into
 * the real commands in the drivers through a table.
 *
 * First, the latched register commands.  These are all soft
 * bits - not the same bit positions as any hardware.  This first 
 * set of commands will be remembered and the port will stay in the
 * requested state until explicitly changed via another ioctl.  For
 * every * mode to be put in, there is an opposite ioctl to take it out.
 * EXCEPTION: It can be in both LPC_VPLT and LPC_VSPP mode
 * simultaneously (but not at the same time as LPC_PRNT).  Got that?
 */

#define LPC_LRST	0x00001	/* assert option INPUT PRIME- signal */
#define LPC_DRST	0x00002	/* deassert option INPUT PRIME- signal */

#define	LPC_SOPT	0x00004	/* select option port */
#define LPC_SVPT	0x00008	/* select versatec */

#define	LPC_DSTR	0x00010	/* enable datastreaming */
#define LPC_DDST	0x00020	/* disable datastreaming */

#define	LPC_VSPP	0x00040	/* enable Versatec spp mode */
#define LPC_DSPP	0x00080	/* disable Versatec spp mode */

#define LPC_VPLT	0x00100	/* enable Versatec plot mode */
#define LPC_DVPT	0x00200	/* disable Versatec plot mode */

#define LPC_PRNT	0x00400	/* normal Versatec print mode */

#define LPC_LATCHMASK	0x00777	/* all of the latched bits */
#define LPC_PRINTMASK	0x007c0	/* just the print mode bits */
/*
 * Next comes the commands that are just sent out once and then
 * forgotten about.  These are known as the PULSED register commands
 * on the VMEbus IKON board.  (Once again, these are just soft
 * bits - they don't match the same hard bits in any registers
 * on any board).
 */

#define LPC_SACK	0x00800	/* software ack */
#define LPC_MCLR	0x01000	/* master clear */
#define LPC_VCLR	0x02000	/* Versatec clear */
#define LPC_VTFF	0x04000	/* Versatec form feed */
#define LPC_VEOT	0x08000	/* Versatec EOT */
#define LPC_VLTR	0x10000	/* Versatec line terminate */

#define LPC_PULSEMASK	0x1f800	/* just the pulsed bits */

#define LPC_ALLVALID	0x1ffff	/* all valid command bits */

