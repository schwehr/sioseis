/*
	IKON Corporation 2617 Western Ave Seattle, WA 98121   (206) 728-6465

	ioctl definition file for Solaris 2.0 driver for Sbus hardcopy boards



	20 October, 1992	BETA release

	23 October, 1992	Moved DMA_MAXBLOCK define to ihcp_reg.h

	23 October, 1995	modified IHCPIO_CMD_MASK to drop the top 3 command bits
				and only mask for the 'magic character' and the
				actual command integer.  this is to retain compatibility
				with Versatec's solaris driver, which seems to have
				dropped the command size and direction bits from the
				encoded ioctl command.  it is now just the character
				and the command value.  this requires that all ioctls
				have unique character and integer values.  _IOW and _IOR
				may not share an integer value, or they will be decoded
				as the same ioctl command


*/



	/************************************************************************
	*									*
	*	This driver is provided at no charge to IKON's customers 	*
	*	in the hope that it will assist them in understanding and	*
	*	using IKON's Sbus hardcopy interface products.  This code	*
	*	is intended to be a working and (relatively!) bug free driver	*
	*	when running on the machine and OS rev available to IKON.	*
	*	IKON will attempt to keep this code running on current OS and	*
	*	hardware from SUN - and others - but does not guarantee this.	*
	*	The user is encouraged to contact IKON with comments,		*
	*	suggestions, and BUG REPORTS.					*
	************************************************************************/




/*	this file is also used inside driver code !!! don't mess with this unless you mean it!
*/

/*
IKON's hardcpopy boards for the Sbus include the following:

		Model 10104	Versatec Differential
		Model 10105	Versatec TTL
		Model 10106	Centronics

The three boards share a common register set, and all operate under
the same software driver.  Not all functions are supported by all
boards - the software will check to make sure that a requested operation
is supported for that particular board.
*/



#ifndef	_SYS_IOCCOM_H
#include <sys/ioccom.h>
#endif

#ifndef	TRUE
#define	TRUE	1
#define	FALSE	0
#endif

#ifndef	OK
#define	OK	0
#endif


/* the ioctl() function call looks like:

		 ioctl(filedescriptor,command,argument)

argument is used in some of the ihcp ioctl calls to provide values to
the driver ioctl routine, or return values to the calling program.  In
one case - data out - it contains an array of character data to be sent
to the attached device.  the argument is restricted to a maximum of 255
bytes - by unix, and by the driver.

the following ioctl commands
are available to programs using the ihcp driver:

IHCPIO_DEV_RESET	reset attached device
IHCPIO_SET_CONFIG	select device timing and busy mode w/ value in arg
IHCPIO_SET_DMATIME	set dma timeout to arg seconds
IHCPIO_SET_RDYTIME	set fifo <1/2 full and fifo empty+device ready
			timeout to arg seconds
IHCPIO_GET_REGS		return board's registers in arg
IHCPIO_GET_STATUS	return device status in arg
IHCPIO_GET_BOARD	return board type in arg
IHCPIO_SET_VMODE	set versatec print/plot/spp mode using value in arg
IHCPIO_SET_VMODEX	same as VMODE, but clear ready sync ff - most plotters
			will use VMODE - both for versatec only
IHCPIO_V_CMD		send pulse command to plotter using value in arg
			(versatec only)
IHCPIO_DATA_OUT		send data bytes from arg array to device.  byte count
			in IHCPIO_COUNT_MASK portion of cmd
IHCPIO_RDY_WAIT		wait for fifo empty and device ready
IHCPIO_HALF_WAIT	wait for fifo less than 1/2 full
IHCPIO_STREAM_ON	select data streaming mode (centronics only)
IHCPIO_STREAM_OFF	clear data streaming mode
IHCPIO_GET_FLAGS	returns unit_flags  in arg
IHCPIO_GET_FIFO		returns fifo status in arg
IHCPIO_MASTER_CLEAR	resets board and clears fifo FACTORY USE ONLY!
IHCPIO_SOFT_ACK		simulates ack sequence from device

*/



/* the ioctl command codes conform to the unix pattern:

	the top 3 bits of the 32 bit value indicate whether arguments
	are to be copied in, copied out, both, or neither.

	0x80000000 = copy in
	0x40000000 = copy out
	0x20000000 = no argument transfer
	0xC0000000 = copy in and out

	the number of bytes in the argument is encoded in the lower
	8 bits of the upper half of the u_int, and the actual command
	is encoded in the lower half.  a rather arbitrary character,
	which is intended to identify the driver, is also encoded in
	the lower half of the command.  it becomes part of the command
	value.
	for all commands except DATA_OUT, the arg length is part of
	the command value.

	WHEN USINT THE IHCP_DATA_OUT IOCTL,
	THE CALLING PROGRAM IS REQUIRED TO COMBINE THE ARGUMENT LENGTH
	WITH THE IOCTL COMMAND AS FOLLOWS:

	cmd = IHCP_DATA_OUT | ( ( arg_length & 0xFF ) << 16 ) ;

	commands which require arguments - in or out - will pass those
	values as 32 bit unsigned integers.  the only exception is the
	DATA_OUT ioctl, which uses an array of up to 255 bytes as its
	argument.

	the magic character that identifies this driver is hereby
	(arbitrarily) chosen to be 'H'.

	the following ioctl commands are defined using pre-existing
	ioctl command macros.  the CMD_MASK and COUNT_MASK values
	defined MUST match the usage in ioccom.h. refer to that include
	file for further information.

	ADDITIONAL IOCTL COMMANDS AND ARGUMENTS ARE FOUND AT THE END OF THIS
	FILE.  THEY ARE INCLUDED FOR COMPATIBILITY WITH SUN AND VERSATEC
	DRIVERS FOR THE 10088 VME BOARD, AND WILL HOPEFULLY BE COMPATIBLE
	WITH OTHER MANUFACTURERS BOARDS AND DRIVERS (SHOULD THERE BE ANY!)

	the "magic character" for these commands is "v"
*/



#define IHCPIO_CMD_MASK 0x0000FFFF	/* cmd in top 3 and bottom 16 bits */
					/* only the bottom 16 bits are     */
					/* used in this version            */

#define IHCPIO_COUNT_MASK 0x00FF0000	/* arg byte count here		   */


/*
	THE SOLARIS 2.0 MACROS IN ioccom.h WANT THE 'MAGIC LETTER' QUOTED.
	THE SOLARIS 1.X MACROS WANTED IT WITHOUT QUOTES.

	SOME VERSIONS OF ioccom.h DIDN'T INCLUDE _IORN AND _IOWN. SOL2 SEEMS TO HAVE THEM
*/


/* dev_reset and stream on & off don't affect other latched function bits*/

#define IHCPIO_DEV_RESET _IO('H',0)	/* reset attached device 	*/
					/* sets & resets latched reset	*/

#define IHCPIO_SET_CONFIG _IOW('H',1,int)	/* ignore busy, speed selection */

	/* config bit patterns: larg[0] = IGNORE_BUSY | SPEEDx */

#define 	IHCPIO_SPEED0   0x00000000	/* slowest device speed	*/
#define 	IHCPIO_SPEED1	0x00000004
#define 	IHCPIO_SPEED2	0x00000008
#define 	IHCPIO_SPEED3	0x0000000C	/*fastest device timing	*/
#define 	IHCPIO_IGNORE_BUSY 0x00000080	/* ignore centr busy 	*/

#define IHCPIO_SET_DMATIME _IOW('H',2,int) /* set dma timeout to arg secs	*/

#define IHCPIO_SET_FIFOTIME _IOW('H',3,int) /* set wait for <1/2 full or	*/
					  /* empty and dev rdy timeout	*/

#define IHCPIO_GET_REGS	_IORN('H',4,24)	/* puts all regs in arg array	*/

	/* returns all board registers.
	   DMAIO registers are all 32 bits wide. IHCP registers are 8
	   bits wide. each is returned in a 32 bit longword in the
	   following order:

	(u_long larg[6])

	larg[0] =	DMAIO CSR
	larg[1] =	DMAIO ADD
	larg[2] =	DMAIO RANGE
	larg[3] =	IHCP LATCHED FUNCTIONS
	larg[4] =	IHCP INTERFACE STATUS
	larg[5] =	IHCP DEVICE STATUS

	*/

#define IHCPIO_GET_STATUS _IOR('H',5,int)	/* returns FORMATTED device	*/
					/* status - not like dump regs	*/

#define 	IHCPIO_DEV_RDY	0x00000010	/* v-rdy or centr ack asserted*/
#define 	IHCPIO_DEV_BUSY	0x00000008	/* centronics asserting busy  */
#define 	IHCPIO_DEV_FAULT 0x00000004	/* centronics asserting fault */
#define 	IHCPIO_DEV_POUT	0x00000002	/* v or c no paper            */
#define 	IHCPIO_DEV_SEL	0x00000001	/* v online or c selected     */

#define IHCPIO_GET_BOARD _IOR('H',6,int)	/* gets board type	*/

#define 	IHCPIO_VERS_DIFF 0x000000A0	/* model 10104		*/
#define 	IHCPIO_VERS_TTL	 0x00000060	/* model 10105		*/
#define 	IHCPIO_CENT	0x000000C0	/* model 10106		*/


#define IHCPIO_SET_VMODE _IOW('H',7,int)	/* sets print/plot, spp - LEAVES*/
						/* READY FF ON			*/
#define IHCPIO_SET_VMODEX _IOW('H',8,int)	/* same as above, but CLEARS RDY*/
 						/* FF & WAITS FOR READY		*/

#define 	IHCPIO_V_SPP	0x00000002	/* sets spp w/MODE(X)      */
#define 	IHCPIO_V_PLOT	0x00000001	/* sets plot mode w/MODE(X)*/
#define		IHCPIO_V_NPRINT	0x00000000	/* 0 = normal print mode   */

#define IHCPIO_V_CMD	_IOW('H',9,int)	/* pulse commands to versatec	*/

#define 	IHCPIO_VLTR	0x00000000	/* remote line terminate*/
#define 	IHCPIO_VEOT	0x00000001	/* remote eot		*/
#define 	IHCPIO_VFED	0x00000002	/* remote form feed	*/
#define 	IHCPIO_VCLR	0x00000003	/* remote buffer clear	*/

#define IHCPIO_DATA_OUT	_IOWN('H',10,0)	/* CALLING PGM WILL AND IN THE  */
					/* ACTUAL CHARACTER COUNT - DONE*/
					/* THIS WAY SO COUNT CAN BE A   */
					/* VARIABLE, NOT CONSTANT !!!   */

					/* sends char array in ARG to	*/
					/* fifo if room. if not - return*/
					/* EINVAL. will flag error if	*/
					/* caller tries to send more	*/
					/* 255 bytes, or if fifo becomes*/
					/* full during transfers.	*/

#define IHCPIO_RDY_WAIT	_IO('H',11)	/* waits for fifo empty and	*/
					/* device ready			*/

#define	IHCPIO_HALF_WAIT _IO('H',12)	/* wait for fifo <half full	*/

#define IHCPIO_STREAM_ON _IO('H',13)	/* turns on data streaming (106)*/
#define IHCPIO_STREAM_OFF _IO('H',14)	/* turns off data streaming	*/

#define IHCPIO_GET_FLAGS _IOR('H',15,int)	/* returns unit's "flags" word	*/
					/* from driver's unit array	*/

#define 	IHCP_DMA_WAIT 0x80000000	/* waiting for dma end	  */
#define 	IHCP_RDY_WAIT 0x40000000	/* waiting for rdy & fifo */
#define		IHCP_HALF_WAIT 0x20000000	/* waitint for fifo<half  */
#define 	IHCP_DMA_TIMEOUT 0x08000000	/* timed out		  */
#define 	IHCP_RDY_TIMEOUT 0x04000000	/* timed out		  */
#define		IHCP_HALF_TIMEOUT 0x02000000	/* timed out		  */
#define		IHCP_SIG_RECEIVED 0x01000000	/* interrupted by signal  */

#define 	IHCP_BOARD_MASK 0x000000FF	/* board type here in flags*/

#define 	IHCP_CLEAR_FLAGS 0x0000FFFF	/* clears various flags	   */

#define	IHCPIO_GET_FIFO _IOR('H',16,int)	/* get formatted fifo flags	*/

#define		IHCPIO_FIFO_FULL 0x00000010	/* set if fifo full	*/
#define		IHCPIO_FIFO_HALF 0x00000008	/* if half full		*/
#define		IHCPIO_FIFO_EMPTY 0x00000004	/* if empty		*/

/*
	new with sol2 version of the driver - mclr and sack
	the user should NEVER need to use these!
*/

#define	IHCPIO_MASTER_CLEAR	_IO('H',17)	/* master clear board - this	*/
						/* is a big hammer - it is 	*/
						/* here for factory testing	*/
						/* only!!!!!!!!!!!!!!!!!	*/
						/* this does not send a clear to*/
						/* the device or change the	*/
						/* print/plot mode!		*/
						/* it does clear the latch reg-	*/
						/* including speed setting!	*/

#define	IHCPIO_SOFT_ACK		_IO('H',18)	/* software ack - simulates an	*/
						/* ack sequence from the device	*/



/*	additional commands and arguments for (hopefully) compatibility
	with SUN and Versatec drivers for the 10088 VME board
	the idea is to make them identical to the existing definitions
	in both name and value, so existing code will run without change
	(other than perhaps a new device name), and if re-compiled, can
	use the old names, even if the old .h file is not available	*/

#ifndef	LPCOMMAND	/* avoid if already defined in other .h file	*/


#define	LPCOMMAND	_IOW('v',3,int)	/* see commands following	*/
#define	LPSETVERSATEC	_IO('v',4)	/* select versatec (10088 only)	*/
#define	LPSETCENTRONICS	_IO('v',5)	/* select centronics (10088)	*/

/*	since the Sbus boards don't have selectable ports, the above _IO
	commands will be tested to make sure that they are asking for
	the type of interface available on the board being used		*/

/*	the following are the arguments available to the LPCOMMAND above*/

/*	first the "latched" functions	*/

#define	LPC_LRST	0x00001	/* assert the centronics INPUT PRIME sig*/
#define	LPC_DRST	0x00002	/* de-assert the INPUT PRIME (reset) sig*/

#define LPC_SOPT	0x00004	/* select option (cent) port		*/
#define	LPC_SVPT	0x00008	/* select versatec port			*/

#define	LPC_DSTR	0x00010	/* enable datastreaming (centr only)	*/
#define	LPC_DDST	0x00020	/* disable datastreaming		*/

#define	LPC_VSPP	0x00040	/* enable versatec SPP mode		*/
#define	LPC_DSPP	0x00080	/* disable SPP mode			*/

#define	LPC_VPLT	0x00100	/* enable versatec plot mode		*/
#define	LPC_DVPT	0x00200	/* disable plot mode			*/

#define	LPC_PRNT	0x00400	/* normal Versatec print mode		*/

#define LPC_LATCHMASK	0x007FF	/* all of the latched bits		*/
#define	LPC_PRINTMASK	0x007C0	/* just the print mode bits		*/

/*	now the "pulsed" functions	*/

#define	LPC_SACK	0x00800	/* software ack		*/
#define	LPC_MCLR	0x01000	/* master clear		*/
#define	LPC_VCLR	0x02000	/* Versatec clear	*/
#define	LPC_VTFF	0x04000	/* Versatec form feed	*/
#define	LPC_VEOT	0x08000	/* Versatec EOT		*/
#define	LPC_VLTR	0x10000	/* Versatec line term	*/

#define	LPC_PULSEMASK	0x1F800	/* just pulses		*/
#define	LPC_ALLVALID	0x1FFFF	/* all valid commands	*/

/* define register read-back command	*/

#define	CISREG	1
#define	CDSREG	2

struct lpregs {
	u_short	cisreg ;	/* interface status register	*/
	u_short cdsreg ;	/* device status register	*/
} ;

/* read back the interface status and device status registers */

#define	LPGETREGS	_IOR('v',10,struct lpregs)

/* set and readback the software timeout value - will apply to fifo wait
	and dma wait - timeout will be 'timeout'/50 seconds
	(fiftieths of a second).	*/

#define	LPSETTIMVAL	_IOW('v',11,int)
#define	LPGETTIMVAL	_IOR('v',12,int)


#endif	/* end of old definitions	*/

/* define the old-style register bits for the compatibility code -
	do it outside the #ifndef so they will still be available
	even if an old include file is used instead of this one to
	define the old ioctl parameters	*/

/* first the interface status register	*/

#define	OLD_DIRY	0x80	/* interface ready	*/
#define	OLD_DVRY	0x40	/* device ready		*/
#define	OLD_DMON	0x20	/* dma enabled		*/
#define	OLD_SOPT	0x10	/* centronics port sel	*/
#define	OLD_DSTR	0x08	/* data streaming	*/
#define	OLD_IENB	0x04	/* interrupt enabled	*/
#define	OLD_IFLG	0x02	/* interrupt flag	*/
#define	OLD_BERR	0x01	/* bus error		*/

/* now the device status register	*/

#define	OLD_VTTL	0x8000	/* versatec TTL mode	*/
#define	OLD_VRDY	0x4000	/* versatec ready	*/
#define	OLD_VPPR	0x2000	/* versatec paper ok	*/
#define	OLD_VONL	0x1000	/* versatec online	*/
#define	OLD_VSPP	0x0800	/* versatec spp mode	*/
#define	OLD_VPLT	0x0400	/* versatec plot mode	*/
#define	OLD_TENB	0x0100	/* test mode enabled	*/
#define	OLD_OACK	0x0080	/* centronics ack on	*/
#define	OLD_ONBY	0x0040	/* centr not busy	*/
#define	OLD_OPPR	0x0020	/* centr paper present	*/
#define	OLD_ONSL	0x0010	/* centr not selected	*/
#define	OLD_OFLT	0x0008	/* centr fault		*/
#define	OLD_OLRS	0x0004	/* centr long reset on	*/



/*
	define various flags and constants.  these may be modified by the user to
	change the power up configuration and operating behavior of the board(s)
	and driver.  at this time, these parameters apply to all boards operating
	under a particlar driver.
*/

#define SPEED_DEF	IHCP_SPEED1	/* default handshake timing		*/
					/* speed0 is slowest speed3 is fastest	*/
					/* speed1 gives 250ns set-up, 800ns	*/
					/* strobe, and 250ns hold for centronics*/
					/* or 100ns hold for versatec		*/

					/* speed1 is chosen because it will give*/
					/* decent performance in centronics apps*/
					/* and selects the highest speed in	*/
					/* versatec applications (speed0=speed2,*/
					/* speed1=speed3 for versatec boards)	*/

#define MODE_DEF	IHCP_NORM_PRINT	/* normal print mode	*/
					/* replace with desired	*/
					/* default mode		*/

#define DMA_TIME_DEF	1800		/* dma time-out default seconds	*/
#define FIFO_TIME_DEF	1800		/* fifo empty and <1/2 full time-out*/

#define DMA_TIME_MAX	3600		/* protect against hanging if 	*/
#define FIFO_TIME_MAX	3600		/* if caller asks for giant #	*/

#define	DMA_TIME_MIN	10		/* try to prevent timeout during*/
#define FIFO_TIME_MIN	10		/* active dma or legit wait	*/

