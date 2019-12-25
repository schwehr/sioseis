/*
 * ihcp_io.h
 * public ioctl and default definition file for Linux 2.0.35 driver for PCI hardcopy boards
 *
 * (this file unchanged through 2.6.x versions of driver)
 *
 * Tahoma Technology
 * (formerly Ikon Corporation)
 * 107 2nd Avenue North
 * Seattle, WA, USA 98109
 *
 * 206.728.6465
 * http://www.tahomatech.com
 * tahoma@tahomatech.com
 *
 * This code released under the GPL, and in the public domain
 * References to IKON left in place for compatibility and historical reasons
 */

#ifndef	IHCP_IO_H
#define	IHCP_IO_H

/*
 * the defines in this file are the "public" definitions used by the driver and the
 * calling program
 *
 * included are the ioctl command codes used by the code that calls the driver, and
 * some defaults that may be changed by editing this file and re compiling the driver
 *
 * this file is also used inside driver code !!! don't mess with this unless you mean it!
 */

#include <sys/ioctl.h>
#include <sys/types.h>

/* the ioctl() function call looks like:
 *
 * ioctl(filedescriptor,command,argument)
 *
 * argument is used in some of the ihcp ioctl calls to provide values to
 * the driver ioctl routine, or return values to the calling program.  In
 * one case - data out - it contains an array of character data to be sent
 * to the attached device.  the argument is restricted to a maximum of 255
 * bytes - by unix, and by the driver.
 *
 * the following ioctl commands
 * are available to programs using the ihcp driver:
 *
 * *** legacy ioctl commands - originally for Sbus hardcopy cards ***
 *
 * IHCPIO_DEV_RESET     reset attached device
 * IHCPIO_SET_CONFIG    select device timing,burst, and busy mode w/ value in arg
 * IHCPIO_SET_DMATIME   set dma timeout to arg seconds
 * IHCPIO_SET_FIFOTIME  set fifo <1/2 full and fifo empty+device ready
 *                      timeout to arg seconds
 * IHCPIO_GET_REGS      return board's registers in arg
 * IHCPIO_GET_STATUS    return device status in arg
 * IHCPIO_GET_BOARD     return board type in arg
 * IHCPIO_SET_VMODE     set versatec print/plot/spp mode using value in arg
 * IHCPIO_SET_VMODEX    same as VMODE, but clear ready sync ff - most plotters
 *                      will use VMODE - both for versatec only
 * IHCPIO_V_CMD         send pulse command to plotter using value in arg
 *                      (versatec only)
 * IHCPIO_DATA_OUT      send data bytes from arg array to device.  byte count
 *                      in IHCPIO_COUNT_MASK portion of cmd
 * IHCPIO_RDY_WAIT      wait for fifo empty and device ready
 * IHCPIO_HALF_WAIT     wait for fifo less than 1/2 full
 * IHCPIO_STREAM_ON     select data streaming mode (centronics only)
 * IHCPIO_STREAM_OFF    clear data streaming mode
 * IHCPIO_GET_FLAGS     returns unit_flags  in arg
 * IHCPIO_GET_FIFO      returns fifo status in arg
 * IHCPIO_MASTER_CLEAR  resets board and clears fifo FACTORY USE ONLY!
 * IHCPIO_SOFT_ACK      simulates ack sequence from device
 *
 * *** new ioctl commands for PCI hardcopy cards **
 *
 * IHCPIO_AUTO_LTR_COUNT        set automatic line terminate byte count (line length) to arg bytes
 * IHCPIO_AUTO_LTR_ON           enable automatic line terminate
 * IHCPIO_AUTO_LTR_OFF          disable auto line terminate
 * IHCPIO_DEV_AND_VEND_ID       return board device and vendor ids in arg
 * IHCPIO_REVISION_ID           return board revision level in arg
 * IHCPIO_LITTLE_ENDIAN         set little endian mode - plotter data will go low byte first
 * IHCPIO_BIG_ENDIAN            set big endian mode - plotter data will go high byte first
 *
 * ioctls to allow direct fiddling of mode and device control
 *
 * IHCPIO_DIRECT_MODE           direct write arg to mode register
 * IHCPIO_DEVICE_CONTROL        direct write arg to device control
 * IHCPIO_INTERFACE_STATUS      direct read of interface status register returned in arg
 * IHCPIO_DEVICE_STATUS         direct read of device status register returned in arg
 * IHCPIO_REVERSE_DATA          read of reverse data register returned in arg
 *
 */



/* the ioctl command codes conform to the unix pattern:
 *
 * the top 3 bits of the 32 bit value indicate whether arguments
 * are to be copied in, copied out, both, or neither.
 *
 * 0x80000000 = copy in
 * 0x40000000 = copy out
 * 0x20000000 = no argument transfer
 * 0xC0000000 = copy in and out
 *
 * the number of bytes in the argument is encoded in the lower
 * 8 bits of the upper half of the u_int, and the actual command
 * is encoded in the lower half.  a rather arbitrary character,
 * which is intended to identify the driver, is also encoded in
 * the lower half of the command.  it becomes part of the command
 * value.
 * for all commands except DATA_OUT, the arg length is part of
 * the command value.
 *
 * WHEN USING THE IHCPIO_DATA_OUT IOCTL,
 * THE CALLING PROGRAM IS REQUIRED TO COMBINE THE ARGUMENT LENGTH
 * WITH THE IOCTL COMMAND AS FOLLOWS:
 *
 * cmd = IHCPIO_DATA_OUT | ( ( arg_length & 0xFF ) << 16 ) ;
 *
 * more accurately, for linux:
 *
 * cmd = IHCPIO_DATA_OUT | ((arg_length & _IOC_SIZEMASK) << _IOC_SIZESHIFT)
 *
 * commands which require arguments - in or out - will pass those
 * values as 32 bit unsigned integers.  the only exception is the
 * DATA_OUT ioctl, which uses an array of up to 255 bytes as its
 * argument.
 *
 * the magic character that identifies this driver is hereby
 * (arbitrarily) chosen to be 'H'.
 *
 * the following ioctl commands are defined using pre-existing
 * ioctl command macros.  the CMD_MASK and COUNT_MASK values
 * defined MUST match the usage in ioctl.h. refer to that include
 * file for further information.
 *
 * ADDITIONAL IOCTL COMMANDS AND ARGUMENTS ARE FOUND AT THE END OF THIS
 * FILE.  THEY ARE INCLUDED FOR COMPATIBILITY WITH SUN AND VERSATEC
 * DRIVERS FOR THE 10088 VME BOARD, AND WILL HOPEFULLY BE COMPATIBLE
 * WITH OTHER MANUFACTURERS BOARDS AND DRIVERS (SHOULD THERE BE ANY!)
 *
 * the "magic character" for these commands is "v"
 */


/*
 * other versions of this driver used the entire lower 16 bits of the command to decode the specific
 * function requested.  this includes both the ioctl type (magic letter) and number as defined
 * in linux/ioctl.h.  we reconstruct the full function mask the hard way to try to protect against
 * the shifting sands of linux defines
 */

#define IHCPIO_CMD_MASK ((_IOC_TYPEMASK << _IOC_TYPESHIFT) | (_IOC_NRMASK << _IOC_NRSHIFT))


#define IHCPIO_COUNT_MASK IOCSIZE_MASK	/* arg byte count here - IOCSIZE_MASK is already shifted left  */


/*
 * THE SOLARIS 2.0 MACROS IN ioccom.h WANT THE 'MAGIC LETTER' QUOTED.
 * THE SOLARIS 1.X MACROS WANTED IT WITHOUT QUOTES.
 * LINUX WANTS QUOTES
 *
 * SOME VERSIONS OF ioccom.h DIDN'T INCLUDE _IORN AND _IOWN. SOL2 SEEMS TO HAVE THEM
 * linux/ioctl.h DOESN'T SEEM TO HAVE THEM
 *
 * we will use the lower-level _IOC macro (asm/ioctl.h) to define these since we need to
 * provide the size as an actual byte count, not as a type input to sizeof(), which is used
 * by the higher level (_IOR, _IOW) macros
 */

#define	_IORN(type, nr, size)	_IOC(_IOC_READ, (type), (nr), (size))
#define	_IOWN(type, nr, size)	_IOC(_IOC_WRITE, (type), (nr), (size))




#define IHCPIO_DEV_RESET _IO('H',0)	/* reset attached device        */
					/* sets & resets latched reset  */

#define IHCPIO_SET_CONFIG _IOW('H',1,__u32)	/* arg bits set configuration   */

/*
 * the config bit patterns use those defined for the Sbus card - even
 * though they don't match the PCI card's bit posistions.  this is done
 * to try to preserve compatibility between applications
 *
 * the first version of this driver doesn't try to support all the possible
 * modes available in the PCI card.
 *
 */

/*      *** legacy bits ****    */

#define 	IHCPIO_SPEED0   0x00	/* slowest device speed */
#define 	IHCPIO_SPEED1	0x04
#define 	IHCPIO_SPEED2	0x08
#define 	IHCPIO_SPEED3	0x0C	/*fastest device timing */
#define 	IHCPIO_IGNORE_BUSY 0x80	/* ignore centr busy    */

/*      *** new for PCI ***     */

#define		IHCPIO_BUSY_HANDSHAKE	0x100	/* use BUSY instead of ACK      */
#define		IHCPIO_4_EDGE		0x200	/* use four edge handshake      */
#define		IHCPIO_V_BURST		0x400	/* select synch burst mode only */
						/* available on some new v-tech */
						/* and requires pld218B or later */

#define IHCPIO_SET_DMATIME _IOW('H',2,__u32)	/* set dma timeout to arg secs  */

#define IHCPIO_SET_FIFOTIME _IOW('H',3,__u32)	/* set wait for <1/2 full or    */
						/* empty and dev rdy timeout    */
						/* to arg seconds               */

/*
 * return array size used here and in ioctl code
 * _IORN takes the direct byte count, not a type to be
 * passed to the sizeof() operator, as is done with
 * the other _IOx macros
 */

#define	IHCP_RETURN_ARRAY_SIZE	19	/* in long words!!      */

#define IHCPIO_GET_REGS	_IORN('H',4,IHCP_RETURN_ARRAY_SIZE * 4)	/* puts all regs in arg array   */

/*
 * returns all "IHCP" registers and some of the "PLX" registers.
 * also returns the device IDs and revision level.
 * PLX registers are all 32 bits wide. IHCP registers are 8
 * bits wide. each is returned in a 32 bit longword as shown below.
 * the DMA registers returned are either channel 0 or 1, depending
 * on which is used in the particular revision of the board.
 *
 *
 * (__u32 larg[19])
 *
 * larg[0] =    device ID (high 16 bits) & vendor id (low 16 bits)
 * larg[1] =    revision ID
 * larg[2] =    PLX interrupt control/status
 * larg[3] =    PLX EEPROM control and user bits
 * larg[4] =    PLX DMA mode
 * larg[5] =    PLX DMA PCI address
 * larg[6] =    PLX DMA local address
 * larg[7] =    PLX DMA transfer count
 * larg[8] =    PLX DMA descriptor pointer
 * larg[9] =    PLX DMA command/status register
 * larg[10]=    interrupt mask
 * larg[11]=    mode
 * larg[12]=    device control
 * larg[13]=    interface control
 * larg[14]=    interface status
 * larg[15]=    device status
 * larg[16]=    reverse data
 * larg[17]=    auto ltr count low
 * larg[18]=    auto ltr count high
 *
 */

#define IHCPIO_GET_STATUS _IOR('H',5,__u32)	/* returns FORMATTED device     */
						/* status in arg                */

#define 	IHCPIO_DEV_RDY	0x00000010	/* v-rdy or centr ack asserted */
#define 	IHCPIO_DEV_BUSY	0x00000008	/* centronics asserting busy  */
#define 	IHCPIO_DEV_FAULT 0x00000004	/* centronics asserting fault */
#define 	IHCPIO_DEV_POUT	0x00000002	/* v or c no paper            */
#define 	IHCPIO_DEV_SEL	0x00000001	/* v online or c selected     */

#define IHCPIO_GET_BOARD _IOR('H',6,__u32)	/* gets board strapping - sbus compatibility    */
						/* strapping bits returned in arg               */

#define 	IHCPIO_VERS_DIFF 0x000000A0	/* versatec differential        */
#define 	IHCPIO_VERS_TTL	 0x00000060	/* versatec ttl                 */
#define 	IHCPIO_CENT	0x000000C0	/* centronics                   */

#define IHCPIO_SET_VMODE _IOW('H',7,__u32)	/* sets print/plot, spp as selected by arg      */
#define IHCPIO_SET_VMODEX _IOW('H',8,__u32)	/* same as above for PCI boards                 */

#define 	IHCPIO_V_SPP	0x00000002	/* sets spp w/MODE(X)      */
#define 	IHCPIO_V_PLOT	0x00000001	/* sets plot mode w/MODE(X) */
#define		IHCPIO_V_NPRINT	0x00000000	/* 0 = normal print mode   */

#define IHCPIO_V_CMD	_IOW('H',9,__u32)	/* pulse commands to versatec selected by arg   */

#define 	IHCPIO_VLTR	0x00000000	/* remote line terminate */
#define 	IHCPIO_VEOT	0x00000001	/* remote eot           */
#define 	IHCPIO_VFED	0x00000002	/* remote form feed     */
#define 	IHCPIO_VCLR	0x00000003	/* remote buffer clear  */

#define IHCPIO_DATA_OUT	_IOWN('H',10,0)		/* CALLING PGM WILL AND IN THE  */
						/* ACTUAL CHARACTER COUNT - DONE */
						/* THIS WAY SO COUNT CAN BE A   */
						/* VARIABLE, NOT CONSTANT !!!   */
						/* see ioctl description above  */

						/* sends char array in arg to   */
						/* fifo if room. if not - return */
						/* EINVAL. will flag error if   */
						/* caller tries to send more    */
						/* 255 bytes, or if fifo becomes */
						/* full during transfers.       */

#define IHCPIO_RDY_WAIT	_IO('H',11)	/* waits for fifo empty and     */
					/* device ready                 */

#define	IHCPIO_HALF_WAIT _IO('H',12)	/* wait for fifo <half full     */

#define IHCPIO_STREAM_ON _IO('H',13)	/* turns on data streaming      */
#define IHCPIO_STREAM_OFF _IO('H',14)	/* turns off data streaming     */

#define IHCPIO_GET_FLAGS _IOR('H',15,__u32)	/* returns unit's "flags" word          */
						/* from driver's unit array in arg      */
						/* these defines are also used internal */
						/* to the driver - so don't modify!     */

#define 	IHCP_DMA_WAIT 0x80000000	/* waiting for dma end    */
#define 	IHCP_RDY_WAIT 0x40000000	/* waiting for rdy & fifo */
#define		IHCP_HALF_WAIT 0x20000000	/* waitint for fifo<half  */
#define 	IHCP_DMA_TIMEOUT 0x08000000	/* timed out              */
#define 	IHCP_RDY_TIMEOUT 0x04000000	/* timed out              */
#define		IHCP_HALF_TIMEOUT 0x02000000	/* timed out              */
#define		IHCP_SIG_RECEIVED 0x01000000	/* interrupted by signal  */

#define 	IHCP_BOARD_MASK 0x000000FF	/* board strapping here in flags        */

#define 	IHCP_CLEAR_FLAGS 0x0000FFFF	/* clears upper half of flags           */

#define	IHCPIO_GET_FIFO _IOR('H',16,__u32)	/* return formatted fifo flags in arg   */

#define		IHCPIO_FIFO_FULL 0x00000010	/* set if fifo full     */
#define		IHCPIO_FIFO_HALF 0x00000008	/* if half full         */
#define		IHCPIO_FIFO_EMPTY 0x00000004	/* if empty             */

/*
 * new with sol2 version of the driver - mclr and sack
 * the user should NEVER need to use these!
 */

#define	IHCPIO_MASTER_CLEAR	_IO('H',17)	/* master clear board - this    */
						/* is a big hammer - it is      */
						/* here for factory testing     */
						/* only!!!!!!!!!!!!!!!!!        */
						/* this does not send a clear to */
						/* the device                   */
						/* it does clear the latch reg- */
						/* including speed setting!     */

#define	IHCPIO_SOFT_ACK		_IO('H',18)	/* software ack - simulates an  */
						/* ack sequence from the device */
/*
 * ** the following are new ioctl commands for PCI hardcopy cards **
 */

#define	IHCPIO_AUTO_LTR_COUNT	_IOW('H',19,__u32)	/* sets automatic line terminate byte   */
							/* count (line length) to arg bytes     */
							/* do not write while data in fifo if	*/
							/* auto ltr previously enabled		*/

#define	IHCPIO_AUTO_LTR_ON	_IO('H',20)	/* enables automatic line terminate     */

#define	IHCPIO_AUTO_LTR_OFF	_IO('H',21)	/* disables auto line terminate */

#define	IHCPIO_DEV_AND_VEND_ID	_IOR('H',22,__u32)	/* returns device and vendor ids in arg                 */
							/* vendor id is low 16 bits, device id is high 16 bits  */

#define	IHCPIO_REVISION_ID	_IOR('H',23,__u32)	/* returns board revision level in arg  */

#define	IHCPIO_LITTLE_ENDIAN	_IO('H',24)	/* plotter data will go low byte first  */

#define	IHCPIO_BIG_ENDIAN	_IO('H',25)	/* plotter data will go high byte first */

/*
 * the following commands allow fiddling some of the device and mode control
 * bits directly - this allows customizing the handshake, and limited software-compelled
 * IEEE 1284 support
 *
 * these are definitely not for the faint of heart!
 */

#define	IHCPIO_DIRECT_MODE	_IOW('H',26,__u32)	/* direct write of arg to mode register */

#define	IHCPIO_DEVICE_CONTROL	_IOW('H',27,__u32)	/* direct write of argto device control */

#define	IHCPIO_INTERFACE_STATUS	_IOR('H',28,__u32)	/* direct read of interface status register returned in arg     */

#define	IHCPIO_DEVICE_STATUS	_IOR('H',29,__u32)	/* direct read of device status register returned in arg        */

#define	IHCPIO_REVERSE_DATA	_IOR('H',30,__u32)	/* read of reverse data register returned in arg        */


/*
 * additional commands and arguments for (hopefully) compatibility
 * with SUN and Versatec drivers for the 10088 VME board
 * the idea is to make them identical to the existing definitions
 * in both name and value, so existing code will run without change
 * (other than perhaps a new device name), and if re-compiled, can
 * use the old names, even if the old .h file is not available
 */

#ifndef	LPCOMMAND		/* avoid if already defined in other .h file    */


#define	LPCOMMAND	_IOW('v',3,__u32)	/* see commands following       */
#define	LPSETVERSATEC	_IO('v',4)	/* select versatec (10088 only) */
#define	LPSETCENTRONICS	_IO('v',5)	/* select centronics (10088)    */

/*
 * since the PCI boards don't have selectable ports, the above _IO
 * commands will be tested to make sure that they are asking for
 * the type of interface strapped on the board being used
 */

/*
 * the following are the arguments available to the LPCOMMAND above
 */

/*
 * first the "latched" functions
 */

#define	LPC_LRST	0x00001	/* assert the centronics INPUT PRIME sig */
#define	LPC_DRST	0x00002	/* de-assert the INPUT PRIME (reset) sig */

#define LPC_SOPT	0x00004	/* select option (cent) port            */
#define	LPC_SVPT	0x00008	/* select versatec port                 */

#define	LPC_DSTR	0x00010	/* enable datastreaming (centr only)    */
#define	LPC_DDST	0x00020	/* disable datastreaming                */

#define	LPC_VSPP	0x00040	/* enable versatec SPP mode             */
#define	LPC_DSPP	0x00080	/* disable SPP mode                     */

#define	LPC_VPLT	0x00100	/* enable versatec plot mode            */
#define	LPC_DVPT	0x00200	/* disable plot mode                    */

#define	LPC_PRNT	0x00400	/* normal Versatec print mode           */

#define LPC_LATCHMASK	0x007FF	/* all of the latched bits              */
#define	LPC_PRINTMASK	0x007C0	/* just the print mode bits             */

/*
 * now the "pulsed" functions
 */

#define	LPC_SACK	0x00800	/* software ack         */
#define	LPC_MCLR	0x01000	/* master clear         */
#define	LPC_VCLR	0x02000	/* Versatec clear       */
#define	LPC_VTFF	0x04000	/* Versatec form feed   */
#define	LPC_VEOT	0x08000	/* Versatec EOT         */
#define	LPC_VLTR	0x10000	/* Versatec line term   */

#define	LPC_PULSEMASK	0x1F800	/* just pulses          */
#define	LPC_ALLVALID	0x1FFFF	/* all valid commands   */

/*
 * define register read-back command
 */

#define	CISREG	1
#define	CDSREG	2

struct lpregs {
	__u16 cisreg;		/* interface status register    */
	__u16 cdsreg;		/* device status register       */
};

/*
 * read back the interface status and device status registers
 */

#define	LPGETREGS	_IOR('v',10,struct lpregs)

/*
 * set and readback the software timeout value - will apply to fifo wait
 * and dma wait - timeout will be 'timeout'/50 seconds
 * (fiftieths of a second).
 */

#define	LPSETTIMVAL	_IOW('v',11,__u32)
#define	LPGETTIMVAL	_IOR('v',12,__u32)


#endif				/* end of old definitions       */

/*
 * define the old-style register bits for the compatibility code -
 * do it outside the #ifndef so they will still be available
 * even if an old include file is used instead of this one to
 * define the old ioctl parameters
 */

/*
 * first the interface status register
 */

#define	OLD_DIRY	0x80	/* interface ready      */
#define	OLD_DVRY	0x40	/* device ready         */
#define	OLD_DMON	0x20	/* dma enabled          */
#define	OLD_SOPT	0x10	/* centronics port sel  */
#define	OLD_DSTR	0x08	/* data streaming       */
#define	OLD_IENB	0x04	/* interrupt enabled    */
#define	OLD_IFLG	0x02	/* interrupt flag       */
#define	OLD_BERR	0x01	/* bus error            */

/*
 * now the device status register
 */

#define	OLD_VTTL	0x8000	/* versatec TTL mode    */
#define	OLD_VRDY	0x4000	/* versatec ready       */
#define	OLD_VPPR	0x2000	/* versatec paper ok    */
#define	OLD_VONL	0x1000	/* versatec online      */
#define	OLD_VSPP	0x0800	/* versatec spp mode    */
#define	OLD_VPLT	0x0400	/* versatec plot mode   */
#define	OLD_TENB	0x0100	/* test mode enabled    */
#define	OLD_OACK	0x0080	/* centronics ack on    */
#define	OLD_ONBY	0x0040	/* centr not busy       */
#define	OLD_OPPR	0x0020	/* centr paper present  */
#define	OLD_ONSL	0x0010	/* centr not selected   */
#define	OLD_OFLT	0x0008	/* centr fault          */
#define	OLD_OLRS	0x0004	/* centr long reset on  */



/*
 * define various flags and constants.  the .._DEF defines may be modified by the user to
 * change the module install time configuration and operating behavior of the board(s)
 * and driver.  at this time, these parameters apply to all boards operating
 * under a particlar driver.
 *
 * THESE DEFINES SHOULD NOT BE MODIFIED HERE - THESE ARE THE DEFAULTS USED WHEN THERE
 * ARE NO OVERRIDING VALUES SUPPLIED TO insmod
 * IF IT IS NECESSARY TO CHANGE THE DEFAULT BEHAVIOR OF THE DRIVER,
 * MAKE CHANGES BY SUPPLYING APPROPRIATE VALUES TO insmod
 *
 * the values that may be modified at module install time are:
 *
 * vers_speed_def       versatec handshake speed, 0 = fastest, 3 = slowest
 * cent_speed_def       centronics handshake speed, 0 = fastest, 3 = slowest
 * mode_def             print/plot mode for versatec, 1 = plot, 0 = print
 * dma_time_def         dma timeout in seconds
 * fifo_time_def        ready & fifo <1/2 full timeout in seconds
 * dma_buf_order        size of dma data copy buffer for each board, in orders
 *                      order is the power of 2 of the number of pages requested (0 = 1 page,
 *                      3 = 8 pages, 5 is max for linux as of this writing)
 *                      buffer memory grabbed at module load time, driver install
 *                      will fail if insufficient kernel memory
 * max_boards           number of boards probed for when module loaded
 *                      setting max_boards higher than actual number of boards
 *                      present will waste a small quantity of kernel memory
 *
 * example:             /sbin/insmod ihcp_debug.o max_boards=3
 *
 * the above loads the version of the driver with debug printing enabled, and probes for
 * a maximum of three boards
 *
 */


#define	LOW_BYTE_FIRST		0	/* little endian data ordering          */
#define	HIGH_BYTE_FIRST		1	/* big endian data ordering             */

#define	DATA_STREAMING_OFF	0	/* for old vme getregs compatibility    */
#define	DATA_STREAMING_ON	1

#define DMA_TIME_MAX	3600	/* protect against hanging if   */
#define FIFO_TIME_MAX	3600	/* if caller asks for giant #   */

#define	DMA_TIME_MIN	10	/* try to prevent timeout during */
#define FIFO_TIME_MIN	10	/* active dma or legit wait     */

#define	BYTE_ORDER_DEF	LOW_BYTE_FIRST	/* default data ordering is little endian               */
					/* choices are LOW_BYTE_FIRST and HIGH_BYTE_FIRST       */

#define	VERS_SPEED_DEF	1	/* versatec speed default - used if property not in conf */
#define CENT_SPEED_DEF	1	/* centronics default           */
#define	MODE_DEF	1	/* 1=plot, 0=print - for versatec       */
#define	DMA_TIME_DEF	1800	/* default dma timeout in seconds       */
#define	FIFO_TIME_DEF	1800	/* ready and 1/2 fifo wait timeout default      */


#define	MAX_PHYS_ORDER_DEF	4	/* 2**max_phys_order * PAGE_SIZE = max dma xfer */
					/* larger user buf is xferred in chunks	*/
					/* 4 -> 64K max w/4K pages			*/

#define	MAX_BOARDS_DEF	2	/* default to two boards, to save a little kernel memory */

#endif	/* IHCP_IO_H */
