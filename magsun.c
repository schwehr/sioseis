#define debug   0 /* set to 1 to get debug print statements */
/*  mag.c is a collection of mag tape driver routines.
su root
cd /dev
ln -s rmt/0bn nrst0
 *  The first rule of Unix mag tape i/o is that the tape drives are accessed
 * by file descriptors, thus, each tape unit (0, 1, 2, 3) must be assigned
 * (through ASTAPE) before it can be used.
 *
 *   The various entry points in mag.c are:
 * astape(lun,name,wring)   -  assigns the tape - gets a file descriptor
 *     lun   - the tape drive unit number. Must be 0, 1, 2 or 3.  long integer.
 *     name  - The name of the reel to be mounted. Not used by Unix. Character*(*)
 *     wring - Now (Aug 22 1990) ignored (but must be present).
 *             A flag indicating whether the tape should have a write ring in or
 *             not.
 *           =0, No write ring should be in the tape. Open the file for reading
 *               only.
 *           =1, A write ring should be in the tape. Open the file for reading
 *               and writing.
 *
 * freetp(lunold,idumb)
 *
 * Function rdmt(lun,buffer,nbytes) - reads up to nbytes from lun into buffer
 *      lun  - The tape drive unit number to read from.
 *      buffer - An array to receive the tape record read.  Must be at least
 *               nbytes long.  Typeless
 *      nbytes - The maximum tape record size to be read.  buffer must be nbytes
 *               long.
 *      Returns a long integer containing the status of the read.
 *          >0,  The number of bytes read.
 *          =0,  Shouldn't happen
 *          <0,  A tape read error occurred.
 *
 * Function wrmt(lun,buffer,nbytes)  -  Write nbytes from buffer to lun.
 *      lun   - The tape drive unit number to write on.
 *      buffer - The array of bytes to be written to tape.  Typeless.
 *      nbytes - The number of bytes to write to tape.
 *      Returns the status of the write operation.
 *          >0,  The write wrote nbytes to tape. (everything ok!).
 *          =0,  Shouldn't happen.
 *          <0,  An error occurred.
 *
 * Function weofmt(lun) - Write a file mark on tape unit lun.
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function skffmt(lun) - Skip forward 1 file on lun.
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function skbfmt(lun) - Skip backwards a file on lun (position is in the
 *                        IRG just before the EOF).
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function skrfmt(lun) - Skip 1 record forward on lun (position in next IRG).
 *                        No data is transferred.
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function skrbmt(lun) - Skip 1 record backwards on lun (position to previous IRG).
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function rewmt(lun) - Rewind lun (position to BOT).
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * Function offlmt(lun) - Rewind and set lun offline.
 *      Returns the operation status.  Long integer.
 *           =0, OK
 *           <0, Error
 *
 * untape(lun) - rewinds, set offline and closes (frees) lun.
 *
 * clrmt(lun) - clears the tape drive controller status register.  This must be
 *              done after any errors have occurred.
 *
 * statmt(lun) - returns the error status of the tape drive. (mtget.mt_erreg)
 *               mtget.mt_erreg seems to be fairly machine and controller
 *               independent whereas mtget.mt_dsreg is very dependent.
 *               THIS IS AN INTEGER*2 FUNCTION
 *               by trial and error the Sun returns:
 *
 * Function getmtget_(lun,n,stat) - return the n'th element of mt_get
 *          n = 0 = mt_type
 *              1 = dsreg
 *              2 = erreg
 *              3 = resid
 *              4 = fileno
 *              5 = blkno
 *              6 = flags
 *              7 = bf
 *
 *      mtget.mt_dsreg  (drive status register):
 *             EMULEX CONTROLLER
 *             bit 0 - Tape unit ready      1
 *                 1 - Rewind in progress   2
 *                 2 - Write protected      4
 *                 3 - Settle down (in process of stopping)  8
 *                 4 - 7 track tape drive   16
 *                 5 - BOT                  32
 *                 6 - Selected (on-line)   64
 *                 7 - Non existent Memory  128
 *                 8 - Bad tape error (crap in the gap or blank tape for 16 feet) 256
 *                 9 - Record length error  512
 *                 10 - EOT                 1024
 *                 11 - bus grant late      2048
 *                 12 - Parity error        4096
 *                 13 - Cyclic Redundancy Error for NRZI or Phase error for 800 bpi  8192
 *                 14 - EOF                 16384
 *                 15 - Several other errors 32768
 *             SUN TAPEMASTER CPC CONTROLLER  ( BIT 0 on the right)
 *             bit 0 - not used
 *                 1 - write protected (no write ring)
 *                 2 - formatter busy
 *                 3 - ready
 *                 4 - EOT
 *                 5 - load point
 *                 6 - on line
 *                 7 - EOF
 *               8-12 - error
 *                 13 - retry occurred
 *                 14 - successful operation
 *                 15 - execution started (busy)
 *              SUN XYLOGIC 472 (trial and error)
 *              bit 2,3 - online and ready bits
 *                 5 - protected (no ring)
 *                 6 - BOT
 *                 7 - EOT
 *
 *
 *  mtget.mt_type  (from <sys/mtio.h>)
 *           MT_ISTS  = VAX UNIBUS TS-11 = 0x01
 *           MT_ISHT  = VAX MASSBUS TU77 = 0x02
 *           MT_ISTM  = VAX UNIBUS TM-11 = 0x03
 *           MT_ISMT  = VAX MASSBUS TU78 = 0x04
 *           MT_ISUT  = VAX UNIBUS GCR   = 0x05
 *           MT_ISCPC = SUN MULTIBUS CPC = 0x06
 *           MT_ISAR  = SUN archive      = 0x07
 *           MT_ISSC  = SUN SCSI         = 0x08
 *           MT_ISXY  = SUN XYLOGIC 472  = 0x09
 *  Above is not true for 1996 HPs.
 *   mtget.mt_type = 5 = 3480
 *                   7 = DAT
 *
 * Written by Paul Henkart, Scripps Oceanography, 17 May 1984
 * Copyright (C) The Regents of the University of California.  ALL RIGHTS RESERVED
 * modified 16 May 1989 by pch: add message when drive can't be opened
 * modified 4 Jan 1990 by pch: chmod 600 when assigning the drive and after
 *                             closing it
 * modified Sept 1990 by pch: comment out clrmt since Sun OS 4.1 now
 *                writes a file mark on MTNOP (no op) !
 * mod Aug 91 - set  fd[unitno] = 0;   when releasing tapes
 * mod 31 Oct 91 - for UTIG Exabyte changes
 * mod 15 Jan 92 - redo Sept 90 mod for MTNOP writing file marks
 * mod ????    - allow up to 40 device names
 * mod 1 Oct 97 - For Sun OS 4.1.3 I seem to get some different status
 *      meanings.  rdmt status = 0, seems to mean EOF
 *                 wrmt status = -1(?) seems to mean parity!
 * mod Mar 99 - args were reversed for strcpy
 * mod Apr 99 - Add reten - retension
 * mod 9 Jul 02 - Increase MAXUNITS to 50
 * mod 4 Aug 02 - Add getmtget
 * mod 16 Sep 02 - Add debug
 *
 * structure for MTIOCGET - June 1992 - mag tape get status command *
 * NOTE THAT THIS IS COMMENTED OUT ON THE HP!!!!!!!!!
struct  mtget   {
        short   mt_type;        * type of magtape device *
 * the following two registers are grossly device dependent *
        short   mt_dsreg;       * ``drive status'' register *
        short   mt_erreg;       * ``error'' register *
 * optional error info. *
        daddr_t mt_resid;       * residual count *
        daddr_t mt_fileno;      * file number of current position *
        daddr_t mt_blkno;       * block number of current position *
        u_short mt_flags;
        short   mt_bf;          * optimum blocking factor *
}
*/
/*  STATUSes:   for both read and write
    <0 = error (blank tape, write protected when writing, EOT on write)
     0 = EOF
    >0 = number of bytes done
*/

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <unistd.h>

#define MAXUNITS 50

/*   also in diskio.c !!!!!
void mknamec( nam )
        char    *nam;
{
        while( *nam != ' ' && *nam != '\0' ) *nam++;
        *nam = '\0';
} */

static  int   fd[MAXUNITS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0 };
static        int   unitno;
static        int	istat;
static        short jstat;
static        long  lstat;
static        int   n;
static        char  device[80];
		int i, j;

static struct  mtop mtop;
static struct  mtget mtget;

astape_(lun, name, wring)
long   *lun;
char   *name;
long   *wring;

{
#if debug
	printf("astape lun=%d, wring=%d, fd=%d\n",*lun,*wring,fd[unitno]);
#endif
      unitno = *lun;
      if( unitno < 0 ) unitno = -unitno;
      if( fd[unitno] != 0 ) return;
      sprintf(device, "/dev/nrst%d\0",unitno);
/*      sprintf(device, "/dev/rmt/%d\0",unitno); */
      if( strncmp(name,"/dev/",5) == 0 ) {
		mknamec( name );   /* make sure the name terminates with a NULL */
		strcpy( device, name);
	}
#if debug
	printf("given device is:  %s\n",device);
	printf("given name is:  %s\n",name);
#endif

      n = 2;    /*  preset to read and write */
      fd[unitno] = open( device, n);
      if( fd[unitno] < 0 ) fd[unitno] = open( device, 0);
      while( fd[unitno] < 0 )
	{
          perror( "ASTAPE open failed");
          printf("%s\n",device);
          printf("Is the tape on unit %d loaded and ready?\n",unitno);
		sleep(5);
		fd[unitno] = open( device, 0);
        }
      istat=ioctl(fd[unitno], MTIOCGET, &mtget);
	if( istat < 0 )
	{
		perror( "ASTAPE status failed" );
		exit( -1 );
	}
#if debug
	printf(" astape istat = %d\n",istat);
#endif
	return;
}

freetp_(lun,name)   /* see untape  */
	long	*lun;
	char	*name;

{
       unitno = *lun;
#if debug
	printf("freetp lun=%d, name= %s\n",*lun,name);
#endif
       sprintf(device, "/dev/nrst%d\0",unitno);
       if( strncmp(name,"/dev/",5) == 0 ) {
                mknamec( name );   /* make sure the name terminates with a NULL */
                strcpy( device, name);
#if debug
		printf("given device is:  %s\n",name);
#endif
        }
       chmod(device,0400);
       close(fd[unitno]);
       chmod(device,0600);
       fd[unitno] = 0;
       return;
}

/*
 *     write an eof
*/
long	weofmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTWEOF;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
	printf("weof, fd=%d, status= %d\n",fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *       skip file forward
*/

long	skffmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTFSF;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
	printf("skff, fd=%d, status= %d\n",fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *     skip back file
*/

long	skbfmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTBSF;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
	printf("skbf, fd=%d, status= %d\n",fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *     skip record forward
*/

long	skrfmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTFSR;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
	printf("skbf, fd=%d, status= %d\n",fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *    skip record backwards
*/

long	skrbmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTBSR;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
        printf("skrb, fd=%d, status= %d\n",fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *      rewind
*/
long	rewmt_(lun)
	long	*lun;
{
       unitno = *lun;
       mtop.mt_op = MTREW;
       mtop.mt_count = 1;
       istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
        printf("rew, fd=%d, status= %d\n",fd[unitno],istat);
#endif
       lstat=istat;
       return(lstat);
}

/*
 *     rewind and put offline
*/

long	offlmt_(lun)
	long	*lun;
{
        unitno = *lun;
        mtop.mt_op = MTOFFL;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
#if debug
	printf("offlmt unitno= %d %d istat= %d\n",unitno,fd[unitno],istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *     Read a record
*/

long	rdmt_(lun,buffer,nbytes)
	long	*lun;
	long	*buffer;
	long	*nbytes;
{
        unitno = *lun;
        n = *nbytes;
        istat = read(fd[unitno],buffer,n);
#if debug
	printf("rdmt fd= %d, n= %d, status= %d\n",fd[unitno],n,istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *     Write a tape record
*/
long	wrmt_(lun,buffer,nbytes)
	long	*lun;
	long	*buffer;
	long	*nbytes;

{
        unitno = *lun;
        n = *nbytes;
        istat = write(fd[unitno],buffer,n);
#if debug
	printf("wrmt fd=%d, n= %d, status= %d\n",fd[unitno],n,istat);
#endif
        lstat=istat;
        return(lstat);
}

/*
 *     Get the status of the drive and controller
*/
long
statmt_(lun)
long    *lun;

{
       unitno = *lun;
       istat=ioctl(fd[unitno], MTIOCGET, &mtget);
/*   see  /usr/include/sys/mtio.h   */

      printf("mt_type=%x\n",mtget.mt_type);
      printf("mt_dsreg=%x\n",mtget.mt_dsreg);
      printf("mt_erreg=%x\n",mtget.mt_erreg);
      printf("mt_resid=%x\n",mtget.mt_resid);
      printf("mt_fileno=%x\n",mtget.mt_fileno);
      printf("mt_blkno=%x\n",mtget.mt_blkno);
      printf("mt_flags=%x\n",mtget.mt_flags);
      printf("mt_bf=%x\n",mtget.mt_bf);

      jstat=mtget.mt_erreg;
/*
 * if no status obtained, make sure we get the most up to date one we can.
 *    STATUS is a joke now.  Drives no longer return drive status! ***
 */
	if( jstat == 0 && fd[unitno] != 0 )
	{
		/* clrmt( lun ); */
      		istat=ioctl(fd[unitno], MTIOCGET, &mtget);
		if( istat < 0 )
		{
			perror( "STATMT failed" );
			exit( -1 );
		}
      		jstat=mtget.mt_erreg;
	}
       return(jstat);
}

/*   Return the mtget structure  */

void	getmtget_(lun,n,stat)
	long    *lun;
	long	*n;
	long	*stat;

{
	unitno = *lun;
	istat=ioctl(fd[unitno], MTIOCGET, &mtget);
	switch( *n ) {
		case 0:
			*stat = mtget.mt_type;
			break;
		case 1:
			*stat = mtget.mt_dsreg;
			break;
		case 2:
			*stat = mtget.mt_erreg;
			break;
		case 3:
			*stat = mtget.mt_resid;
			break;
		case 4:
			*stat = mtget.mt_fileno;
			break;
		case 5:
			*stat = mtget.mt_blkno;
			break;
		case 6:
			*stat = mtget.mt_flags;
			break;
		case 7:
			*stat = mtget.mt_bf;
			break;
	}
	return;
}

/*       clear the status register */
long	clrmt_(lun)
	long	*lun;
{
       unitno = *lun;
       mtop.mt_op = MTNOP;
       mtop.mt_count = 1;
      istat = ioctl( fd[unitno], MTIOCTOP, &mtop);
       if( istat < 0 )
       {
               perror( "CLRMT failed" );
               exit( -1 );
       }

       return (0L);
}

/*
 *     Unassign the tape drive (close the file)
*/
long	untape_(lun)
	long	*lun;
{
        unitno = *lun;
        sprintf(device,"/dev/nrst%d",*lun);
        /* the close file closes tapes that are open for writing by writing two
           end of files at the current tape position. I submit that this is a
           very bad if not just plain incorrect practice.  Suppose you write
           2 files to tape then reread the first file then close the file!
           So, my way around this is to just change the mode of the file to
           a read only file, then close does not write 2 file marks!!!
         */
        chmod(device,0400);
        close(fd[unitno]);
        chmod(device,0600);
        fd[unitno] = 0;
        return(0L);
}

/*
 *     retension
*/

long    reten_(lun)
        long    *lun;
{
        unitno = *lun;
        mtop.mt_op = MTRETEN;
        mtop.mt_count = 1;
        istat=ioctl(fd[unitno], MTIOCTOP, &mtop);
/*      printf("reten unitno= %d %d istat= %d\n",unitno,fd[unitno],istat);*/
       if( istat < 0 )
       {
               perror( "MTRETEN failed" );
               exit( -1 );
       }
        lstat=istat;
        return(lstat);
}
