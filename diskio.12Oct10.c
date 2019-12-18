#define	debug	0 /* set to 1 to get debug print statements */

/* set tabstop=6  */
/*  diskio is a set of entry points to do all diskio for large programs
 * where file keeping is a problem.  Diskio also does the actual i/o in an
 * efficient manner (not Fortran!), and also allows the user to position
 * within a file.
 *
 *    The following entry points are in this c program.
 * GETFIL(mode, lun, name, istat)   assigns disk files and unit numbers
 * GETFIL64(mode, lun, name, istat) needed in some machines for files > 2GB
 * FREFIL(mode, lun, istat)    frees or releases units
 * PODISC(lun, mode, nwrd)    positions lun to 32 bit word nwrd
 * PODISC64(lun, mode, nwrd) positions  lun to 64 bit word nwrd
 * RDDISC(lun, buffer, nwrds, istat)   reads nwrds 32 bit words from disk unit lun
 * WRDISC(lun, buffer, nwrds)    writes nwrds 32 bit words to disk unit lun
 * PODISCB(lun, mode, nbyte) positions lun to byte nbyte, nbyte is 32 bits
 * PODISCB64(lun, mode, nbyte) positions lun to byte nbyte, nbyte is 64 bits
 * RDDISCB(lun, buffer, nbytes, istat)   reads nbytes bytes from disk unit lun
 * WRDISCB(lun, buffer, nbytes)    writes nbytes bytes to disk unit lun
 * FILSIZ( name, isize )  return the size of the file name
 * GETDIR( name, istat )  return the current path
 * GODIR( name, istat ) Change directories (cd) to name
 * ADRDISC( lun, address ) Return the current disk address.
 * ADRDISC64( lun, address ) Return the current disk address (2 32bit words).
 * FDSYNC( lun ) - Calls fsync (synchronize a file's in-memory state )
 *
****   NOTE  ****   lun is an index to an array of file descriptors within this
                   subroutine, thus any I/O must be done through this subroutine
                   (since the other I/O doesn't have the file descriptor!)

c  Call GETFIL(MODE, LUN, NAME, ISTAT)
c
c     GETFIL KEEPS TRACK OF, AND OPTIONALLY ASSIGNS, DISK FILE UNIT NUMBERS.
C  THE PURPOSE IS TO ASSIGN UNIT NUMBERS TO SUBROUTINES AS THEY ARE NEEDED,
C  RATHER THAN EACH SUBROUTINE ASSIGNING A PARTICULAR NUMBER FOR A PARTICULAR
C  TASK, AND THUS NO OTHER ROUTINES BEING ABLE TO USE THAT UNIT NUMBER EVEN
C  THOUGH THE ORIGINAL TASK IS DONE.  MOST FORTRANS ALSO HAVE A LIMIT AS TO
C  THE LARGEST UNIT NUMBER ALLOWED, THUS GETFIL OPTIMIZES THE TOTAL NUMBER OF UNIT
C  NUMBERS USED IN A COMPUTER RUN.  FURTHER MOTIVATION FOR USE OF GETFIL IS THAT
C  SYSTEM CONFIGURATIONS ALLOW CERTAIN UNIT NUMBERS TO BE USED FOR SYSTEM
C  SOFTWARE AND THE POOR PROGRAMMER HAS TO KEEP TRACK OF WHAT THE SYSTEM USES
c  (such as stdin=0, stdout=1, errout=2).
C    GETFIL WILL FIND AN UNUSED UNIT NUMBER AND RETURN IT'S NUMBER, AS WELL AS
C  OPTIONALLY ASSIGNING A DISK FILE TO THE UNIT NUMBER.  A UNIT NUMBER CAN
C  BE OPTIONALLY RESERVED BUT NOT ASSIGNED TO A DISK FILE.
C     THE FILE ASSOCIATED WITH THE UNIT LUN WILL HAVE A NAME sioseis_tmp##, WHERE  ##
C  IS LUN.  WHEN REQUESTED, GETFIL CLOSES, DELETES, AND OPENS sioseis_tmp##.
C     FILES sioseis_tmp## MAY BE DELETED AND FILE UNIT NUMBERS RELEASED BY USING
C  SUBROUTINE FREFIL.
C  GETFIL ARGUMENTS:
C   MODE   - THE TYPE OF DISK ASSIGNMENT TO MAKE. INTEGER*4
c          >0,  LUN is returned by GETFIL.
C          =1,  FINDS A FREE UNIT NUMBER AND RETURNS IT VIA LUN.
C               CREATE A NEW DISK FILE NAMED sioseis_tmp## AND START AT THE BEGINNING.
C          =2,  JUST RETURN A FREE UNIT NUMBER AND RESERVE IT SO THAT NO OTHER
C               CALL CAN USE IT.  DO NOT OPEN ANY FILES FOR IT.
C          =3,  FINDS A FREE UNIT NUMBER AND CREATES THE FILE GIVEN IN NAME TO
C               THE UNIT.  NAME MUST BE GIVEN.
c          =4,  Finds a free unit number and opens the existing file name. NAME
c               must be given.  The file is opened for reading and writing
c               unless permission is denied, in which case the file is opened
c               for reading only.
c          <0,  LUN must be specified by the calling routine.
C          =-1,  RESERVE UNIT NUMBER LUN ( LUN MUST BE SPECIFIED BY THE
C                CALLING ROUTINE) AND  CREATES THE FILE sioseis_tmp## FOR READING AND
c                WRITING.
C          =-2,  RESERVE UNIT NUMBER LUN. DO NOT OPEN ANY FILES.
c          =-3,  creates file name on unit lun. (both specified)
c          =-4,  opens file name on unit lun. (both must be specified)
C   LUN     - THE FILE UNIT NUMBER. INTEGER*4
C             LUN IS SET BY GETFIL WHEN MODE>0.
C             LUN MUST BE SET BY THE CALLING ROUTINE WHEN MODE<0.
C   NAME    - A CHARACTER FILE NAME ASSIGNED BY GETFIL THAT HAS BEEN ASSIGNED
C             ON LUN.  THIS IS RETURNED BY GETFIL.  DISKIO will generate
c             a 6 character name for mode 1.  DISKIO forces the string by
c             to terminate will a NULL, as is required in C, by putting
c             a NULL in the first blank in the string.
  ****   FORTRAN users must terminate the name with a blank   ******
c   ISTAT   - The return status of the file assignment.
            =0, File opened properly.

C           =-1, TOO MANY FILES ALREADY EXIST, UNIT NUMBER NOT RESERVED.
C           =-2, LUN IS ALREADY ASSIGNED.

  CALL FREFIL( MODE, LUN, ISTAT )
  Frefil releases and closes (with file marks at the current pointer) the file
associated with lun.  The file must have been assigned via GETFIL.  The file
may be (optionally) deleted (unlinked).

  arguments:
    mode  - The mode of freeing to perform.
          =-2,  Close the file on lun, do no release the unit (file descriptor).
          =-3,  Close and delete the file on lun.
          =1,  Release the unit, do not close or delete the file.
          =2,  Release the unit and close the file (do not delete it).
          =3,  Release the unit, close and delete the file.
          =4,  Release, close and delete all SCRATCH files opened by
               GETFIL mode 1.
    lun   - The logical unit number of the file
    istat - The return status of the file action.
          >=0, No problems.
          =-1, Invalid mode.
          =-2, Invalid lun.
          =-3, lun was not assigned.



  CALL PODISC( LUN, MODE, NWRD )
  CALL PODISCB( LUN, MODE, NBYTE )
  PODISC positions and open the disc file associated with lun.  The positioning
may be to an absolute address or relative to the current file pointer.
The first address is 0, the second adress is 1, etc.
  ARGUMENTS:
    LUN  - The unit number of the file to be positioned.
    MODE - The mode of positioning.
         =1, The file is positioned to the ABSOLUTE word address.
         =2, The file is positioned nwrd RELATIVE to the current file pointer.
    NWRD - The number of 4 byte words to position to. The number of 32 words.
    NBYTE - The byte number to postion to. 


  CALL RDDISC( LUN, BUFFER, NWRDS, ISTAT )
  CALL RDDISCB( LUN, BUFFER, NBYTES, ISTAT )
  RDDISC reads nwrds 32bit words from the disc file associated with the file on
unit lun.  
  RDDISCB reads nbytes from the disc file associated with the file on
unit lun.  
  ARGUMENTS:
    LUN    - The logical unit number of the file to be read.
    BUFFER - The array to receive the results of the read.  Buffer must be at
             laest nwrds long.
    NWRDS  - The number of words to read into buffer. Each words is 4 bytes.
    NBYTES - The number of bytes to read into buffer.
    ISTAT  - The return status of the read.
           >0, ISTAT words/bytes were read into buffer (No problems).
           =-1,  An end of file was detected.
           <-1, A problem occurred.


  CALL WRDISC( LUN, BUFFER, NWRDS)
  CALL WRDISCB( LUN, BUFFER, NBYTES)
  WRDISC writes nwrds from buffer to disc file associated with lun.
  WRDISCB writes nbytes bytes from buffer to disc file associated with lun.
If the device is a tty, then write to it and hope that it is finished by the time
we do another (e.g. if a plot is sent to the C.ITOH printer we shall not
worry about the XONN/XOFF protocal - GETFIL sets TTY devices to raw mode).
  ARGUMENTS:
   LUN    - The logical unit number of the file to write to.
   BUFFER - The array in memory to write to disc.
   NWRDS  - The number of 32 bit words (4 byte words) to write to disc.
   NBYTES  - The number of bytes to write to disc.

Where has the copyright gone?
Where has the mod history gone?

Copyright (C) by The Regents of The University of California, 1980
Written by Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca.
All Rights Reserved.

mod 29 June 1995 - block unit 7 because of HPUX Fortran.
mod 3 Aug 98 - When file can't be opened in getfil, set fd[*lun]
    to the unit number and set *lun to 0
mod Mar 99 - Keep track of the file names in char array fname so that
     unlink works!
mod May 99 - Add new entries filsiz, getdir, godir - Rudolf Widmer-Schnidrig
mod May 99 - Try to open as a large file (>2GB) if open fails.
           - Add getfil64 to open or create as a largefile.
           - Add adrdisc
mod Sep 99 - mkname barfed on inserting \0 when the file name was a constant
mod Dec 99 - Bah Humbug.  Change frefil unlink to rm sioseis_tmp%d
mod Oct 00 - DLz, added subroutines podisc64, podiscb64 to use 64-bit 
             addresses to position the disk
mod Apr 02 - Add #include <unistd.h> and SEEK_CUR, SEEK_SET
mod Jul 02 - Add define osx and define debug
mod Jul 02 - Add PMODE_ALL so tmp files can be deleted by everybody
mod 23 Oct 02 - Add fsync to "sync" or force write the buffer.
mod 19 May 03 - getfil64 mode 3 needs close after creat so it can be
                read as well as written.
mod 28 May 02 - Mac OSX returns 0 file size on > 2GB files, so
                getfil then went bad.
mod 26 Sep 03 - Always use 64bit args to lseek in podisc and podiscb.
mod 13 Oct 04 - Correct inverted order of fname declaration.
mod 2 Sep 05 - Change the tmp file names to include the pid for multiple
               usage in same directory.
mod 15 May 06 - Extend podisc addresses from 2GB to 8GB by making the
                address argument unsigned long and then doing an add
                rather than a multiply since gcc only supports 64 bit
                addition.
mod 13 June 06 - exit(1) if write error.
               - Previous podisc/lseek changes caused bomb on OSX because
                 offset on OSX is NOT off_t.  off_t is 8 bytes, but 
                 address must be int (even unsigned int bombed).  Do the
                 2GB to 4GB extension by calling lseek 4 times!
mod 29 Apr 08 - The podisc change above prevented mode 0 working (bad/wrong thing to do, but it used to work)
mod 5 May 08 - Now podisc relative doesn't work.
mod 20 May 08 - podisc > 2GB didn't work, so try seek_set to 0, then do 4
                set_cur offset (which is in 4 byte words
mod 1 Aug 08 - Increase file name from 80 to 200
mod Jul 09 - Change podisc address to unsigned.
mod 25 Aug 09 - That change screwed negative relative positioning.
mod 25 Aug 09 - New function podisc_un
mod 30 Aug 09 - Cygwin didn't like podisc_un, so made it podiscun
mod 3 May 10 - make printf of long long (and off_t)   %d%d
mod 7 May 10 - podiscun was wrong.
*/

/* #include <sgtty.h>
	struct    sgttyb    term; */ /*  if the device is a tty then it's not disk!  */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


#define   MAXFDS    40  /* the most files allowed in UNIX */
#define   PMODE     0755 /* read, write, execute for owner, read and exec for group */
#define	PMODE_ALL	0777 /* read, write, execute for everybody  */
char *strcat();
char *strcpy();
void mknamec( nam )
	char 	*nam;
{
	while( *nam != ' ' && *nam != '\0' ) *nam++;  /* find a blank or the end */
        *nam = '\0';
}

/* 0 is stdin
   1 is stdout
   2 is stderr
   5 is fortran reader
   6 is fortran printer
   7 HPUX fortran has problems with opening unit 7 'UNFORMATTED'
*/
static    int       fd[MAXFDS] = {0, 1, 2, -1, -1, 5, 6, 7, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
static	int	reserved[MAXFDS] = {0, 1, 2, -1, -1, 5, 6, 7, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                               -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
static	int       origin;
static	char      fname[MAXFDS][200];
static	char      tname[200];
static	int       nbytes;
static	int       status;
static	int       i;
static	int		pid;
static	struct    stat	stbuf;
	long long int	offset64, temp64;
	long long int	status64;
	void	exit();
	off_t	offset;



     void getfil64_(mode, lun, name, istat)
          int       *mode;
          int       *lun;
          char      *name;
          int       *istat;
{
#if debug
	printf("getfil64, mode= %d, lun= %d\n",*mode);
#endif
	*istat = 0;
	*lun = -1;
	for (i = MAXFDS-1; fd[i] == -1 && i > 0; i--)  *lun = i;
	if ( *lun == -1 ){
	if( fd[3] == -1 ) *lun = 3;
	else if( fd[4] == -1 ) *lun = 4 ;
	if(  *lun == -1) {
		printf(" ***  ERROR  ***  Too many units for getfil64 (%d max).%d\n",MAXFDS,*mode);
		*istat = -1;
		exit(0) ; }
	}
	if(*mode == 1 ) {
		pid = getpid();
		sprintf(name, "sioseis_%dtmp%d\0", pid,*lun);
		status = creat( tname, PMODE );
		close(status);
		strcpy(fname[*lun],name);
		status = open(name,2);
	}
	if( *mode == 3 ) {
		strcpy(tname,name);
		mknamec( tname );
		status = creat( tname, PMODE );
		close(status);
		open(tname,2);
		fd[*lun] = status;
		strcpy(fname[*lun],tname);
	}
	if( *mode == 4 ) {
		strcpy(tname,name);
		mknamec( tname );
		status = open( tname, 2);
		if( status == -1 ) status = open( tname, 0);
		if( status == -1 ) status = open( tname, 1);
	}
#if debug
	printf("getfil64 status= %d, lun= %d\n",status,*lun);
#endif
	if( status != -1 ) {
		fd[*lun] = status;
		strcpy(fname[*lun],tname);
		*istat = 0;
		return;
	}else{
		perror(name);
		*lun = -1;
		*istat = status;
		return;
	}
}




	void getfil_(mode, lun, name, istat)
          int       *mode;
          int       *lun;
          char      *name;
          int       *istat;

{
      *istat = 0;
#if debug
	printf("getfil, mode= %d\n",*mode);
#endif
      if(*mode < 0 ) {   /* negative mode means the caller specifies the lun */
		if( *lun < 0 || *lun > MAXFDS ){   /* is it a legal lun? */
			printf("Bad disk unit number of %d\n",*lun);
			exit(0) ; }
		if(*mode == -1 ) {
			pid = getpid();
			sprintf(name, "sioseis_%dtmp%d\0", pid,*lun);
			status = creat( name, PMODE_ALL ); /* read and write */
			close(status);
			status = open(name,2); }
		if(*mode == -2) {
			fd[*lun] = *lun;
			return ; } /* just reserve the unit number */
		if( *mode == -3 ) {  /* creat the file on the specified unit */
			strcpy(tname,name);
			mknamec( tname ); /* make sure the name terminates with a NULL */
			fd[*lun] = creat ( tname, PMODE );
			status = close(fd[*lun]);
			status = open(tname,2);} /* might be a different fd! */
		if( *mode == -4 )  {
			strcpy(tname,name);
			mknamec( tname ); /* make sure the name terminates with a NULL */
			status = open ( tname, 2);
			if( status == -1 ) status = open( tname, 0); 
			if( status == -1 ) status = open( tname, 1);
			if( status == -1 ) {
/*		printf("wouldn't open as a small file, trying as a big file.\n");*/
				status = open( tname, 2);
				if( status == -1 ) status = open( tname, 0);
				if( status == -1 ) status = open( tname, 1);
			}
			if( status != -1 ) stat( tname, &stbuf );  /* get the status and file size */
#if debug
			stat( tname, &stbuf );
			printf("lun= %d, status=%d, stbuf.st_size = %x\n",
			*lun,status,stbuf.st_size); 
#endif
		}
		fd[*lun] = status;
		strcpy(fname[*lun],name);
		if( fd[*lun] == -1 ) status = -1 ;
		if( status > 0 ) {
			*istat = 0;
           /*      if( strcmp("/dev/tty", name) == 0 ) { 
                      printf(" modifying tty!!!");
                      ioctl( fd[*lun], TIOCGETP, &term);
                      term.sg_flags = EVENP | ODDP | RAW | XTABS;
                      ioctl( fd[*lun], TIOCSETN, &term); }
           */
			return;}
		*istat = -3 ;
		perror(name);
		return; }
       else {      /* find a free unit by searching fd for a -1 */
           *lun = -1;
           for (i = MAXFDS-1; fd[i] == -1 && i > 0; i--)  *lun = i;
           if ( *lun == -1 ){
               if( fd[3] == -1 ) *lun = 3;
                   else if( fd[4] == -1 ) *lun = 4 ;
               if(  *lun == -1) {
                   printf(" ***  ERROR  ***  Too many units for getfil (%d max).%d\n",MAXFDS,*mode);
                   *istat = -1;
                   exit(0) ; }
           }
           if ( *mode == 2) {  /* =2 means just reserve the unit - don't open */
               fd[*lun] = 99 ;  /* set it to  something , but not -1 */
               return ; }
           if(*mode == 1 ) {
               pid = getpid();
               sprintf(name, "sioseis_%dtmp%d", pid,*lun) ;   /* create the name sioseis_tmp##  */
			status = creat( name, PMODE_ALL );
               close(status);
               strcpy(fname[*lun],name);
			fd[*lun] = status;
			status = open(name,2); }
           if( *mode == 3 ) {
			strcpy(tname,name);
                mknamec( tname );   /* make sure the name terminates with a NULL */
                status = creat( tname, PMODE );   /* open with read and write privileges */
                close(status);
                status = open(tname,2);
                fd[*lun] = status;
                strcpy(fname[*lun],tname);}
           if( *mode == 4 ) { 
			strcpy(tname,name);
    			mknamec( tname );
			status = open( tname, 2);
			if( status == -1 )  status = open( tname, 0);  /* open it for read only if read and write fails */
			if( status == -1 ) status = open( tname, 1);  /* open for write only if read only failed */
			if( status == -1 ) {
/*		printf("wouldn't open as a small file, trying as a big file.\n");*/
				status = open( tname, 2);
				if( status == -1 ) status = open( tname, 0);
				if( status == -1 ) status = open( tname, 1);
			}
			if( status != -1 ) stat( tname, &stbuf );  /* get the status and file size */
#if debug
			printf("lun= %d, status=%d, tbuf.st_size = %x\n",
			*lun,status,stbuf.st_size);
#endif
			if( status != -1 ) {
				fd[*lun] = status; /* return the file desriptor  */
				strcpy(fname[*lun],tname);
				*istat = 0;  /* everything is ok */
       /*          if( strncmp("/dev/tty", name, 8 ) == 0 ) {
                      printf(" unit=%d",fd[*lun]);
                      printf(" modifying tty!!!");
                      ioctl( fd[*lun], TIOCGETP, &term);
                      term.sg_flags = EVENP | ODDP | RAW | XTABS;
                      ioctl( fd[*lun], TIOCSETN, &term);  }
        */
				return ;
			}
          }
#if debug
	printf("lun= %d, status= %d, fd[*lun]= %d\n",*lun,status, fd[*lun]);
#endif
		if( status <= 0 ) {
/*			fd[*lun] = -1;  changed Mar 12, 99  pch  */
			*lun = -1;
/*			perror(name);   removed 1 Oct 99, pch  */
			*istat = status ;
			return;
		}
	}
}


	void frefil_( mode, lun, istat)
        int    *mode;
        int    *lun;
        int    *istat;

{
        char    cmd[80];

#if debug
      printf("frefil, mode= %d, lun= %d\n",*mode,*lun);
#endif
      if( *mode == -2) {   /* -2 means just close lun - do not release the unit */
            status = close( fd[*lun] );    /* close -- delete a descriptor  */
            return; }
      if( *mode == -3) {  /* -1 means close and delete sioseis_tmp## - don't release the unit number */
            status = close( fd[*lun] );
            sprintf(cmd, "rm ");
            strcat(cmd,fname[*lun]);
            system(cmd);
            fd[*lun] = -1 ;
            return; }
      if( *mode == 1) {   /* =1 means just to release the unit - don't close! */
            fd[*lun] = -1 ;
            return; }
      if( *mode == 2 )  {   /* =2 means release the unit number and close sioseis_tmp## */
            status = close( fd[*lun] );
            fd[*lun] = -1;
            return; }
      if( *mode == 3 ) {  /* =3 means release, close and delete */
            status = close( fd[*lun] );
            sprintf(cmd, "rm ");
            strcat(cmd,fname[*lun]);
            system(cmd);
            fd[*lun] = -1 ;
            return; }
      if( *mode == 4 )  { /*  =4 means release, close, delete all scratch (sioseis_tmp) files */
            for ( i = 3; i < MAXFDS ; i++ ) {
                  if( reserved[i] == -1 &&  fd[i] > 0 && fd[i] != 99 ) {
                      status = close( fd[i] );
				pid = getpid();
               	sprintf(cmd, "sioseis_%dtmp%d", pid, i ) ;   /* create the name sioseis_tmp##  */
                      if( strcmp(cmd,fname[i]) == 0 ) {
                          sprintf(cmd, "rm sioseis_%dtmp%d\n", pid,i);
                          system(cmd);
                      }
                      fd[i] = -1; }
            }
            return;
      }
}

	void filsiz_(name, bsize)
	char      *name;
	long long       *bsize;

{
	strcpy(tname,name);
	mknamec( tname );   /* make sure the name terminates with a NULL */
      stat(tname, &stbuf);
/*	printf("stbuf.st_size= %x(hex), %d(dec)\n",stbuf.st_size,stbuf.st_size); */
      *bsize = stbuf.st_size;
      return;
}

/* GETCWD
     Note: getdir has one argument more than the call to this
                   function in FORTRAN!
    (FORTRAN (at least on SUNs) expects to be passed the length of
    all character variables in a way that remains hidden to the
    FORTRAN user. This is done by passing additional arguments,
    one integer for each character variable!
    here cwd_len contains the length of the character variable cwd
    exactly as declared in the FORTRAN main program! )

    If this code is portable I eat a broom!     -ruedi
    Works on Sun and HP.  bombs on SGI.  - paul
*/

	void getdir_(cwd, istat, cwd_len)
	char      *cwd;
	int       *istat;
	int       cwd_len;
{
	*istat = -1 ;
/*	printf("GETDIR: cwd_len = %d\n", cwd_len); */
	getcwd(cwd, cwd_len);
	*istat = 0 ;
/*	printf("GETDIR: cwd = %s\n", cwd); */
	return;
}

	void godir_(dir, istat)
	char        *dir;
	int         *istat;
{
	*istat = -1 ;
/*	printf("GODIR: dir = %s\n", dir); */
	if (( *istat == chdir( dir ) ) == NULL) return;
	perror("godir");
	return;
}



	void podisc_( lun, mode, addres)
      int     *lun;
      int     *mode;
/*  unsigned doesn't work because mode 2 may have a negative  */
/*      unsigned long     *addres; */
      int     *addres;

{
#if debug
      printf(" podisc, lun=%d, mode=%d, addr=%d(dec) %x(hex)\n",*lun,*mode,*addres,*addres);
#endif
      origin = SEEK_SET;  /* preset to origin of the beginning of the file */
      if( *mode == 2 ) origin = SEEK_CUR;   /* mode = 2 means relative to the current position */
      temp64 = *addres;
      offset64 = temp64 + temp64 + temp64 + temp64 ;  /* the address was given in units of 4 bytes */
/*  convert to long long int before doing the addition  */
/*  do it as addition since gcc does not do long long multiplication  */
	 if( *mode == 1 || *mode == 0 ) {
/* this is for when lseek offset has to be 32 bit word */
	 	/*offset = *addres * 4;*/
	 	offset = *addres;
		status = lseek( fd[*lun], (off_t)0, SEEK_SET) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
	 } else {
	 	offset = *addres;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
		status = lseek( fd[*lun], offset, SEEK_CUR) ;
	 }
      status64 = status;
      if( status64 == -1 ) {
          /*    positioning an unopened file has a status of -1  */
		fprintf( stderr, "Warning: disk address= %d%d, status= %d, addres= %d\n",offset64,status64,temp64);
		lseek( fd[*lun], (off_t)0, SEEK_SET );
	 }
      return;
}

/*****     podiscun    ******/
/*****  mode 2 with negative relative address will fail !!!!!!!!  */
        void podiscun_( lun, mode, addres_un )
      int     *lun;
      int     *mode;
      unsigned long     *addres_un;

{
#if debug
      printf(" podiscun, lun=%d, mode=%d, addr=%d \n",*lun,*mode,*addres_un);
#endif
      origin = SEEK_SET;  /* preset to origin of the beginning of the file */
      if( *mode == 2 ) origin = SEEK_CUR;   /* mode = 2 means relative to the current position */
      temp64 = *addres_un;
/*      offset64 = temp64 + temp64 + temp64 + temp64 ; */
      if( *mode == 1 || *mode == 0 ) {
        /*offset = *addres * 4;*/
                status = lseek( fd[*lun], (off_t)0, SEEK_SET) ;
                status = lseek( fd[*lun], temp64, SEEK_CUR) ;
                status = lseek( fd[*lun], temp64, SEEK_CUR) ;
                status = lseek( fd[*lun], temp64, SEEK_CUR) ;
                status = lseek( fd[*lun], temp64, SEEK_CUR) ;
        } else {
                printf(" ****  ERROR  ****  Do not use podisc_un with relative seeks\n");
         }
      if( status == -1 ) {
          /*    positioning an unopened file has a status of -1  */
                fprintf( stderr, "Warning: disk address= %d%d, status= %d, addres= %d\n",offset64,status,temp64);
                lseek( fd[*lun], (off_t)0, SEEK_SET );
         }
      return;
}


	void podisc64_( lun, mode, addres64)
      int	*lun;
      int	*mode;
      off_t	*addres64;  /* 64 bit address  */

{
#if debug 
	printf(" podisc64, lun=%d, mode=%d, addr=%d\n",*lun,*mode,*addres64);
#endif
	temp64 = *addres64;
      offset64 = temp64 + temp64 + temp64 + temp64;  /* the address was given in units of 4 bytes */
      origin = SEEK_SET;  /* preset to origin of the beginning of the file */
      if( *mode == 2 ) origin = SEEK_CUR;   /* mode = 2 means relative to the current position */
      status64 = lseek( fd[*lun], offset64, origin) ;
      if( status64 < 0 ) {
		fprintf( stderr, "Warning: disk address, %d, less than 0, setting to 0\n",status64);
		lseek( fd[*lun], (off_t)0, SEEK_SET );
      }
      return;
}

	void podiscb_( lun, mode, addres)
      int     *lun;
      int     *mode;
      int     *addres;

{
#if debug
      printf(" podiscb, lun=%d, mode=%d, addr=%d\n",*lun,*mode,*addres);
#endif
      offset64 = *addres ;  /* the address was given in units of bytes */
      origin = SEEK_SET;  /* preset to origin of the beginning of the file */
      if( *mode == 2 ) origin = SEEK_CUR;   /* mode = 2 means relative to the current position */
      status64 = lseek( fd[*lun], offset64, origin) ;
      if( status64 < 0 ) {
		fprintf( stderr, "Warning: disk address, less than 0, setting to 0\n");
		lseek( fd[*lun], (off_t)0, SEEK_SET );
      }
      return;
}

	void podiscb64_( lun, mode, addres64)
      int     *lun;
      int     *mode;
      off_t	*addres64;

{
#if debug
	printf(" podiscb64, lun=%d, mode=%d, addr=%d\n",*lun,*mode,*addres64);
#endif
      offset64 = *addres64 ;  /* the address was given in units of bytes */
      origin = SEEK_SET;  /* preset to origin of the beginning of the file */
      if( *mode == 2 ) origin = SEEK_CUR;   /* mode = 2 means relative to the current position */
      if( status < 0 ) {
	fprintf( stderr, "Warning: disk address, %d, less than 0, setting to 0\n",status);
	lseek( fd[*lun], (off_t)0, SEEK_SET );
      }
      return;
}

	void adrdisc_( lun, addres )
	int	*lun;
	int	*addres;

{
	*addres = lseek( fd[*lun], (off_t)0, SEEK_CUR );
#if debug
	printf("addr= %d \n",*addres);
#endif
/*
   This is wrong on OSX - it prints when the address < 2GB -
like everytime it's called.  What to do?  Just wait until it's
wrong and the calling program gets the wrong answer?  Guess so,
so comment the warning out.
   The right way to do it would be to do the lseek with 64 bits
and error out if the address is > 32 bits - then the caller
knows it got a bad address.
*/
	return;
}

	void adrdisc64_( lun, addres64 )
	int	*lun;
	off_t	*addres64;

{
	*addres64 = lseek( fd[*lun], (off_t)0, SEEK_CUR );
/*	printf("addr64= %d\n",*addres64);  what's the format for longlong?  */
	return;
}

	void rddisc_(lun, buffer, nwrds, istat)
       int     *lun;
       int     *buffer;
       int     *nwrds;
       int     *istat;

{
       nbytes = *nwrds * 4 ;  /* nwrds is the number of 4 byte words to read */
#if debug
	offset = lseek( fd[*lun], (off_t)0, SEEK_CUR );
#endif
       status = read( fd[*lun], buffer, nbytes);
#if debug
	printf(" rddisc, lun=%d, from byte %d%d nbytes=%d, status=%d\n",*lun,offset,nbytes,status);
#endif
       if( status < 0 ) {
              printf(" ***  ERROR  ***  disc file read error on unit %d, status = %d\n",*lun,status);
              perror("rddisc");
              *istat = status ; }
       else *istat = status / 4 ;  /* convert the number of bytes read to words */
       *istat = status / 4;
       if ( *istat == 0 ) *istat = -1 ;  /* istat=-1 means end of file  */
       return;
}


	void rddiscb_(lun, buffer, n, istat)
       int     *lun;
       int     *buffer;
       int     *n;
       int     *istat;

{
	nbytes = *n ; 
#if debug
	offset = lseek( fd[*lun], (off_t)0, SEEK_CUR );
#endif
       status = read( fd[*lun], buffer, nbytes);
#if debug
	printf(" rddiscb, lun=%d, n=%d, status=%d from %d\n",*lun,*n,status,offset);
#endif
       if( status < 0 ) {
              printf(" ***  ERROR  ***  disc file read error on unit %d, status = %d\n",*lun,status);
              perror("rddisc");
              *istat = status ; }
       else *istat = status ;
       *istat = status;
       if ( *istat == 0 ) *istat = -1 ;  /* istat=-1 means end of file  */
       return;
}

	void wrdisc_(lun, buffer, nwrds)
       int     *lun;
       int     *buffer;
       int     *nwrds;

{
#if debug
/*       watch out if it's a 64 bit file  */
	offset = lseek( fd[*lun], (off_t)0, SEEK_CUR );
	printf(" wrdisc, lun=%d, to byte %d %d, nwrds=%d\n",*lun,offset,*nwrds);
#endif
       nbytes = *nwrds * 4 ;  /* nwrds is the number of 4 byte words to write */
       status = write( fd[*lun], buffer, nbytes);
       if( status != nbytes ) {
             printf(" ***  ERROR  ***  Disk file write error on unit %d, status = %d\n",*lun,status);
             perror("wrdisc"); 
             exit(1); }
       return;
}

	void wrdiscb_(lun, buffer, n)
       int     *lun;
       int     *buffer;
       int     *n;

{
#if debug
	offset = lseek( fd[*lun], (off_t)0, SEEK_CUR );
	printf(" wrdiscb, lun=%d, to byte %d %d, nwrds=%d\n",*lun,offset,*n); 
#endif
	nbytes = *n ;
	status = write( fd[*lun], buffer, nbytes);
if( status != nbytes ) {
		printf(" ***  ERROR  ***  Disk file write error on unit %d, status = %d\n",*lun,status);
		perror("wrdisc"); 
          exit(1); }
	return;
}

	void fdsync_( lun )
	int     *lun; 
 
{
	status = fsync( fd[*lun] );
	status = fflush( stdin );
#if debug
	printf("fdsync:  fd= %d, lun= %d, status= %d\n",fd[*lun],*lun,status);
#endif
	return;    
}



/* end */
