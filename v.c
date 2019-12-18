/*    a program to interface to the Sun Consulting's Versatec driver.
   note that the driver switches are toggles, once set they must be
   unset before doing something different.

   The plotter must be ATTACHED before using (by calling entry attach).
   VPRINT sets the versatec to the PRINT mode.  The mode stays set until
         changed.  All VWRITE after will cause PRINT.
   VPLOT set the plot mode.  Successive calls to VWRITE will be plots.
   VWRITE send nbytes to the Versatec.  It will be in either PRINT or PLOT
         mode depending on which of the above entries was called last.

    Copyright (C), Paul Henkart, Scripps Oceanography, August 1987

I have a change to "v.c", the versatec driver, you might want to
use generally. It interlocks some file (typically the same file
the line printer spooler locks) so that you can assure multiple
independent users of the plotter that plots will not interfere.
It is a really simple change:

diff utig/v.c henkart/v.c
1,5c1
<  * modified 25jan90 mw, uses lock file /usr/spool/vpd/lock
<  * modified 30nov89 mw, locks the plotter first.
<  *
<     a program to interface to the Sun Consulting's Versatec driver.
---
> /*    a program to interface to the Sun Consulting's Versatec driver.
24,25d19
< #define LOCK_FILE "/usr/spool/vpd/lock"
<
33,47d26
<       int     lockfd;
<  * get an exclusive lock
<       if( (lockfd = open( LOCK_FILE, O_RDONLY, 0 )) < 0 )
<       {
<               perror( "plot3 - lockfile open" );
<               exit( 1 );
<       }
<       if( flock( lockfd, 2 ) < 0 )
<       {
<               perror( "plot3 - flock failed" );
<               exit( 1 );
<       }
<

--
Mark Wiederspahn, Institute for Geophysics, Univ. of Texas at Austin
voice: (512) 471-0490   fax: (512) 471-8844    telex: (910) 874-1380

 mod 14 Apr 00 by pch to remove the DSTR command in attach for the Ewing, which
               doesn't use the Centronics interface.
 mod ??? 00 by pch to add vflush
 mod 8 Oct 10 - Add attach_alias
*/

#include <stdio.h>
/*#include "lpcmd.h"*/
/*#include "/opt/IKONihcp/ihcp_io.h"   /*   Ewing  */
#include "ihcp_io.h"
typedef unsigned short u_short;
#include <sys/file.h>
#include <sys/ioctl.h>
#include <fcntl.h>

    int fd;
    int command;
    int status;
    char c;
static  char      tname[200];

void attach_()
{
/*    fd = open( "/dev/lpv0", O_WRONLY );  /* open the Versatec for write only */
    fd = open( "/dev/ihcp0", O_WRONLY );  /* open the Ewing Atlantekfor write only */
    if( fd == -1 ) perror(" open failed.");
    command = LPC_DVPT;  /* unset any and all switches - disable plot */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;  /* disable simultaneous print/plot */
    status = ioctl( fd, LPCOMMAND, &command);
/*    command = LPC_DSTR;
    status = ioctl( fd, LPCOMMAND, &command);  */
    command = LPC_VCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_MCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

void attach_alias_( name )
    char	*name;
{
/*    fd = open( "/dev/lpv0", O_WRONLY );  /* open the Versatec for write only */
/*    fd = open( "/dev/ihcp0", O_WRONLY );  /* open the Ewing Atlantekfor write only */
    strcpy(tname,name);
    mknamec( tname );
    fd = open( tname, O_WRONLY );
    if( fd == -1 ) perror(" open failed.");
    command = LPC_DVPT;  /* unset any and all switches - disable plot */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;  /* disable simultaneous print/plot */
    status = ioctl( fd, LPCOMMAND, &command);
/*    command = LPC_DSTR;
    status = ioctl( fd, LPCOMMAND, &command);  */
    command = LPC_VCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_MCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

detach_()
{
    command = LPC_DVPT;		/* disable plot mode  */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;		/* disable DSPP mode  */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_VTFF;		/* form feed  */
    status = ioctl( fd, LPCOMMAND, &command);
    status = write( fd, c, 1 );
    close(fd);
    return;
}

vprint_()
{
    command = LPC_DVPT;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_PRNT;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

vplot_()
{
    command = LPC_VPLT;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

vwrite_( buf, nbytes )
    int    *buf;
    int    *nbytes;
{
    int    n;

     n = *nbytes;
     status = write( fd, (char *) buf, n );
     if( status != n )
         printf(" vwrite failed with status %d, nbyte= %d\n",status,n);
     return;
}

vflush_()
{
    fflush(fd);
    return;
}

vspp_()    /* set the simultaneous print/plot mode  */
{
    command = LPC_VSPP;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

vdspp_()   /* disable SPP before plotting the next n (10?) lines  */
{
    command = LPC_DSPP;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}


