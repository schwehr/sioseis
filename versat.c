/*    a program to interface to the Sun Consulting's Versatec driver.
   note that the driver switches are toggles, once set they must be
   unset before doing something different.

   The plotter must be ATTACHED before using.
   VPRINT sets the versatec to the PRINT mode.  The mode stays set until
         changed.  All VWRITE after will cause PRINT.
   VPLOT set the plot mode.  Successive calls to VWRITE will be plots.
   VWRITE send nbytes to the Versatec.  It will be in either PRINT or PLOT
         mode depending on which of the above entries was called last.

    Copyright (C), Paul Henkart, Scripps Oceanography, August 1987
*/

#include <stdio.h>
#include "/usr/include/lpcmd.h"
#include <sys/file.h>
#include <sys/ioctl.h>

    int fd;
    int command;
    int status;
    char c;

attach_()
{
    fd = open( "/dev/lpv0", 1 );  /* open the Versatec for write only */
    command = LPC_DVPT;  /* unset any and all switches - disable plot */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;  /* disable simultaneous print/plot */
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSTR;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_VCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_MCLR;
    status = ioctl( fd, LPCOMMAND, &command);
    return;
}

detach_()
{
    command = LPC_DVPT;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_DSPP;
    status = ioctl( fd, LPCOMMAND, &command);
    command = LPC_VTFF;    /* form feed  */
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
         printf(" vwrite failed with status %d\n");
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


