
#include <fcntl.h>
#include <stdio.h>

    int fd;
    int status;
    int address;

static    long std_transfer = 0x1B2A0000;
static    short remote_clear = 0x1B41;
static    short remote_line_terminate = 0x1B42;
static    short remote_EOT = 0x1B43;
static    short remote_FF = 0x1B44;
static    short remote_reset = 0x1B45;

attach_()
{
/*    fd = open( "/dev/ecpp0", O_WRONLY ); /* Revelle's iSys V12 with Centronics */
    fd = creat( "junk", 0777 );
    if( fd == -1 ) {
	perror(" open failed in v12");
	exit(); }
    return;
}

detach_()
{
    status = write( fd, &remote_FF, 2 );
    status = write( fd, &remote_EOT, 2 );
    close(fd);
    return;
}

vprint_()
{
    return;
}

vplot_()
{
    status = write( fd, &std_transfer, 3 );
    if( status == -1 ) {
	perror(" write failed in v12");
        exit(); }
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
         printf(" vwrite failed with status %d\n",n);
     return;
}



