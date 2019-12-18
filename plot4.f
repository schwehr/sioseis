c     This version of plot2 takes a UNIX SIOSEIS seismic plot file from a
c  SIOSEIS disc file and writes directly to the plotter using HP-RTL.  
c  PLOT2 opens c  all files for the user and assumes the Versatec is /dev/lpv0
c     This program is a beast because UNIX doesn't allow the use of the VERSATEC
c  SSP feature (simultaneous print/plot), therefore this program ors in a
c  rasterized version of the annotation.
c     Another problem is that CR/LF are need.
c
c  Copyright (C) 1985
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c
     
      PARAMETER (nchars=80)
      INTEGER*2 ibuf(5000)    
      CHARACTER*80 token
      CHARACTER*90 prtbuf
      DATA prtbuf/' '/

      nbytes = ZZ 
      PRINT *,' Enter the name of the input SIOSEIS plot file:'
      CALL rdline
      CALL getoke( token, nchars )
      token( nchars+1:nchars+1) = CHAR(0)
      CALL getfil( 4, ilun, token, istat )                               { open the plot file
      CALL attach
c****
c****   Get the Versatec toner flowing and get the plot out of the dried toner
c****  by printing 20 blank lines.  FF (Form Feed) doesn't get the toner flowing
C****
      CALL vprint                                                       { set UNIX to put the Versatec in print mode
      prtbuf = ' '
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 20 i = 1, 20  
         CALL vwrite ( prtbuf, 10 )                                     { write 10 characters
   20 CONTINUE
c****
c****    PRINT 44 LINES.  Line 1 is the date of the plot.  lines 2-41 are the
c****  SEGY tape header/SIOSEIS comments.  42-44 contain some plotting parameters.
c****
      DO 50 i = 1,44
         CALL rddisc( ilun, prtbuf, 20, istat )                          { read 20 words (80 characters)
         prtbuf(81:81) = CHAR(10)                                       { put in a CR
         CALL vwrite( prtbuf, 81 )
   50 CONTINUE
      prtbuf = ' '                                                      { make a gap between the header and the data
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 70 i = 1, 10  
         CALL vwrite( prtbuf, 10 )                                      { write 10 characters
   70 CONTINUE
      CALL vplot
c****
  100 CONTINUE
      CALL rddisc( ilun, ibuf, mwrds, istat )
      IF( istat .NE. mwrds ) GOTO 9999
      CALL vwrite( ibuf, nbytes )
      GOTO 100
c****
c****    END OF PLOT FILE
c****     
 9999 CONTINUE
      CALL vprint
      prtbuf = ' '  
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 10000 i = 1, 60
         CALL vwrite( prtbuf, 10 )                                      { write 10 characters
10000 CONTINUE
      CALL detach
      PRINT *,' finished the seismic plot.'
          
    
      END

