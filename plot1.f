c     This version of plot2 takes a UNIX SIOSEIS seismic plot file from a
c  SIOSEIS disc file and writes directly to the Versatec plotter.  PLOT2 opens
c  all files for the user and assumes the Versatec is /dev/lpv0
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
      INTEGER*2 ifonts(160)
      CHARACTER*1 c
      INTEGER*2 iorbuf(8,16)                                            { this holds 8 rasterized characters
      CHARACTER*90 prtbuf
      CHARACTER*80 token
      DATA iorbuf/128*0/
      DATA prtbuf/' '/
      DATA ifonts/
     * 0,4080,2064,2064,2064,2064,2064,2064,2064,2064,2064,2064,
     *      2064,2064,4080,0,                                           { zero
     1    0,64,192,320,576,64,64,64,64,64,64,64,64,64,4080,0,           { one
     2  0,3968,2112,32,16,16,16,16,32,64,128,256,512,1024,4080,0,       { two
C     2  0,3968,64,32,16,16,16,16,16,16,16,16,16,16,4080,0,             { two
     3  0,3968,64,32,32,32,32,3968,32,32,32,32,32,32,3968,0,            { three
     4  0,1024,1024,1056,1056,1056,2032,32,32,32,32,32,32,32,32,0,      { four
     5  0,2016,1024,1024,1024,1024,1984,256,128,64,32,32,64,128,1920,0, { five
     6  0,1984,2048,2048,2048,2048,2048,2048,2048,2016,2080,2080,
     6         2080,2080,2016,0,                                        { six
     7  0,4080,16,32,64,128,256,256,512,512,1024,1024,2048,2048,
     7         2048,0,                                                  { seven
     8 0,4080,2064,2064,2064,2064,2064,2064,4080,2064,2064,2064,2064,
     8          2064,4080,0,                                            { EIGHT
     9 0,1984,4064,4112,4112,4064,2000,16,16,16,16,16,16,16,16,0        { nine
     *   /  
 

    1 PRINT *,' Enter the value used for SIOSEIS plot parameter nibs.'
      READ *, itype
      IF( itype .NE. 160 .AND. itype .NE. 200 .AND.
     *    itype .NE. 201 .AND. itype .NE. 5732 ) THEN
          PRINT *,' Incorrect answer, try again.'
          GOTO 1
      ENDIF
      IF( itype .EQ. 160 ) mwrds = 90                                   { the number of 32 bit words per raster line
      IF( itype .EQ. 200 ) mwrds = 66
      IF( itype .EQ. 201 ) mwrds = 146
      IF( itype .EQ. 5732 ) mwrds = 146
      nbytes = mwrds * 4                                                { the number of bytes per raster line
      PRINT *,' Enter the name of the input SIOSEIS plot file:'
      CALL rdline
      CALL getoke( token, nchars )
      token( nchars+1:nchars+1) = CHAR(0)
      CALL getfil( 4, ilun, token, istat )                               { open the plot file
      CALL attach
c****
c****   Get the Versatec toner flowing and get the plot out of the dried toner
c****  by printing 20 blank lines.  FF (Form Feed) doesn't get the toner flowing{
C****
      CALL vprint                                                       { set UNIX to put the Versatec in print mode
      prtbuf = ' '
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 20 i = 1, 20  
c         CALL vwrite ( prtbuf, 10 )                                     { write 10 characters
   20 CONTINUE
c****
c****    PRINT 44 LINES.  Line 1 is the date of the plot.  lines 2-41 are the
c****  SEGY tape header/SIOSEIS comments.  42-44 contain some plotting parameters.
c****
      DO 50 i = 1,44
         CALL rddisc( ilun, prtbuf, 20, istat )                          { read 20 words (80 characters)
         prtbuf(81:81) = CHAR(10)                                       { put in a CR
c         CALL vwrite( prtbuf, 81 )
   50 CONTINUE
      prtbuf = ' '                                                      { make a gap between the header and the data
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 70 i = 1, 10  
c         CALL vwrite( prtbuf, 10 )                                      { write 10 characters
   70 CONTINUE
      CALL vplot 

c**** 
c****   Now get the 8 characters of annotation from the disc and then the 132
c**** words of plot, then OR them together like the Versatec simultaneous print/
c**** mode would do.  Then plot the thing{
c****
      
  100 CONTINUE
      DO 101 i = 1, 4
  101 ibuf(i) = 0
      CALL rddisc(ilun,prtbuf,2,istat)
      IF( istat .NE. 2) GOTO 9999
      IF( prtbuf(1:8) .NE. ' ') THEN
          DO 120 i=1,8                                                   { do each character
              c = prtbuf(i:i)
              IF( c .NE. ' ') THEN
c                                   ICHAR('0') is 48 (decimal or 30 hex)
                  j = (ICHAR(c)-48)*16                                  { get the index of the char
                  IF( j .LT. 0 .OR. j .GT. 128 ) GOTO 120
                  DO 110, k=1,16                                        { store each character raster line 
                      iorbuf(i,k) = ifonts(j+k)
  110             CONTINUE
               ENDIF
  120     CONTINUE
      ENDIF
      CALL rddisc(ilun,ibuf(5),mwrds,istat)
      IF( istat .NE. mwrds ) GOTO 9999
   
      DO 200 i=1,8                                                       { or the annotation in
         ibuf(i) = iorbuf(i,1)                                           { only 1 raster line!
  200 CONTINUE
  
      DO 220 j=1,15                                                      { move the or buffer up 1 raster line
         DO 210 i=1,8                                                    { do each character
             iorbuf(i,j) = iorbuf(i,j+1)
  210    CONTINUE
  220 CONTINUE
      DO 230 i=1,8                                                       { zero out the next rastered annotation
          iorbuf(i,16)=0
  230 CONTINUE
    
      CALL vwrite( ibuf, mwrds*4 )
      GOTO 100

c****
c****    END OF PLOT FILE
c****     
 9999 CONTINUE
      CALL vprint
      prtbuf = ' '                                                      { make a gap between the header and the data
      prtbuf(10:10) = CHAR(10)                                          { put in a CR
      DO 10000 i = 1, 10  
         CALL vwrite( prtbuf, 10 )                                      { write 10 characters
10000 CONTINUE
      CALL detach
      PRINT *,' finished the seismic plot.'
          
    
      END
