c     This version of plot takes a UNIX SIOSEIS seismic plot file from a
c  SIOSEIS disc file and writes directly to the SUN screen.
c
c  Copyright (C) 1989
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c
     
      PARAMETER (nchars=80)
      INTEGER*2 ibuf(5000)    
      INTEGER*4 lbuf(10)
      EQUIVALENCE (ibuf(1),lbuf(1))
      CHARACTER*80 token
      DATA ndone/0/

    1 PRINT *,' Enter the value used for SIOSEIS plot parameter nibs.'
      READ *, itype
      IF( itype .EQ. 60 .OR. itype .EQ. 100 .OR. itype .EQ. 80 .OR.
     &    itype .EQ. 4160 .OR. itype .EQ. 120 ) THEN
          PRINT *,' PLOT3 does not work on that type of plotter,'
          STOP
      ENDIF
      IF( itype .NE. 160 .AND. itype .NE. 200 .AND.
     *    itype .NE. 8122. AND. itype .NE. 8222 .AND.
     *    itype .NE. 201 .AND. itype .NE. 5732 .AND.
     *    itype .NE. 7222 .AND. itype .NE. 7224 .AND.
     *    itype .NE. 7236 .AND. itype .NE. 7244 .AND.
     *    itype .NE. 7422 .AND. itype .NE. 7424 .AND.
     *    itype .NE. 7436 .AND. itype .NE. 7444 .AND.
     *    itype .NE. 8625 .AND. itype .NE. 128 .AND.
     *    itype .NE. 850 .AND. itype .NE. 7600 .AND.
     *    itype .NE. 2847 .AND. itype .NE. 2848 .AND.
     *    itype .NE. 2858 .AND. itype .NE. 2859 .AND.
     *    itype .NE. 3444
     *   ) THEN
          PRINT *,' Incorrect answer, try again.'
          GOTO 1
      ENDIF
      istart = 5
       IF( itype .EQ. 160 ) mwrds = 90                                  ! the number of 32 bit words per raster line
      IF( itype .EQ. 128 ) mwrds = 40
      IF( itype .EQ. 200 ) mwrds = 66
      IF( itype .EQ. 201 ) mwrds = 146
      IF( itype .EQ. 300 ) mwrds = 328
      IF( itype .EQ. 850 ) mwrds = 54
      IF( itype .EQ. 2847 .OR. itype .EQ. 2858 ) mwrds = 225
      IF( itype .EQ. 2848 .OR. itype .EQ. 2859 ) mwrds = 328
      IF( itype .EQ. 3444 ) mwrds = 538
      IF( itype .EQ. 5732 ) mwrds = 147
      IF( itype .EQ. 8122 ) mwrds = 66
      IF( itype .EQ. 8222 ) mwrds = 132
      IF( itype .EQ. 7222 ) mwrds = 132
      IF( itype .EQ. 7224 ) mwrds = 144
      IF( itype .EQ. 7225 ) mwrds = 147
      IF( itype .EQ. 7236 ) mwrds = 220
      IF( itype .EQ. 7244 ) mwrds = 269
      IF( itype .EQ. 7422 ) mwrds = 264
      IF( itype .EQ. 7424 ) mwrds = 288
      IF( itype .EQ. 7425 ) mwrds = 294
      IF( itype .EQ. 7436 ) mwrds = 440
      IF( itype .EQ. 7444 ) mwrds = 538
      IF( itype .EQ. 7600 ) THEN
          mwrds = 448
          istart = 21
      ENDIF
      IF( itype .EQ. 7600 .OR. itype .EQ. 3444 .OR.
     &    itype .EQ. 2848 .OR. itype .EQ. 2859 ) THEN
          PRINT *,' Which plane do you want? (0 means all)'
          READ (*,*) iplane
      ENDIF
      IF( itype .EQ. 8122 ) mwrds = 132*2
      IF( itype .EQ. 8222 ) mwrds = 264*2
      IF( itype .EQ. 8242 ) mwrds = 512*2
      IF( itype .EQ. 8625 ) mwrds = 294
      IF( itype .EQ. 9242 ) mwrds = 500*2
 
      PRINT *,' Skip how many horizontal raster lines?'
      READ *, nhskip
      PRINT *,' Skip how many dots vertically?'
      READ *, nvskip
      IF( nvskip .NE. 0 ) istart = nvskip / 16 + 1
      nbytes = mwrds * 4                                                { the number of bytes per raster line
      nwrds = mwrds + mwrds
      PRINT *,' Enter the name of the input SIOSEIS plot file:'
      CALL rdline
      CALL getoke( token, nchars )
      token( nchars+1:nchars+1) = CHAR(0)
      CALL getfil( 4, ilun, token, istat )                               { open the plot file

c****
c****    throw away 44 LINES.  Line 1 is the date of the plot.  lines 2-41 are the
c****  SEGY tape header/SIOSEIS comments.  42-44 contain some plotting parameters.
c****
      DO 50 i = 1,44
         CALL rddisc( ilun, prtbuf, 20, istat )                          { read 20 words (80 characters)
   50 CONTINUE
      CALL scopen( 1152, 0 )
c****
  100 CONTINUE
      CALL rddisc( ilun, ibuf, mwrds, istat )
      IF( istat .NE. mwrds ) GOTO 9999
      ndone = ndone + 1
      IF( ndone .LT. nhskip ) GOTO 100
      IF( itype .EQ. 7600 .OR. itype .EQ. 3444 .OR.
     &    itype .EQ. 2848 .OR. itype .EQ. 2859 ) THEN
          IF( iplane .GT. 0 ) THEN
              itemp = MOD(ndone,3)
              IF( itemp .EQ. 0 ) itemp = 3
              IF( itemp .NE. iplane ) GOTO 100
          ENDIF
      ENDIF
      CALL scplot( ibuf(istart), 56)
      GOTO 100
c****
c****    END OF PLOT FILE
c****     
 9999 CONTINUE
c      IF( ndone .LT. 1152 ) THEN
c          DO 10000 i = 1, 28
c10000     lbuf(i) = 0
c          DO 10100 i = 1, 1152-ndone
c             CALL scplot( ibuf(1), 56)
c10100     CONTINUE
c      ENDIF
      CALL scclose
      END

