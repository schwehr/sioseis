c     Takes a SIOSEIS seismic plot file from disk and write directly to the
c  Atlantek plotter on /dev/lpv0.
c  all files for the user and assumes the Versatec is /dev/lpv0
c     This program is a beast because UNIX doesn't allow the use of the VERSATEC
c  SSP feature (simultaneous print/plot), therefore this program ors in a
c  rasterized version of the annotation.
c     Another problem is that CR/LF are need.
c
c  Copyright (C) 1985
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla, Ca. 92093
c
c  mod 12 Dec 99 - Make plot2.f into aplot.f
c  mod Feb (??) 00 - Position the disk everytime and close and reopen
c                    the file if eof is hit.
c  mod May (??) 00 - Had v.c drop the Centronics left over from Versatec days
c  mod 3 Sept 00 - Oops, The May change should have done the disk read
c                  in bytes, not 32 bit words!  That means it was
c                  plotting only 1 fourth of the raster lines.
c  mod 23 Jul 08 - Allow multiple plot files by using the first portion of
c                  each plotfile.  Start by allowing a max of two plotfiles
c                  and using the first half of each one.
c
     
c      PARAMETER (nchars=80)
      INTEGER*2 ibuf(5000)    
      CHARACTER*200 pathname1, pathname2
      CHARACTER*90 prtbuf
      DATA prtbuf/' '/, itype/7224/, nread/0/

      IF( itype .EQ. 7224 ) mwrds = 144                                 ! the number of 32 bit words per raster line
      nbytes = mwrds * 4                                                ! the number of bytes per raster line
      index1 = 1
      index2 = mwrds/2 + 1
      nargs = iargc()
      IF( nargs .LT. 1 .OR. nargs .GT. 3 ) THEN
          PRINT *,' atlantek usage: atlantek file1 [ file2 ]'
          STOP
      ENDIF
      nplotters = narg
      CALL getarg( 1, pathname1 )
      CALL getfil( 4, ilun1, pathname1, istat )                           ! open the plot file
      CALL attach
      IF( nplotters .EQ. 2 ) THEN
          CALL getarg( 2, pathname2 )
          CALL getfil( 4, ilun2, pathname2, istat )
      ENDIF
c****
c****    PRINT 44 LINES.  Line 1 is the date of the plot.  lines 2-41 are the
c****  SEGY tape header/SIOSEIS comments.  42-44 contain some plotting parameters.
c****
      DO 50 i = 1,44
         CALL rddisc( ilun1, prtbuf, 20, istat )                          ! read 20 words (80 characters)
      IF( nplotters .EQ. 2 ) CALL rddisc( ilun2, prtbuf, 20, istat )
         prtbuf(81:81) = CHAR(10)                                       ! put in a CR
c         CALL vwrite( prtbuf, 81 )
   50 CONTINUE
      prtbuf = ' '                                                      ! make a gap between the header and the data
      prtbuf(10:10) = CHAR(10)                                          ! put in a CR
      DO 70 i = 1, 10  
c         CALL vwrite( prtbuf, 10 )                                      ! write 10 characters
   70 CONTINUE
      CALL vplot
c****
  100 CONTINUE
      istat2 = nbytes
c**** Keep the plots in sync - pause/stop if either one pauses, otherwise
c**** the spacing could get messy.
      CALL adrdisc( ilun1, iaddress1 )
      CALL rddiscb( ilun1, ibuf, nbytes, istat1 )
      IF( nplotters .EQ. 2 ) THEN
          CALL adrdisc( ilun2, iaddress2 )
          CALL rddiscb( ilun2, ibuf(index2), nbytes, istat2 )
      ENDIF
      IF( istat1 .NE. nbytes .OR. istat2 .NE. nbytes ) THEN
          CALL frefil( -2, ilun1, istat )
          IF( nplotters .EQ. 2 ) CALL frefil( -2, ilun2, istat )
          CALL sleep(5)
          CALL getfil( -4, ilun1, pathname1, istat )
          IF( nplotters .EQ. 2 ) CALL getfil( -4, ilun2,pathname2,istat)
          CALL podiscb( ilun1, 1, iaddress1 )
          IF( nplotters .EQ. 2 ) CALL podiscb( ilun2, 1, iaddress2 )
          GOTO 100
      ENDIF
      CALL vwrite( ibuf, nbytes )
      GOTO 100
c****
c****    END OF PLOT FILE
c****     
 9999 CONTINUE
c      CALL vprint
      prtbuf = ' '  
      prtbuf(10:10) = CHAR(10)                                          ! put in a CR
      DO 10000 i = 1, 60
c         CALL vwrite( prtbuf, 10 )                                      ! write 10 characters
10000 CONTINUE
      CALL detach
      PRINT *,' finished the seismic plot.'
          
    
      END

