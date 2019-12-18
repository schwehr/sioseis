c     Takes one or two SIOSEIS seismic plot files from disk and writes
c  to a file that must be linked to the plotter in /dev.
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
c  mod 8 Oct 10 - Generalize by getting the plotter type and number of
c                 bytes/words from the plot file.
c               - Use new function attach_alias (in v.c) that opens a file
c                 with the name of the plotter type rather than a device.
c                 That file should be an alias to /dev/"plotter"
c               - Eliminate the print feature that hasn't been used in 25 years.
c               - Check for the exitence of the output file
c mod 3 Mar 11 - Make the check for 7225 and 624 as well at 7224
c

c      PARAMETER (nchars=80)
      INTEGER*2 ibuf(5000)
      CHARACTER*200 pathname1, pathname2
      CHARACTER*90 prtbuf
      CHARACTER*4 atype/' '/, token/' '/
      LOGICAL iexist
      DATA prtbuf/' '/, itype/0/, nread/0/, nplotters/1/

      nargs = iargc()
      IF( nargs .LT. 1 .OR. nargs .GT. 3 ) THEN
          PRINT *,' versatec usage: versatec file1 [ file2 ]'
          STOP
      ENDIF
      nplotters = nargs
      CALL getarg( 1, pathname1 )
      CALL getfil( 4, ilun1, pathname1, istat )                           ! open the plot file
      IF( istat .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Plot file ',pathname1(1:20),
     &            ' does not exist.'
          STOP
      ENDIF
      IF( nplotters .EQ. 2 ) THEN
          CALL getarg( 2, pathname2 )
          CALL getfil( 4, ilun2, pathname2, istat )
      ENDIF
c****
c****    PRINT 44 LINES.  Line 1 is the date of the plot.  lines 2-41 are the
c****  SEGY tape header/SIOSEIS comments.  42-44 contain some plotting parameters.
c****
      CALL rddisc( ilun1, prtbuf, 20, istat )
      IF( nplotters .EQ. 2 ) CALL rddisc( ilun2, prtbuf, 20, istat )
      READ( prtbuf,'(66X,I4,I4)' ) itype, nlines
      atype = prtbuf(67:70)
      IF( itype .EQ. 7224 ) mwrds = 144
      IF( itype .EQ. 7225 ) mwrds = 147
      IF( itype .EQ. 624 ) mwrds = 296
      IF( itype .NE. 7224 .AND. itype .NE. 7225 .AND.
     &    itype .NE. 624 ) THEN
          PRINT *,' ***  ERROR  ***  Incorrect raster file type of ',
     &     itype
          PRINT *,' Only NIBS 624, 7224 or 7225 permitted.'
          CALL EXIT
      ENDIF
      nbytes = mwrds * 4                                                ! the number of bytes per raster line
      IF( itype .EQ. 624 ) THEN
          INQUIRE( FILE='624', EXIST=iexist )
          IF( .NOT. iexist ) THEN
              PRINT *,' Program VERSATEC output file does not exist.'
              PRINT *,' On the machine with the plotter, type:',
     &                ' ln -s /dev/ihcp0 624'
              CALL EXIT
          ENDIF
      ENDIF
      IF( itype .EQ. 7224 ) THEN
          INQUIRE( FILE='7224', EXIST=iexist )
          IF( .NOT. iexist ) THEN
              PRINT *,' Program VERSATEC output file does not exist.'
              PRINT *,' On the machine with the plotter, type:',
     &                ' ln -s /dev/ihcp0 7224'
              CALL EXIT
          ENDIF
      ENDIF
      IF( itype .EQ. 7225 ) THEN
          INQUIRE( FILE='7225', EXIST=iexist )
          IF( .NOT. iexist ) THEN
              PRINT *,' Program VERSATEC output file does not exist.'
              PRINT *,' On the machine with the plotter, type:',
     &                ' ln -s /dev/ihcp0 7225'
              CALL EXIT
          ENDIF
      ENDIF
      CALL attach_alias( atype )
      DO 50 i = 1,43
         CALL rddisc( ilun1, prtbuf, 20, istat )                          ! read 20 words (80 characters)
      IF( nplotters .EQ. 2 ) CALL rddisc( ilun2, prtbuf, 20, istat )
   50 CONTINUE
      index1 = 1
      index2 = mwrds
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
c****     Use the SIOSEIS negative number in file IN/in convention of tapes.
          INQUIRE( FILE='in', EXIST=iexist )
          IF( iexist ) THEN
              OPEN(UNIT=20,FILE='in')
              READ(20,'(A1)',END=150) token(1:1)
              IF( token(1:1) .EQ. '-' ) THEN
                  CALL detach
                  PRINT *,' finished the seismic plot.'
                  CLOSE(20,STATUS='DELETE')
                  CALL EXIT
              ENDIF
  150         CLOSE(20)
          ENDIF
          CALL sleep(4)
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


      END

