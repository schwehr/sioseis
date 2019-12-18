      SUBROUTINE getlanno( anntyp, hdr, lhdr, ihdr,
     &    buf, lbuf, ibuf, fanno, lanno )
c     Return the 8 character annotation (lanno), given annytp and the
c  SEGY header or the user's fanno.
c ANNTYP - The type of annotation to be done. A maximum of eight
c         characters are printed before the tag.
c       = 1, The annotation is taken from the user specified FANNO.
c       = 2, The annotation is the shot number of the trace plotted.
c       = 3, The annotation is the rp number of the trace plotted.
c       = 4, The annotation is the gmt time in hours, minutes and seconds
c            associated with the trace.
c       = 5, The annotation is the gmt time. Traces are annotated at even
c            intervals of ANNINC minutes.  FTAG and TAGINC are ignored.
c       = 6, The annotation is the shot-receiver distance (header word
c            10).  When used in conjunction with hscale, the annotated
c            range is adjusted for roundoff.  i.e.  The annotated range
c            is exact for placing on the plot and is not the exact range
c            for the trace.
c       = 7, The annotation is the trace number of the shot/rp.
c       = 8, The annotation is the energy source point number (header(5))
c       = 9, The first four characters are the GMT and the second four
c            characters are the rp number.
c       = 10, No annotation is done.
c         IF( token(1:6) .EQ. 'HEADER' ) anntyp = 11
c         IF( token(1:4) .EQ. 'RPTR' ) anntyp = 12
c         IF( token(1:5) .EQ. 'SH&TR' ) anntyp = 13
c         IF( token(1:5) .EQ. 'RP&TR' ) anntyp = 14
c         IF( token(1:6) .EQ. 'GMTSEC' ) anntyp = 15
c       = 16, latidude as +/-DDD MM
c       = 17, longitude as +/-DDD MM
c       = 18, latitude as MM SS SS
c       = 19, latitude as MM SS SS
c
c  mod 16 Apr 07 - 7 is the shot trace number, not the shot or rp trace number
c  mod 4 Aug 08 - Add lat & long
c  mod 22 Jul 10 - Add lat/long as MM SS SS
c  mod 23 Jul 10 - Allow lat/long in decimal degrees and DDDMMSS.ss
c  mod 1 Sep 11 - Allow 4 digit trace number on SH&TR and RP&TR
c
      INTEGER anntyp, fanno, hdr
      DIMENSION buf(60), lbuf(60), ibuf(120)
      INTEGER*2 ibuf
      REAL*8 dtemp
      CHARACTER*8 lanno, token
c
c             1   2   3   4   5   6   7   8   9   10  11  12  13  14
      GO TO (640,610,620,630,650,660,670,680,620,700,690,625,626,627,
c          15    16   17   18   19   20   21   22
     &     628, 800, 810, 800, 810, 820, 830, 840 ), ANNTYP
  610 WRITE( lanno, '(I8)' ) lbuf(3)
      RETURN
  620 WRITE( lanno, '(I8)' ) lbuf(6)
      RETURN
  625 WRITE( lanno, '(I8)' ) lbuf(7)
      RETURN
  626 WRITE(token,'(I8)') lbuf(3)
      WRITE(lanno,'(4x,I4)') lbuf(4)
      lanno(1:4) = token(5:8)
      RETURN
  627 WRITE(token,'(I8)') lbuf(6)
      WRITE(lanno,'(4x,I4)') lbuf(7)
      lanno(1:4) = token(5:8)
      RETURN
  628 WRITE(lanno,'(1x,2I2.2,1x,I2.2)') ibuf(81), ibuf(82), ibuf(83)
c    day, hour, minute
      RETURN
  630 WRITE( lanno,'(I3, 1X, 2I2.2 )') IBUF(80),IBUF(81), ibuf(82)
c    hour, minute, millisecond
      RETURN
  640 WRITE(LANNO,'(I8)') FANNO
      RETURN
c       = 5, The annotation is the gmt time. Traces are annotated at even
c            intervals of ANNINC minutes.  FTAG and TAGINC are ignored.
  650 RETURN
  660 WRITE( lanno, '(I8)' ) lbuf(10)
      RETURN
c       = 7, The annotation is the trace number of the shot/rp.
c   make that the shot trace number, not shot/rp
c  670 IF( lbuf(7) .EQ. 0 ) THEN
  670     WRITE( lanno, '(I8)' ) lbuf(4)
c      ELSE
c          WRITE( lanno, '(I8)' ) lbuf(7)
c      ENDIF
      RETURN
c       = 8, The annotation is the energy source point number (header(5))
  680 WRITE( lanno, '(I8)' ) lbuf(5)
      RETURN
  685 CONTINUE
c       = 9, The first four characters are the GMT and the second four
c            characters are the rp number.
      WRITE( lanno, '(I8)' ) lbuf(6)
      WRITE( lanno(1:4), '(2I2.2)') ibuf(81), ibuf(82)
      RETURN
  690 IF( hdr .NE. 0 ) THEN
c****  convert to integer since 'E' and '.' can't be annotated
          ltemp = NINT(buf(hdr))
          WRITE( lanno, '(I8)' ) ltemp
      ENDIF
      IF( ihdr .NE. 0 ) WRITE( lanno, '(I8)' ) ibuf(ihdr)
      IF( lhdr .NE. 0 ) WRITE( lanno, '(I8)' ) lbuf(lhdr)
C****
  700 RETURN
c****
c****    16 & 17  - lat & long
c****   ibuf(45) = 2, then seconds of arc
c****            = 3, then decimal degrees
c****            = 4, then +/-DDDMMSS.ss
c****
  800 dtemp = DABS(DFLOAT(lbuf(20)))
      IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
      IF( ibuf(36) .LT. 0 ) dtemp = DABS(dtemp / DFLOAT(ibuf(36)))
      IF( ibuf(45) .EQ. 3 ) dtemp = dtemp * 60. * 60.
      IF( ibuf(45) .EQ. 2 .OR. ibuf(45) .EQ. 3 ) THEN
          temp = dtemp
          CALL secsdms( 1, temp, ideg, min, sec )
      ENDIF
      IF( lbuf(20) .LT. 0 ) ideg = -ideg
      IF( anntyp .EQ. 16 ) WRITE( lanno, '(I4,1x,I3)' ) ideg, min
      IF( anntyp .EQ. 18 ) THEN
          itemp1 = sec
          sec = sec - itemp1
          itemp2 = INT(sec * 100.)
          WRITE( lanno, '(I2,1x,I2,1x,I2)' ) min, itemp1, itemp2
      ENDIF
      RETURN
c****
  810 dtemp = DABS(DFLOAT(lbuf(19)))
      IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
      IF( ibuf(36) .LT. 0 ) dtemp = ABS(dtemp / DFLOAT(ibuf(36)))
      IF( ibuf(45) .EQ. 3 ) dtemp = dtemp * 60. * 60.
      IF( ibuf(45) .EQ. 2 .OR. ibuf(45) .EQ. 3 ) THEN
          temp = dtemp
          CALL secsdms( 1, temp, ideg, min, sec )
      ENDIF
      IF( lbuf(19) .LT. 0 ) ideg = -ideg
      IF( anntyp .EQ. 17 ) WRITE( lanno, '(I4,1x,I3)' ) ideg, min
      IF( anntyp .EQ. 19 ) THEN
          itemp1 = sec
          sec = sec - itemp1
          itemp2 = INT(sec * 100.)
          WRITE( lanno, '(I2,1x,I2,1x,I2)' ) min, itemp1, itemp2
      ENDIF
      RETURN
C****
  820 CONTINUE
      depth = lbuf(16)
      ltemp = ibuf(35)
      scalar = FLOAT(ltemp)
      IF( scalar .LT. 0 ) depth = -depth / scalar
      IF( scalar .GT. 0 ) depth = depth * scalar
      WRITE( lanno, '(F8.2)' ) depth   !  water depth at source
      RETURN
  830 WRITE( lanno, '(F8.3)' ) buf(50)   !  water bottom time in secs
      RETURN
  840 WRITE( lanno, '(I8)' ) ibuf(17)   !  fold
      RETURN
      RETURN
      RETURN



      END

