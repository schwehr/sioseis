      SUBROUTINE ptrlst( lun, ntrlist, trlist, ltrhdr, itrhdr, trhdr )
      INTEGER trlist(*), ltrhdr(*)
      INTEGER*2 itrhdr(*)
      REAL trhdr(*)
      REAL*8 deg, dscalar
      CHARACTER*120 line
c
c  Written August 2009, replacing ptrlist.c because I couldn't figure out how
c      to make c and fortran to make nice with writing to a file rather than stdout
c
c    mod 17 Sept 09 - g95 doesn't like FLOAT(short)
c    mod 6 Oct 11 - Add fold (type 32)
c    mod 11 Sep 12 - Read decimal degrees correctly
c                  - Receiver X & Y used source
      j = 1
      line = ' '
      DO i = 1, ntrlist
         IF( trlist(i) .EQ. 2 ) THEN   ! shotno
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(3)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 3 ) THEN   ! rpno
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(6)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 4 ) THEN   ! day + GMT
             WRITE( line(j:j+9), '(I3.1,1H ,2I2.2,1Hz)' )
     &        itrhdr(80),itrhdr(81),itrhdr(82)
             j = j + 10
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 6 ) THEN   ! range
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(10)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 7 ) THEN   ! shottr
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(4)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 8 ) THEN   ! espn
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(5)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 12 ) THEN   ! rptr
             WRITE( line(j:j+7), '(I7.1)' ) ltrhdr(7)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 28 ) THEN   ! Water bottom time at source
             depth = ltrhdr(16)
             ltemp = itrhdr(35)
c             scalar = FLOAT(itrhdr(35))
             scalar = FLOAT(ltemp)
             IF( scalar .LT. 0 ) depth = -depth / scalar
             IF( scalar .GT. 0 ) depth = depth * scalar
             WRITE( line(j:j+7), '(F7.2)' ) depth
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 29 ) THEN   ! Water bottom time at receiver
             depth = ltrhdr(17)
c             scalar = FLOAT(itrhdr(35))
             ltemp = itrhdr(35)
             scalar = FLOAT(ltemp)
             IF( scalar .LT. 0 ) depth = -depth / scalar
             IF( scalar .GT. 0 ) depth = depth * scalar
             WRITE( line(j:j+7), '(F7.2)' ) depth
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 30 ) THEN   ! water bottom time
             WRITE( line(j:j+7), '(F7.3)' ) trhdr(50)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 31 ) THEN   ! deep water delay
             WRITE( line(j:j+7), '(F7.3)' ) trhdr(46)
             j = j + 8
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 32 ) THEN   ! fold
             WRITE( line(j:j+7), '(I5.1)' ) itrhdr(17)
             j = j + 6
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 15 ) THEN   ! GMT + sec
             WRITE( line(j:j+8), '(2I2.2,1Hz,1x,I2.2)' )
     &        itrhdr(81),itrhdr(82),itrhdr(83)
             j = j + 9
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 16 ) THEN   !  SXD
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(19)
             ELSE
                 deg = DFLOAT(ltrhdr(19))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             WRITE( line(j:j+11), '(F11.6)' ) deg
             j = j + 12
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 17 ) THEN   !  SYD
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(20)
             ELSE
                 deg = DFLOAT(ltrhdr(20))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             WRITE( line(j:j+11), '(F11.6)' ) deg
             j = j + 12
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 18 ) THEN   !  SXDM
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(19)
             ELSE
                 deg = DFLOAT(ltrhdr(19))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.
             WRITE( line(j:j+12), '(I4,1x,F7.3)' ) ideg, rmin
             j = j + 13
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 19 ) THEN   !  SYDM
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(20)
             ELSE
                 deg = DFLOAT(ltrhdr(20))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.D0
             WRITE( line(j:j+12), '(I4,1x,F7.3)' ) ideg, rmin
             j = j + 13
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 20 ) THEN   !  SXDMS
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(19)
             ELSE
                 deg = DFLOAT(ltrhdr(19))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.
             min = rmin
             deg = deg - (DFLOAT(min)/60.D0)
             sec = deg * 3600.D0
             WRITE( line(j:j+14), '(I4,1x,I2.1,1x,F6.3)' ) ideg, min,sec
             j = j + 15
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 21 ) THEN   !  SYDMS
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(20)
             ELSE
                 deg = DFLOAT(ltrhdr(20))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.D0
             min = rmin
             deg = deg - (DFLOAT(min)/60.)
             sec = deg * 3600.
             WRITE( line(j:j+15), '(I4,1x,I2.1,1x,F6.3)' ) ideg, min,sec
             j = j + 14
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 22 ) THEN   !  RXD
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(21)
             ELSE
                 deg = DFLOAT(ltrhdr(21))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             WRITE( line(j:j+9), '(F9.5)' ) deg
             j = j + 10
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 23 ) THEN   !  RYD
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(22)
             ELSE
                 deg = DFLOAT(ltrhdr(22))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             WRITE( line(j:j+9), '(F9.5)' ) deg
             j = j + 10
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 24 ) THEN   !  RXDM
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(21)
             ELSE
                 deg = DFLOAT(ltrhdr(21))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.D0
             WRITE( line(j:j+11), '(I4,1x,F7.3)' ) ideg, rmin
             j = j + 12
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 25 ) THEN   !  RYDM
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(22)
             ELSE
                 deg = DFLOAT(ltrhdr(22))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.D0
             WRITE( line(j:j+11), '(I4,1x,F7.3)' ) ideg
             j = j + 12
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 26 ) THEN   !  RXDMS
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(21)
             ELSE
                 deg = DFLOAT(ltrhdr(21))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = deg * 60.
             min = rmin
             deg = deg - (DFLOAT(min)/60.0D0)
             sec = deg * 3600.
             WRITE( line(j:j+13), '(I4,1x,I2,1x,F6.3)' ) ideg,min,sec
             j = j + 14
             GOTO 100
         ENDIF
         IF( trlist(i) .EQ. 27 ) THEN   !  RYDMS
             IF( itrhdr(45) .EQ. 3 ) THEN
                 deg = trhdr(22)
             ELSE
                 deg = DFLOAT(ltrhdr(22))
             ENDIF
             IF( itrhdr(45) .EQ. 2 ) deg = deg / 3600.D0
             dscalar = DFLOAT( itrhdr(36) )
             IF( dscalar .LT. 0 ) deg = -deg / dscalar
             IF( dscalar .GT. 0 ) deg = deg * dscalar
             ideg = deg
             IF( deg .GT. 0 ) THEN
                 deg = deg - DFLOAT(ideg)
             ELSE
                 deg = -deg + DFLOAT(ideg)
             ENDIF
             rmin = DFLOAT(ideg) * 60.D0
             min = rmin
             deg = deg - (DFLOAT(min)/60.D0)
             sec = deg * 3600.D0
             WRITE( line(j:j+13), '(I4,1x,I2,1x,F6.3)' ) ideg,min,sec
             j = j + 14
             GOTO 100
         ENDIF

  100    CONTINUE
      ENDDO
      WRITE( lun, '(A)' ) line(1:j)
      RETURN
      END
