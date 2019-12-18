      PROGRAM ascii2segy
c   Read an ascii file where each line is a trace (line feed terminates the trace).
c
      CHARACTER*80 filename, token
      INTEGER lunin, lunout, istat, nargs, iargc
      INTEGER*2 head2(200), itrhdr(120)
      REAL areal
      CHARACTER*3200 head1
      DIMENSION ltrhdr(60), buf(10000)
      EQUIVALENCE (ltrhdr(1),itrhdr(1))
      COMMON /sioln2/ ICHAR, NCHARS, iprint, lunpo
      INTEGER ichar, nchars, iprint, i, lunpo
      DATA lunin/10/, lunout/11/
c
      nargs = iargc()
      IF( nargs .LT. 1 ) THEN
          PRINT *,' Incorrect ascii2segy usage.'
          PRINT *,' Usage:     ascii2segy filename'
          STOP
      ENDIF
      CALL getarg( 1, filename )
      OPEN( UNIT=lunin, FILE=filename, STATUS='OLD',
     &      IOSTAT=istat, FORM='FORMATTED' )
      IF( istat .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Could not open file:  ',filename
          istop = -1
          RETURN
      ENDIF
      filename = 'out.segy'
      CALL getfil( -3, lunout, filename, istat )
      head1 = ' '
      CALL wrdiscb( lunout, head1, 3200 )
      DO i = 1, 200
         head2(i) = 0
      ENDDO
      head2(13) = 5
      CALL swap16( head2, 200 )
      CALL wrdiscb( lunout, head2, 400 )
      DO i  = 1, 60
         ltrhdr(i) = 0
      ENDDO
      itrhdr(15) = 1
      CALL swap16( itrhdr(15), 1 )
      itrhdr(59) = 300   ! .3 ft
      CALL swap16( itrhdr(59), 1 )
      ltrhdr(3) = 1
      CALL swap32( ltrhdr(3), 1 )
      itrno = 0

      DO i = 1, 10000
         numdat = 0
         itrno = itrno + 1
         DO j = 1, 10000
            buf(j) = 0.
         ENDDO
         CAll rline( lunin )
         IF( nchars .LE. 0 ) STOP
         DO j = 1, 10000
            CALL getoke( token, nchars1 )
            IF( nchars1 .GT. 0 ) THEN
                numdat = numdat + 1
                CALL dcode( token,nchars1,buf(numdat),istat)
                IF( istat .NE. 2 ) THEN
                    PRINT *,' ***  ERROR  ***  sample ',numdat,
     &                     token(1:nchars),' is not numeric.'
                    STOP
                ENDIF
            ENDIF
         ENDDO
         itrhdr(58) = numdat
         CALL swap16( itrhdr(58), 1 )
         ltrhdr(4) = itrno
         CALL swap32( ltrhdr(4), 1 )
         CALL wrdiscb( lunout, ltrhdr, 240 )
         CALL swap32( buf, numdat )
         CALL wrdisc( lunout, buf, numdat )
      ENDDO
      END




