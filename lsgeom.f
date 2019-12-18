C    A PROGRAM TO READ AND LIST A SEGY FORMATTED DISK
      INTEGER*2 ibuf
      CHARACTER*80 name, token
      DIMENSION BUF(30000),IBUF(30000),LBUF(30000)
      EQUIVALENCE (BUF(1),IBUF(1)),(BUF(1),LBUF(1))
      DATA icompt/7/, ndone/0/, long/0/

      nargs = iargc()
      IF( nargs .LT. 1 .OR. nargs .GT. 3 ) THEN
          PRINT *,' lsgeom (list XY coords in SEG-Y disk file) usage:',
     &      ' lsgeom SEGY-filename [trace-inc ntodo]'
          STOP
      ENDIF
      CALL getarg( 1, name )
      CALL getfil( 4, lun, name, istat )
      IF( istat .NE. 0 ) STOP
      nskip = 1
      ntodo = 9999999
      IF( nargs .GT. 1 ) THEN
          CALL getarg( 2, token )
          READ( token, * ) nskip
          IF( nargs .GT. 2 ) THEN
              CALL getarg( 3, token )
              READ( token, * ) ntodo
          ENDIF
      ENDIF
      IF( nskip .GT. 0 ) NSKIP=NSKIP-1
      IF( is_big_endian() .LT. 0 ) icompt = 4
      CALL rddisc( lun, buf, 800, istat )
      IF( istat .NE. 800 ) THEN
          PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *        ' wanted 800 words, read ',istat,' words.'
          STOP
      ENDIF
      CALL rddisc( lun, buf, 100, istat )
      IF( istat .NE. 100 ) THEN
          PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *            ' wanted 100 words, read ',istat,' words.'
          STOP
      ENDIF
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( ibuf, 100 )
      idtype = ibuf(13)
      PRINT 40
40    FORMAT('     Shot  Tr    SourceX    SourceY ReceiverX',
     &   ' ReceiverY Scalar   yr day hr min sec mil')
  110 CONTINUE
  120 CALL rddisc( lun, buf, 60, istat)
      IF( istat .EQ. -1 ) STOP
      IF( istat .NE. 60 ) THEN
          PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *            ' wanted 60 words, read ',istat,' words.'
          STOP
      ENDIF
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
          CALL swap32( lbuf(1), 7 )
          CALL swap16( ibuf(15), 1 )
          CALL swap32( lbuf(10), 1 )
          CALL swap32( lbuf(16), 1 )
          CALL swap16( ibuf(35), 2 )
          CALL swap32( lbuf(19), 4 )
          CALL swap16( ibuf(45), 5 )
          CALL swap16( ibuf(55), 5 )
          CALL swap16( ibuf(79), 6 )
          CALL swap16( ibuf(110), 1 )
      ENDIF
      IF( ibuf(58) .NE. 32767 ) THEN
          nsamps = ibuf(58)
      ELSE
          nsamps = lbuf(58)
      ENDIF
      IF( nsamps .LT. 0 ) nsamps = nsamps + 65537
      IF( nsamps .EQ. 0 ) STOP
      IF( ibuf(45) .EQ. 2 ) THEN
          temp = lbuf(19)
          IF( ibuf(36) .GT. 0 ) temp = temp * REAL(ibuf(36))
          IF( ibuf(36) .LT. 0 ) temp = ABS(temp / REAL(ibuf(36)))
          CALL secsdms( 1, temp, longdeg, longmin, seclong )
          IF( lbuf(19) .LT. 0 ) longdeg = -longdeg
          temp = lbuf(20)
          IF( ibuf(36) .GT. 0 ) temp = temp * REAL(ibuf(36))
          IF( ibuf(36) .LT. 0 ) temp = ABS(temp / REAL(ibuf(36)))
          CALL secsdms( 1, temp, latdeg, latin, seclat )
          IF( lbuf(20) .LT. 0 ) latdeg = -latdeg
          PRINT 49,LBUF(3),LBUF(4), longdeg, longmin, seclong,
     &     latdeg, latin, seclat,
     *        (IBUF(I),I=79,84)
   49     FORMAT(I9,I4,1x,2(I4,1x,I2,1x,F3.0),27x,I4,I4,I3,I4,I4,I4)
      ELSE
          PRINT 50,LBUF(3),LBUF(4), (lbuf(i),i=19,22), ibuf(36),
     *        (IBUF(I),I=79,84)
   50     FORMAT(I9,I4,1x,4(1x,I9),I7,1x,I4,I4,I3,I4,I4,I4)
      ENDIF
      ndone = ndone + 1
      IF( ndone .GE. ntodo ) STOP
      IF( idtype .EQ. 3 .OR. idtype .EQ. 4 ) THEN
          nbytes = nsamps * 2
      ELSE
          nbytes = nsamps * 4
      ENDIF
      CALL rddiscb( lun, buf, nbytes, istat )
      IF( istat .NE. nbytes ) THEN
          PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *        ' wanted',nbytes,' bytes, read ',istat,' bytes.'
          STOP
      ENDIF
      IF( NSKIP .GT. 0 ) THEN
          DO 150 I=1,NSKIP
              CALL rddisc( lun, buf, 60, istat)
              IF( istat .EQ. -1 ) STOP
              IF( istat .NE. 60 ) THEN
                  PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *            ' wanted 60 words, read ',istat,' words.'
                  STOP
              ENDIF
              IF(icompt.EQ.2.OR.icompt.EQ.4) CALL swap16(ibuf(58),1)
              IF( ibuf(58) .NE. 32767 ) THEN
                  nsamps = ibuf(58)
              ELSE
                  nsamps = lbuf(58)
              ENDIF
              IF( nsamps .EQ. 0 ) STOP
              IF( idtype .EQ. 3 .OR. idtype .EQ. 4 ) THEN
                  nbytes = nsamps * 2
              ELSE
                  nbytes = nsamps * 4
              ENDIF
              CALL rddiscb( lun, buf, nbytes, istat )
              IF( istat .NE. nbytes ) THEN
                  PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *                ' wanted',nbytes,' bytes, read ',istat,' bytes.'
                  STOP
              ENDIF
  150     CONTINUE
      ENDIF
      GO TO 110
      END
