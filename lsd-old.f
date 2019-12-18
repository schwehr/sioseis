C    A PROGRAM TO READ AND LIST A SEGY FORMATTED DISK
c  -K = list Knudsen header items.
c  -O = Odec - NOT IMPLEMENTED
c
c  mod 5 Feb 14 - honor segy rev 1 delay scalar
c  mod ??????   - honor segy rev 1 water bottom scalar
c
      INTEGER*2 ibuf
      CHARACTER*200 name, token
      REAL*8 dtemp
      DIMENSION BUF(70000),IBUF(222),LBUF(222)
      EQUIVALENCE (BUF(1),IBUF(1)),(BUF(1),LBUF(1))
      DATA icompt/7/, ndone/0/, long/0/, ntodo/999999/
      LOGICAL knudsen/.FALSE./, odec/.FALSE./


      nargs = iargc() + 1
      CALL getarg( 1, name )
      numarg = 2
      IF( name(1:1) .EQ. '-' ) THEN
          IF( name(2:2) .EQ. 'K' .OR. name(2:2) .EQ. 'k' )knudsen=.TRUE.
          IF( name(2:2) .EQ. 'O' ) odec = .TRUE.
          CALL getarg( numarg, name )
          numarg = numarg + 1
      ENDIF
      IF( nargs .LT. 2 .OR. nargs .GT. 6 ) THEN
          PRINT *,' lsd (list SEG-Y disk file) usage:',
     &      ' lsd [-K] SEGY-filename [trace-inc ntodo long-list]'
          STOP
      ENDIF
      CALL getfil( 4, lun, name, istat )
      IF( istat .NE. 0 ) THEN
          PRINT *,' Could not open file:   ',name(1:50)
          STOP
      ENDIF
      IF( is_big_endian() .LT. 0 ) icompt = 4
      nskip = 1
      IF( nargs .GT. numarg ) THEN
          CALL getarg( numarg, token )
          numarg = numarg + 1
          READ( token, * ) nskip
          IF( nargs .GT. numarg ) THEN
              CALL getarg( numarg, token )
              numarg = numarg + 1
              READ( token, * ) ntodo
          ENDIF
          IF( nargs .GT. numarg ) THEN
              CALL getarg( numarg, token )
              numarg = numarg + 1
              long = 1
          ENDIF
      ENDIF
      IF( nskip .GT. 0 ) NSKIP=NSKIP-1
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
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16( ibuf, 200 )
      idtype = ibuf(13)
      segyrev = REAL(ibuf(151)) / 256.
      ltemp = ibuf(153) * 3200
      IF( ibuf(151) .GT. 256 .AND. ibuf(151) .LT. 512 .AND.
     &     ibuf(153) .GT. 0 ) CALL podiscb( lun, 2, ltemp )
c****  This will fail if the number of headers is unknown (ibuf(153) = -1)
      PRINT 40
40    FORMAT('      SHOT   TR       RP    TR ID  RANGE ',
     *   '  DELAY NSAMPS    SI   YR DAY HR MIN SEC')
50    FORMAT(I10,I6,I8,I6,I3,I7,F8.2,I7,I6,I5,I4,I3,I4,I4)
  110 CONTINUE
      CALL rddisc( lun, buf, 60, istat)
      IF( istat .EQ. -1 ) THEN
          PRINT *,' Read End-Of-File.'
          STOP
      ENDIF
      IF( istat .NE. 60 ) THEN
          PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *            ' wanted 60 words, read ',istat,' words.'
          STOP
      ENDIF
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
          CALL swap32( lbuf, 7 )
          CALL swap16( ibuf(15), 4 )
          CALL swap32( lbuf(10), 8 )
          CALL swap16( ibuf(35), 2 )
          CALL swap32( lbuf(19), 4 )
          CALL swap16( ibuf(45), 40 )  ! 45-84, bytes 90-168
          CALL swap16( ibuf(85), 35 )  ! rev 1 did away with the sioseis use of "unassigned"
c          IF( .NOT. knudsen ) THEN
c              CALL swap32( lbuf(46), 8 )
c              CALL swap16( ibuf(110), 1 )
c          ENDIF
      ENDIF
      CALL ushort2long( ibuf(58), nsamps )
      delay = ibuf(55)
      IF( ibuf(108) .NE. 0 .AND. segyrev .GE. 1. ) THEN    ! rev 1 delay scalar
          IF( ibuf(108) .GT. 0 ) THEN
              delay = delay * ibuf(108)
          ELSE
              delay = delay / (-1 * ibuf(108) )
         ENDIF
      ENDIF
c***  a negative delay is possible
      PRINT 50,LBUF(3),LBUF(4),LBUF(6),LBUF(7),IBUF(15),LBUF(10),
     *        delay,nsamps,IBUF(59),(IBUF(I),I=79,83)
      IF( long .NE. 0 ) THEN
          relev = REAL(lbuf(11))
          selev = REAL(lbuf(12))
          sdep = REAL(lbuf(13))
          wdep = REAL(lbuf(16))
          IF( ibuf(35) .GT. 0 ) THEN
              relev = relev * REAL(ibuf(35))
              selev = selev * REAL(ibuf(35))
              sdep = sdep * REAL(ibuf(35))
              wdep = wdep * REAL(ibuf(35))
          ENDIF
          IF( ibuf(35) .LT. 0 ) THEN
              relev = -relev / REAL(ibuf(35))
              selev = -selev / REAL(ibuf(35))
              sdep = -sdep / REAL(ibuf(35))
              wdep = -wdep / REAL(ibuf(35))
          ENDIF
          PRINT *,' shot',lbuf(3),' tr',lbuf(4),' src',lbuf(5),
     &        ' rp',lbuf(6),' tr',lbuf(7),' id',ibuf(15),' eog',lbuf(51)
          PRINT *,' fold',ibuf(17),' range',lbuf(10),' relev',relev,
     &        ' selev',selev,' sdep',sdep,' wdep',wdep
          PRINT *,' time switch or shot time millisecond ',ibuf(84)
c****   ibuf(45) = 2, then seconds of arc
c****            = 3, then decimal degrees
c****            = 4, then +/-DDDMMSS.ss
          IF( lbuf(19)+lbuf(20) .NE. 0 ) THEN
              IF( ibuf(45) .EQ. 2 ) THEN
                PRINT *,' Coordinates in seconds of arc and had scalar',
     &                    ibuf(36)
                  dtemp = DABS(DFLOAT(lbuf(19)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  temp = dtemp
                  CALL secsdms( 1, temp, ideg, min, sec )
                  IF( lbuf(19) .LT. 0 ) ideg = -ideg
                  PRINT *,' source long ',ideg, min, sec,
     &                    '(',temp,'arcsecs)'
                  dtemp = DABS(DFLOAT(lbuf(20)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  temp = dtemp
                  CALL secsdms( 1, temp, ideg, min, sec )
                  IF( lbuf(20) .LT. 0 ) ideg = -ideg
                  PRINT *,' source lat. ',ideg, min, sec,
     &                    '(',temp,'arcsecs)'
              ELSEIF( ibuf(45) .EQ. 3 ) THEN
               PRINT *,' Coordinates in decimal degrees and had scalar',
     &                    ibuf(36)
                  PRINT *,' Source and receiver xy coordinates:',
     &                  buf(19), buf(20), buf(21), buf(22)
                  dtemp = (ABS(buf(19)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  temp = dtemp * 3600.
                  CALL secsdms( 1, temp, ideg, min, sec )
                  IF( buf(19) .LT. 0 ) ideg = -ideg
                  PRINT *,' source long. ',ideg, min, sec,
     &                '(',temp,'arcsecs)'
                  dtemp = (ABS(buf(20)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  temp = dtemp * 3600.
                  CALL secsdms( 1, temp, ideg, min, sec )
                  IF( buf(20) .LT. 0 ) ideg = -ideg
                  PRINT *,' source lat. ',ideg, min, sec,
     &                '(',temp,'arcsecs)'
              ELSEIF( ibuf(45) .EQ. 1 ) THEN
                  PRINT *,' Coordinates in length and had scalar',
     &                    ibuf(36)
                  dtemp19 = DABS(DFLOAT(lbuf(19)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  dtemp20 = DABS(DFLOAT(lbuf(20)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  PRINT *,' Source coordinates: ', dtemp19, dtemp20
                  dtemp21 = DABS(DFLOAT(lbuf(21)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  dtemp22 = DABS(DFLOAT(lbuf(22)))
                  IF( ibuf(36) .GT. 0 ) dtemp = dtemp * DFLOAT(ibuf(36))
                  IF(ibuf(36) .LT. 0) dtemp=DABS(dtemp/DFLOAT(ibuf(36)))
                  PRINT *,' Receiver coordinates: ', dtemp21, dtemp22
              ELSE
                  PRINT *,' Coordinate units:',ibuf(45),' scalar:',
     &               ibuf(36)
                  PRINT *,' Source coordinates: ', lbuf(19), lbuf(20)
                  PRINT *,' Receiver coordinates: ', lbuf(21), lbuf(22)
              ENDIF
          ENDIF
          IF( ibuf(64)+ibuf(65) .NE. 0 ) THEN
              PRINT *,' Correlation switch (1=no, 2=yes):', ibuf(63)
              PRINT *,' Sweep frequency at start (Hz).', ibuf(64)
              PRINT *,' Sweep frequency at end (Hz).', ibuf(65)
          ENDIF
          IF( ibuf(66) .NE. 0 ) PRINT *,' Sweep length:',ibuf(66)
          IF( knudsen ) THEN
c****         Valid on raw files only.  sioseis diskox doesn't swap the same way.
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &            CALL swap16( ibuf(95), 20 )
              PRINT *,' PingStartTimeMS:',ibuf(95)
              PRINT *,' TxPower:',ibuf(96)
              PRINT *,' RxGain:',ibuf(97)
              PRINT *,' ProcessingGain:',ibuf(98)
              PRINT *,' Sensitivity:',ibuf(99)
              PRINT *,' MuxChannel:',ibuf(100)
              PRINT *,' EchoStrength:',ibuf(101)
              PRINT *,' PrimaryChannel:',ibuf(102)
              PRINT *,' Kel_PulseLength parameter:',ibuf(103)
              PRINT *,' TxBlank:',ibuf(104)
              PRINT *,' SoundSpeed:',ibuf(105)
              PRINT *,' StartDepth:',ibuf(106)
              PRINT *,' EndDepth:',ibuf(107)
              PRINT *,' EchoThreshold:',ibuf(108)
              PRINT *,' Heave:',ibuf(109)
              PRINT *,' HeaveLatency:',ibuf(110)
              PRINT *,' GpsLatency:',ibuf(111)
              PRINT *,' EventMarkCode:',ibuf(112)
              PRINT *,' EventMarkNumber:',ibuf(113)
              PRINT *,' Scaler:',ibuf(114)
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &            CALL swap32( lbuf(58), 1 )
              IF( ibuf(114) .LT. 0 ) 
     &            temp = - FLOAT(lbuf(58)) / FLOAT(ibuf(114))
              PRINT *,' DataRate:',temp
          ENDIF
      ENDIF
      ndone = ndone + 1
      IF( ndone .GE. ntodo ) STOP
      IF( idtype .EQ. 3 .OR. idtype .EQ. 4 ) THEN
          nbytes = nsamps * 2
      ELSE
          nbytes = nsamps * 4
      ENDIF
      IF( nsamps .GT. 0 ) THEN
          CALL rddiscb( lun, buf, nbytes, istat )
          IF( istat .NE. nbytes ) THEN
              PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *            ' wanted',nbytes,' bytes, read ',istat,' bytes.'
              STOP
          ENDIF
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
              CALL ushort2long( ibuf(58), nsamps )
              IF( ibuf(15) .EQ. 28 .AND. nsamps .NE. 1024 )
     &           nsamps = (2048 - 240) / 4
              IF( idtype .EQ. 3 .OR. idtype .EQ. 4 ) THEN
                  nbytes = nsamps * 2
              ELSE
                  nbytes = nsamps * 4
              ENDIF
              IF( nsamps .GT. 0 ) THEN
                  CALL rddiscb( lun, buf, nbytes, istat )
                  IF( istat .NE. nbytes ) THEN
                      PRINT *,' ***  ERROR  ***  disk file incorrect.',
     *                  ' wanted',nbytes,' bytes, read ',istat,' bytes.'
                      STOP
                  ENDIF
              ENDIF
  150     CONTINUE
      ENDIF
      GO TO 110
      END
