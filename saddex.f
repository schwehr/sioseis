      SUBROUTINE saddex( buf, lbuf, ibuf )      
c
c  ARGUMENTS:
c  buf   - The trace, with SEGY header as TYPE REAL
c  lbuf  - The trace, with SEGY header as TYPE INTEGER*4
c  ibuf  - The trace, with SEGY header as TYPE INTEGER*2
c
c  COPYRIGHT (C) The Regents of the University of California
c  ALL RIGHTS RESERVED.  
c  Written by Paul Henkart, SIO, August 1992
c  mod july 95 - initialize interp to 0
c
      DIMENSION buf(111), lbuf(111), ibuf(111), scr(10), lscr(111)
      INTEGER*2 ibuf
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /sadd/ lun, nlists, nwrds
      INTEGER fno, lno, ftr, ltr, fno2, ftr2
      SAVE
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      DATA mlists/0/, fno2/0/, interp/0/
c
      IF( ibuf(itridptr) .EQ. 2 ) RETURN                                ! forget dead traces
c****
c****    Get the first parameter list
c****
   10 IF( mlists .EQ. 0 ) THEN
          CALL podisc( lun, 1, 0 )                                      ! get the first parameter list from disk
          CALL rddisc( lun, scr, nwrds, istat )
          mlists = mlists + 1
          scalar = scr(1)
          fno = lscr(2)
          lno = lscr(3)
          ftr = lscr(4)
          ltr = lscr(5)
          lprint = lscr(6)
      ENDIF
      IF( lbuf(lrptrptr) .NE. 0 ) THEN
          lnum = lbuf(lrpnptr)
          ltrno = lbuf(lrptrptr)
      ELSE
          lnum = lbuf(lshotptr)
          ltrno = lbuf(lshtrptr)
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' lnum=',lnum,' ltrno=',ltrno,
     &    ' fno=',fno,' lno=',lno
      IF( lnum .LT. fno .AND. mlists .EQ. 1 ) RETURN
      IF( lnum .GT. lno .AND. mlists .GE. nlists ) RETURN
      IF( ltrno .LT. ftr .OR. ltrno .GT. ltr ) RETURN
c****
c****
c****
   20 IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' scalar=',scalar,' fno=',fno,' lno=',lno,' ftr=',ftr,
     &            ' ltr=',ltr
      ENDIF
      IF( lnum .LT. fno ) THEN                                          ! assume shot numbers always increase
          IF( mlists .EQ. 1 ) THEN
              IF( interp .EQ. 0 ) RETURN
              GOTO 500
          ENDIF
      ENDIF
      IF( lnum .LE. lno ) GOTO 500                                      ! is the shot in this list
      IF( mlists .GT. nlists ) THEN
          IF( interp .EQ. 0 ) RETURN
          GOTO 500
      ENDIF
      IF( lnum .GE. fno2 ) THEN
          IF( mlists .GT. 1 ) THEN
              scalar = scalar2
              fno = fno2
              lno = lno2
              ftr = ftr2
              ltr = ltr2
          ENDIF
          IF( mlists .GE. nlists ) RETURN
          CALL rddisc( lun, lscr, nwrds, istat )                        ! read the next parameter list
          scalar2 = scr(1)
          fno2 = lscr(2)
          lno2 = lscr(3)
          ftr2 = lscr(4)
          ltr2 = lscr(5)
          mlists = mlists + 1
          GOTO 20
      ENDIF
c****
c****   Do the scalar add
c****
  500 CONTINUE
c      nsamps = ibuf(isampptr)
      nsamps =  numdat
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' scalar add of ',scalar
      DO 600 i = 1, nsamps
         buf(numhdr+i) = buf(numhdr+i) + scalar
  600 CONTINUE
      RETURN
      END
      
