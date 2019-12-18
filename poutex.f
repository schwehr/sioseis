      SUBROUTINE POUTEX(BUF,LBUF,IBUF,SCR,LSCR,ISCR,istop)
C     POUTEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS PRINTER
C  OUTPUT.  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /POUT/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORARY ARRAY BUF.  PRINT WINDOW TIMES FOR TRACES BETWEEN
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE PRINTED, INCLUDING THE TRACE HEADER.  THE FIRST
C           DATA SAMPLE MUST BE AT TIME DELAY.  THIS IS THE FLOATING
C           POINT (REAL) TRACE ARRAY.
C  LBUF   - THE LONG INTEGER TRACE ARRAY.  THIS IS REALLY THE SAME AS BUF, BUT
C           PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING ANYTHING TO AN ARGUMENT.
C  IBUF   - THE SHORT INTEGER TRACE ARRAY.  NEEDED FOR 16 BIT TRACE HEADER
C           ADDRESSES.
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.  THEREFORE, SCR MUST
C           BE AT LEAST 56 32BIT WORDS BIG.  SCR MAY BE DESTROYED BY THE CALLING
C           ROUTINE.
C  LSCR   - THE SAME SCRATCH ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, SEPTEMBER 1980
C
c  mod 4 August 1998 - Add parameter indices.
c  mod 20 Jan 99 - Don't print the header line when printing indices.
c  mod 7 Apr 00 - FORMAT didn't work with INDICES.
c  mod 23 May 00 - Use NUMDAT rather than ibuf(58) for nsamps
c  mod 2 Jul 01 - Change rddisc into lscr from scr because an SGI
c                 (arad3d) seemed to outoptimize (incorrectly)
c  modified 1 May 92 for the FORMAT parameter
c  mod 26 May 93 to cleanup/fix the indices of the parameter list
c  mod 13 Oct 04 - Use F10.9 for the format of the sample interval print
c  mod 7 Mar 05 - Check for ftr/ltr before SETS window.
c  mod 15 Jun 05 - Use REAL*8 when printing/converting.
c  mod 22 Sep 05 - Add istop
c                - Add info
c  mod 25 Jul 06 - CALL RLSEAP only if sets was given.
c  mod 14 Feb 07 - Do negative lat/long correctly on info.
c  mod 23 Feb 07 - Add parameter TRLIST
c  mod 5 Dec 07 - Flush the Fortran print buffer before doing trlist
c  mod 7 Aug 08 - Add info 2 - print the sum of amplitudes in each window
c  mod 10 Aug 09 - Add info 3 - NGDC metadata stuff
c  mod 14 Aug 09 - Add OPATH for info 3
c  mod 15 Aug 09 - Make trlist honor OPATH
c  mod 16 Aug 09 - Cygwin didn't like DFLOAT(real)
c  mod 27 Aug 09 - ptrlist needed a FLOAT trace header argument.
c  mod 24 Jun 10 - Add ASCII
c  mod 20 Jun 11 - Allow FORMAT and INDICES to work better (FORMAT ('Z' and 'I')) when only 1 INDICES
c  mod 26 Jul 12 - Add info 5 - print statistics
c  mod 7 Aug 12 - Don't print trace statistics if istop = -1 (there's no trace).
c  mod 11 Sep 12 - Decimal degrees was read correctly.
c  mod 26 Jul 18 - change print of start & end time so that index=1 is time 0
C
      SAVE
      PARAMETER (MAX=10)                                                ! THE MAXIMUM NUMBER OF ELEMENTS OF THE USER ARRAY SETS
      PARAMETER (MAX_LIST = 10 )
      PARAMETER (NWRDS=24)                                              ! THE LENGTH OF EACH PARAMETER LIST
      DIMENSION BUF(111),LBUF(111),IBUF(111),
     &          SCR(111),LSCR(111),ISCR(111)
      DIMENSION OLD(MAX),CUR(MAX),INDXS(MAX)
      INTEGER indices(20), tr_list(MAX_LIST)
      REAL*8 dscr(100), dlat1, dlat2, dlong1, dlong2, dtemp, scalar
      REAL*8 wbc, ebc, nbc, sbc
      DIMENSION sums(MAX/2)
      COMMON /POUT/ MUNIT,NLISTS
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite, maxsamps,nbperw
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
     *   SECS,LRENUM,ISRCF,IDTYPE
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD
      COMMON /apmem/ a(500000)
      COMMON /diskin1/ spath, path
      CHARACTER*80 spath
      CHARACTER*200 path
      INTEGER FNO,ADDWB,FTR,TRINC,HEADER,INC,THEADS
      LOGICAL FIRST
      CHARACTER*80 CHEADR(40), form, filename
      CHARACTER*2 name
      EQUIVALENCE (ivms,cheadr(1))
      INTEGER*2 IBUF,ISCR
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr, lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr
      DATA FIRST /.TRUE./, llnum/0/, wbc,ebc,nbc,sbc/0.,0.,0.,0./
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      ISIG=0
      IF( istop .LT. 0 ) THEN
          IF( info .NE. 0 ) GOTO 1400
          RETURN
      ENDIF
      NSAMPS = numdat
      DELAY=BUF(46)                                                     ! THE FLOATING POINT DEEP WATER DELAY IN SECONDS
      SI=BUF(49)                                                        ! THE FLOATING POINT SAMPLE INTERVAL IN SECONDS
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE
      CALL PODISC(MUNIT,1,0)                                            ! REWIND THE PARAMETER FILE
      CALL RDDISC(MUNIT,LSCR,NWRDS,ISTAT)                               ! READ THE PARAMETERS
      CALL rddisc( munit, ivms, 20, istat )
c****  VMS doesn't like reading into CHARACTERS
      form = cheadr(1)
      CALL rddisc( munit, indices, 20, istat )
      ISIG=1                                                            ! SET SIGNAL INDICATING THAT PARAM LIST IS IN SCR
      FNO=LSCR(1)
      LNO=LSCR(2)
      noinc = lscr(3)
      ADDWB=LSCR(4)
      LPRINT=LSCR(5)
      FTR=LSCR(6)
      LTR=LSCR(7)
      TRINC=LSCR(8)
      HEADER=LSCR(9)
      INC=LSCR(10)
      THEADS=LSCR(11)
      info = lscr(12)
      ntrlist = lscr(13)
      luno1 = lscr(14)
      IF( ntrlist .GT. 0 ) THEN
          CALL rddisc( munit, tr_list, ntrlist, istat )
c          CALL FLUSH( 6 )
      ENDIF
      DO 20 I=1,MAX
   20 CUR(I)=SCR(nwrds-max+i)
      MLISTS=1
      stime = 0.
      etime = 0.
c
   50 CONTINUE
      LNUM=LBUF(3)                                                      !  IS THE DATA ON TAPE SORTED BY SHOT
      LTRACE=LBUF(4)
      name = 'sh'
      IF( LBUF(7) .NE. 0 ) THEN
          LNUM = LBUF(6)                                                !  OR BY RP
           name = 'rp'
          LTRACE=LBUF(7)
      ENDIF
      IF(LNUM.EQ.LLNUM.AND.MLISTS.NE.1) GO TO 1000                      ! IS IT THE SAME AS THE LAST SHOT (RP)
      LLNUM=LNUM                                                        ! NO, IT'S NOT THE SAME - DO WE NEED NEW PARAMS
   70 IF(LNUM.GE.FNO) GO TO 100                                         ! IS THIS SHOT BEFORE THIS PARAMTER LIST
      IF( mlist .EQ. 1 .AND. info .NE. 0 ) GOTO 1400
      IF(MLISTS.EQ.1) RETURN                                            ! IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) GO TO 10                                        ! IS IT IN OR BEFORE THE LAST LIST
      IF( info .NE. 0 .AND. info .NE. 2 ) GOTO 1400
      RETURN                                                            ! DON'T DO ANYTHING UNLESS EXPLICITLY REQUESTED TO
  100 CONTINUE                                                          !  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                          ! USE THE PARAMETERS OF THIS LIST
      IF( mlists .GE. nlists .AND. info .NE. 0 ) GOTO 1400
      IF( MLISTS .GE. NLISTS ) RETURN                                    ! ANY MORE USER PARAM LISTS ON DISC
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE                                                          ! SET THE PRESENT LIST INTO OLD SO WE CAN GET A NEW ONE IN SCR
      IF(ISIG.EQ.1) GO TO 118
      DO 115 I=1,MAX
  115 OLD(I)=CUR(I)
      GO TO 130
  118 CONTINUE
      DO 120 I=1,MAX                                                    ! SAVE THE CURRENT PARMETER SET
  120 OLD(I)=SCR(nwrds-max+i)
  130 CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      CALL rddisc( munit, form, 20, istat )
      CALL rddisc( munit, indices, 20, istat )
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      noinc = lscr(3)
      ADDWB=LSCR(4)
      LPRINT=LSCR(5)
      FTR=LSCR(6)
      LTR=LSCR(7)
      TRINC=LSCR(8)
      HEADER=LSCR(9)
      INC=LSCR(10)
      THEADS=LSCR(11)
      info = lscr(12)
      ntrlist = lscr(13)
      IF( ntrlist .GT. 0 ) CALL rddisc( munit, tr_list, ntrlist, istat )
      DO 140 I=1,MAX
  140 CUR(I)=SCR(nwrds-max+i)
      MLISTS=MLISTS+1
      GO TO 70
C****
C****     SAVE THE CURRENT LIST IN CUR AND LEVS
C****
  500 CONTINUE
      IF(ISIG.EQ.0) GO TO 1000
      noinc = lscr(3)
      ADDWB=LSCR(4)
      LPRINT=LSCR(5)
      FTR=LSCR(6)
      LTR=LSCR(7)
      TRINC=LSCR(8)
      HEADER=LSCR(9)
      INC=LSCR(10)
      THEADS=LSCR(11)
      info = lscr(12)
      DO 505 I=1,MAX
  505 CUR(I)=SCR(nwrds-max+i)
      IF(LNUM.LT.FNO.AND.MLISTS.GT.1) GO TO 600
      GO TO 1000
C****
C****      SPATIALLY VARY THE PRINT WINDOW TIMES
C****
  600 CONTINUE
      RATIO=(LNUM-LNO)/(FNO-LNO)
      DO 610 I=1,MAX
  610 CUR(I)=RATIO*(SCR(I+8)-OLD(I))+OLD(I)
C****
C****       SETUP THE INDEXES AND THE AP ARRAYS
C****
 1000 CONTINUE
      IF( IAND(lprint,2) .NE. 0) THEN
          PRINT *,fno,lno,noinc,addwb,lprint,ftr,ltr, trinc
          PRINT *,header,inc,info,theads, form, cur
          PRINT *,buf(46), buf(49)
          IF( ntrlist .NE. 0 )
     &        PRINT *,' tr_list:', (tr_list(i),i=1,ntrlist)
      ENDIF
      IF( noinc .NE. 1 .AND. fno .NE. lno ) THEN
          IF( (lnum-fno)/noinc*noinc .NE. (lnum-fno) ) THEN
              IF( info .NE. 0 .AND. info .NE. 2 ) GOTO 1400
              RETURN
          ENDIF
      ENDIF
      LASTNO=LNO
C****
C****   PRINT THE TAPE HEADERS IF WE HAVE TO
C****
      IF(THEADS.EQ.0) GO TO 1090
      PRINT *,' THE EBCDIC TAPE HEADER IS:'
      CALL PODISC(IUNHDR,1,0)
      IF( icompt .NE. 5 ) THEN                                          ! The Cray is different!
          CALL RDDISC( IUNHDR, ivms, 800, ISTAT)
      ELSE
          CALL RDDISC( IUNHDR, cheadr, 400, ISTAT)
      ENDIF
      DO 1050 I=1,40
         CALL EBCASC( cheadr(i), 80, cheadr(i) )                         ! CONVERT THE EBCDIC HEADER TO ASCII
         PRINT *, CHEADR(I) (1:80)
 1050 CONTINUE
      PRINT *,' THE BINARY TAPE HEADER IS:'
      CALL RDDISC(IUNHDR,SCR,50,ISTAT)
      PRINT 1070,(ISCR(I),I=1,200)
 1070 FORMAT(10(1X,I6))
      THEADS=0
C****
C****      PRINT THE TRACE HEADER IF REQUESTED
C****
 1090 IF(HEADER.EQ.0) GO TO 1200
      PRINT 1100,LBUF(3),LBUF(4),LBUF(6),LBUF(7)
 1100 FORMAT(' SHOT ',I10,' TRACE ',I5,' RP ',I7,' TRACE ',I5)
      PRINT *,' HEADER:'
      PRINT 1120,(LBUF(I1),I1=1,7),IBUF(itridptr),LBUF(10),LBUF(16),
     *    LBUF(19),LBUF(20),(IBUF(I2),I2=idelmptr,isiptr),
     *   (IBUF(I3),I3=iyrptr,igmtptr),
     *  (BUF(I4),I4=46,50),LBUF(51),(BUF(I5),I5=52,53)
 1120 FORMAT(' 1L:',I7,' 2L:',I7,' 3L:',I7,' 4L:',I7,' 5L:',I7,
     *   ' 6L:',I7,' 7L:',I7,/,' 15I:',I4,' 10L:',I7,' 16L:',I7,
     *   ' 19L:',I7,' 20L:',I7,' 55I:',I5,' 56I:',I5,/,' 57I:',I5,
     *   ' 58I:',I5,' 59I:',I5,' 79I:',I5,' 80I:',I5,' 81I:',I5,
     *   ' 82I:',I5,/,' 83I:',I5,' 84I:',I5,' 46:',F10.5,' 47:',F10.5,
     *   ' 48:',F10.5,' 49:',F10.9,/,' 50:',F10.5,' 51L:',I5,
     *   ' 52I:',F10.5,' 53:',F10.5)
C****
C****       PRINT THE INDICES SPECIFIED
C****
 1200 CONTINUE
      DO L = FTR, LTR, TRINC
         IF( LTRACE .EQ. L ) GOTO 1201
      ENDDO
      IF( info .NE. 0 .AND. info .NE. 2 ) GOTO 1400
      RETURN
 1201 CONTINUE
      IF( indices(1) .NE. 0 ) THEN
          n = 0
          DO i = 1, 20, 2
             IF( indices(i) .GT. 0 ) THEN
                 n = n + 1
                 IF( indices(i) .EQ. 1 ) dscr(n) = ibuf(indices(i+1))
                 IF( indices(i) .EQ. 2 ) dscr(n) = lbuf(indices(i+1))
                 IF( indices(i) .EQ. 3 ) dscr(n) = buf(indices(i+1))
             ENDIF
          ENDDO
          IF( form(1:1) .EQ. '(' ) THEN
              IF( n .GT. 1 ) THEN
                  WRITE( luno1, form ) (dscr(i),i=1,n)
              ELSEIF( indices(1) .EQ. 1 ) THEN
                  WRITE( luno1, form ) ibuf(indices(2))
              ELSEIF( indices(1) .EQ. 2 ) THEN
                  WRITE( luno1, form ) lbuf(indices(2))
              ELSEIF( indices(1) .EQ. 3 ) THEN
                  WRITE( luno1, form ) buf(indices(2))
              ENDIF
          ELSE
              WRITE (luno1,'(10(1H ,G15.5))') (dscr(i),i=1,n)
          ENDIF
      ENDIF
c****
c****    PRINT THE SETS WINDOWS
c****
      IF( si .EQ. 0. ) GOTO 1310                                        ! watch out for divide by 0
      IF( addwb .EQ. 0 ) THEN                                           ! should we add the water bottom time in?
          wbtime = 0.                                                   ! no
      ELSE
          wbtime = buf(50)                                              !  get the water bottom time
      ENDIF
      ndows = 0                                                         ! count the number of data windows
      DO 1205 i = 1, max
          indxs(i) = 0
          IF( cur(i) .EQ. 0. .AND. i .NE. 1 ) GOTO 1205                 ! a 0 time means no more
          indxs(i) = NINT(( cur(i) + wbtime - delay ) / si ) + 1
          IF( indxs(i) .LT. 1 ) indxs(i) = 1                            ! always start after the beginning
          IF( indxs(i) .GT. nsamps ) indxs(i) = nsamps                  ! but don't go too far
          IF( MOD(i,2) .EQ. 0 ) THEN                                    ! is it the end of a window?
              ndows = ndows + 1
              IF( indxs(i) .LE. indxs(i-1) ) THEN                       ! is the end before the start?
                  indxs(i-1) = 0                                        ! drop the whole window
                  indxs(i) = 0
                  ndows = ndows -1
              ENDIF
          ENDIF
 1205 CONTINUE
      IOUT=0
      IF( ndows .GT. 0 ) CALL RLSEAP(BUF(NUMHDR+1),NSAMPS)
      IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' )
     &    WRITE( filename, '(A2,I6.6,2Htr,I3.3,2H.m )' )
     &           name, lnum, ltrace
      IF( form .EQ. 'ASCII' .OR. form .EQ. 'ascii')
     &    WRITE( filename, '(A2,I6.6,2Htr,I3.3,4H.txt )' )
     &           name, lnum, ltrace
      IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' .OR.
     &    form .EQ. 'ASCII' .OR. form .EQ. 'ascii') THEN
          PRINT *,' Creating file ',filename
          CALL getfil( 2, luno, filename, istat )                       ! get an unused unit number
          OPEN ( UNIT = luno,
     &           FILE = FILENAME,
     &           FORM = 'FORMATTED',
     &           STATUS = 'UNKNOWN' )
          IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' )
     &        WRITE( luno, '(6Ha = [ )' )
      ELSE
          IF( indices(1) .EQ. 0 .AND. ntrlist .EQ. 0 .AND. info .NE. 2 )
     &        PRINT 1100,LBUF(3),LBUF(4),LBUF(6),LBUF(7)
      ENDIF
      DO 1300 I=1,NDOWS
         IF(I.GT.NDOWS) GO TO 1310                                      ! F77 DOES IT MORE THAN ONCE IF NDOWS=1!!!
         j = i * 2 - 1
         IS=INDXS(j)+NUMHDR
         IE=INDXS(j+1)+NUMHDR
         IF( IAND(lprint,2) .NE. 0 ) PRINT *,' indices=',is,ie
         IF(IS.GE.IE) GO TO 1310
         TS=(INDXS(j)-1)*SI+DELAY       !  the time at index 1 is 0
         TE=(INDXS(j))*SI+DELAY
         IF( info .EQ. 0 ) THEN
             PRINT 1230,TS,TE
 1230        FORMAT(' START TIME=',F10.4,' END TIME=',F10.4)
         ELSEIF( info .EQ. 2 ) THEN
             sum = 0.
             DO j = is, ie, inc
                sum = sum + buf(j)
             ENDDO
             sums(i) = sum
         ELSEIF( info .EQ. 4 ) THEN
             sum = 0.
             DO j = is, ie, inc
                sum = sum + buf(j) * buf(j)
             ENDDO
             sums(i) = sum
         ENDIF
         IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' .OR.
     &       form .EQ. 'ASCII' .OR. form .EQ. 'ascii') THEN
                  WRITE( luno, '(1X,G15.5)' ) (buf(j), j = is, ie, inc )
         ELSEIF( form(1:1) .EQ. '(' ) THEN
             WRITE( *, form ) (buf(j), j = is, ie, inc )
         ELSE
             IF( info .EQ. 0 ) PRINT 1240,(BUF(J),J=IS,IE,INC)
         ENDIF
 1240    FORMAT(5(1X,G15.5))
 1300 CONTINUE
      IF( info .EQ. 2 ) PRINT *,' shot ',lbuf(3),' trace ',lbuf(4),
     &    ' window sums: ', (sums(i),i=1,ndows)
      IF( info .EQ. 4 ) PRINT *,' shot ',lbuf(3),' trace ',lbuf(4),
     &    ' window energy: ', (sums(i),i=1,ndows)
C****
C****
 1310 CONTINUE
      IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' .OR.
     &    form .EQ. 'ASCII' .OR. form .EQ. 'ascii') THEN
          IF( form .EQ. 'MATLAB' .OR. form .EQ. 'matlab' )
     &        WRITE( luno, '(1H])' )
          CLOSE( luno, STATUS = 'KEEP' )
          CALL frefil( 1, luno, istat )
      ENDIF
c****
c****    Print the header entries specified with TRLIST
c****
      IF( ntrlist .GT. 0 )
     &    CALL ptrlst( luno1, ntrlist, tr_list, lbuf, ibuf, buf )
c****
c**** STATISTICS & Information
c****
 1400 IF( info .EQ. 1 .OR. info .EQ. 3 ) THEN
          IF( etime .EQ. 0. ) THEN
              iyear1 = ibuf(79)
              jday1 = ibuf(80)
              ihour1 = ibuf(81)
              min1 = ibuf(82)
              isec1 = ibuf(83)
              IF( ibuf(45) .EQ. 3 ) THEN
                  dlat1 = buf(20)
                  dlong1 = buf(19)
              ELSE
                  dlat1 = DFLOAT(lbuf(20))
                  dlong1 = DFLOAT(lbuf(19))
              ENDIF
              nbc = dlat1
              sbc = dlat1
              wbc = dlong1
              ebc = dlong1
              stime = delay
              CALL julcal( month1, iday1, iyear1, jday1 )
          ENDIF
          stime = AMIN1(stime,delay)
          temp = delay + si * FLOAT(nsamps)
          etime = AMAX1(etime,temp)
          IF( ibuf(80) .GT. jday1-5 .AND. ibuf(80) .LT. jday1+4) THEN
              iyear = ibuf(79)
              jday = ibuf(80)
              ihour = ibuf(81)
              min = ibuf(82)
              isec = ibuf(83)
              CALL julcal( month, iday, iyear, jday )
          ENDIF
              IF( ibuf(45) .EQ. 3 ) THEN
                  dlat2 = buf(20)
                  dlong2 = buf(19)
              ELSE
                  dlat2 = DFLOAT(lbuf(20))
                  dlong2 = DFLOAT(lbuf(19))
              ENDIF
          scalar = DFLOAT(ibuf(36))
          IF( dlat2 .LT. sbc ) sbc = dlat2
          IF( dlat2 .GT. nbc ) nbc = dlat2
          IF( dlong2 .GT. ebc ) ebc = dlong2
          IF( dlong2 .LT. wbc ) wbc = dlong2
          IF( istop .NE. 0 .AND. (info.EQ.1 .OR. info.EQ.3 ) ) THEN
              IF( dlong1 .NE. 0 .OR. dlat1 .NE. 0 ) THEN
                  dtemp = dlat1
                  IF( ibuf(45).EQ. 3 ) dtemp = dlat1 * 60. * 60.
                  IF( scalar .GT. 0 ) dtemp = dtemp * scalar
                  IF( scalar .LT. 0 ) dtemp = dtemp / (-1. * scalar)
                  CALL dsecsdms( 1, dtemp, latdeg1, latmin1, seclat1 )
                  dtemp = dlong1
                  IF( ibuf(45).EQ. 3 ) dtemp = dlong1 * 60. * 60.
                  IF( scalar .GT. 0 ) dtemp = dtemp * scalar
                  IF( scalar .LT. 0 ) dtemp = dtemp / (-1. * scalar)
                  CALL dsecsdms( 1, dtemp, longdeg1, longmin1, seclong1)
                  IF( dlong1 .LT. 0 ) longdeg1 = -longdeg1
                  dtemp = dlat2
                  IF( ibuf(45).EQ. 3 ) dtemp = dlat2 * 60. * 60.
                  IF( scalar .GT. 0 ) dtemp = dtemp * scalar
                  IF( scalar .LT. 0 ) dtemp = dtemp / (-1. * scalar)
                  CALL dsecsdms( 1, dtemp, latdeg2, latmin2, seclat2 )
                  IF( dlat2 .LT. 0 ) latdeg2 = -latdeg2
                  dtemp = dlong2
                  IF( ibuf(45).EQ. 3 ) dtemp = dlong2 * 60. * 60.
                  IF( scalar .GT. 0 ) dtemp = dtemp * scalar
                  IF( scalar .LT. 0 ) dtemp = dtemp / (-1. * scalar)
                  CALL dsecsdms( 1, dtemp, longdeg2, longmin2, seclong2)
                  IF( dlong2 .LT. 0 ) longdeg2 = -longdeg2
              ENDIF
              DO i = 100, 1, -1
                 IF( path(i:i) .EQ. ' ' ) n = i
                 IF( ICHAR(path(i:i)) .EQ. 0 ) n = i
                 IF( path(i:i) .EQ. '/' ) GOTO 1499
                 j = i
              ENDDO
 1499         filename = ' '
              filename = path(j:n)
              IF( info .EQ. 1 ) THEN
                  PRINT 1500,filename, jday1, ihour1, min1, isec1,
     &           latdeg1, latmin1, seclat1, longdeg1, longmin1, seclong1
 1500            FORMAT(A25,' Begins: day',I3,1x,I2.2,1H:,I2.2,1H:,I2.2,
     &           ', lat: ', I4,1x,I2,1x,F6.3,' long: ',I4,1x,I2,1x,F6.3)
                 IF( min .EQ. min1 .AND. isec .EQ. isec1 .AND.
     &           ihour .EQ. ihour1 .AND. jday.EQ.jday1 ) isec = isec + 1
                 PRINT 1510,filename,jday, ihour, min, isec,
     &           latdeg2, latmin2, seclat2, longdeg2, longmin2,seclong2,
     &           stime, etime
 1510            FORMAT(A25,'   Ends: day',I3,1x,I2.2,1H:,I2.2,1H:,I2.2,
     &           ', lat: ', I4,1x,I2,1x,F6.3,' long: ',I4,1x,I2,1x,F6.3,
     &           ' data times: ',F5.3,' to ',F6.3,' secs.')
              ENDIF
              IF( info .EQ. 3 ) THEN
                  IF( filename(1:4) .EQ. 'env-' ) THEN
                      WRITE ( luno1, 1600 )
 1600                 FORMAT('Datatype: Knudsen SBP Envelopes')
                  ELSE
                      WRITE ( luno1, 1601 )
 1601                 FORMAT('Datatype: Knudsen SBP Correlates')
                  ENDIF
                  DO i = 2, 200
                     n = i-1
                     IF( ICHAR(filename(i:i)) .EQ. 0 ) GOTO 1605
                  ENDDO
 1605             CONTINUE
                  WRITE( luno1, 1610 ) filename(1:n)
 1610             FORMAT('Line_Number: ',a)
                  WRITE( luno1, 1620 ) iyear1,month1,iday1
 1620             FORMAT('Beginning_Date: ',I4.4,'-',I2.2,'-',I2.2)
                  WRITE( luno1, 1630 ) iyear, month, iday
 1630             FORMAT('Ending_Date: ',I4.4,'-',I2.2,'-',I2.2)
                  WRITE( luno1, 1640 ) ihour1, min1, isec1
 1640             FORMAT('Start_Time: ',I2.2,':',I2.2,':',I2.2)
                  WRITE( luno1, 1650 ) ihour, min, isec
 1650             FORMAT('End_Time: ',I2.2,':',I2.2,':',I2.2)
                  IF( scalar .GT. 0 ) THEN
                      wbc = wbc * scalar
                      ebc = ebc * scalar
                      nbc = nbc * scalar
                      sbc = sbc * scalar
                  ENDIF
                  IF( scalar .LT. 0 ) THEN
                      wbc = wbc / ABS(scalar)
                      ebc = ebc / ABS(scalar)
                      nbc = nbc / ABS(scalar)
                      sbc = sbc / ABS(scalar)
                  ENDIF
                  IF( ibuf(45) .EQ. 2 ) THEN
                      temp = wbc
                      CALL secsdms( 1, temp, ideg, min, sec )
                      dtemp = sec
                      wbc = DABS(DFLOAT(ideg)) + DFLOAT(min)/60. +
     &                      dtemp/3600.D0
c*****   cygwin doesn't like DFLOAT(sec)
c     &                      DFLOAT(sec)/3600.
                      IF( ideg .LT. 0 ) wbc = -wbc
                      temp = ebc
                      CALL secsdms( 1, temp, ideg, min, sec )
                      ebc = DABS(DFLOAT(ideg)) + DFLOAT(min)/60. +
     &                      dtemp/3600.D0
                      IF( ideg .LT. 0 ) ebc = -ebc
                      temp = nbc
                      CALL secsdms( 1, temp, ideg, min, sec )
                      nbc = DABS(DFLOAT(ideg)) + DFLOAT(min)/60. +
     &                      dtemp/3600.D0
                      IF( ideg .LT. 0 ) nbc = -nbc
                      temp = sbc
                      CALL secsdms( 1, temp, ideg, min, sec )
                      sbc = DABS(DFLOAT(ideg)) + DFLOAT(min)/60. +
     &                      dtemp/3600.D0
                      IF( ideg .LT. 0 ) sbc = -sbc
                  ENDIF
                  CALL filsiz( path, nbytes )
                  WRITE( luno1, 1660 ) wbc
 1660             FORMAT('West_Bounding_Coordinate: ',F14.8)
                  WRITE( luno1, 1670 ) ebc
 1670             FORMAT('East_Bounding_Coordinate: ',F14.8)
                  WRITE( luno1, 1680 ) nbc
 1680             FORMAT('North_Bounding_Coordinate: ',F14.8)
                  WRITE( luno1, 1690 ) sbc
 1690             FORMAT('South_Bounding_Coordinate: ',F14.8)
                  WRITE( luno1, 1700 ) FLOAT(nbytes) / 1000000
 1700             FORMAT('Transfer_Size_MB: ',F6.2)
                  WRITE( luno1, 1790 )
 1790             FORMAT('---')
              ENDIF
          ENDIF
      ELSEIF( info .EQ. 5 .AND. istop .GE. 0 ) THEN
          is = 0
          ie = nsamps
          IF( cur(2) .NE. 0 ) THEN
              is = (cur(1) - delay) / si
              ie = (cur(2) - delay) /si
          ENDIF
          n = ie - is
          IF( n .LE. 2 ) THEN
              n = nsamps
              etime = delay + n*si
          ENDIF
          xmin = 0.
          xmax = 0.
          xabsmin = 999999999.
          IF( in .EQ. 0 ) THEN
              DO i = 1, n
                 xmin = AMIN1(xmin, buf(numhdr+is+i))
                 xmax = AMAX1(xmax, buf(numhdr+is+i))
                 temp = ABS(buf(numhdr+is+i))
                 IF( temp .NE. 0. ) xabsmin = AMIN1(xabsmin,temp)
              ENDDO
              IF( xabsmin .EQ. 999999999. ) xabsmin = 0.
              CALL moment( buf(numhdr+is+1), n,
     &             ave, adev, sdev, var, skew, curt )
          ELSE
              DO i = 0, n-1
                 xmin = AMIN1(xmin, a(in+is+i))
                 xmax = AMAX1(xmax, a(in+is+i))
                 temp = ABS(a(in+is+i))
                 IF( temp .NE. 0. ) xabsmin = AMIN1(xabsmin,temp)
              ENDDO
              IF( xabsmin .EQ. 999999999. ) xabsmin = 0.
              CALL moment( a(in+is), n,
     &             ave, adev, sdev, var, skew, curt )
          ENDIF
          PRINT *,' Record',lnum,' trace',ltrace,' from',is*si+delay,
     &        ' to',ie*si+delay
          PRINT *,' min=',xmin,' max=',xmax,' smallest=',xabsmin,
     &        ' ave=',ave,' adev=',adev,' sdev=',sdev,
     &        ' var=',var,' skew=',skew,' curt=',curt
      ENDIF
      RETURN
      END
