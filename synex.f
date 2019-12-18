      SUBROUTINE SYNEX(BUF,LBUF,IBUF,SCR,LSCR,ISTOP)
C     SSYNEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS SYN
C  (SYNTHETIC INPUT).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /SYN/) AND THE TRACE WITH TRACE HEADER
C  WILL BE LEFT IN MEMORY ARRAY BUF.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE PROCESSED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  ISTOP  - A SIGNAL TO THE CONTROLLER INDICATING THAT THERE IS NO MORE
C           INPUT TO BE GENERATED, AND SYNEX SHOULD NOT BE CALLED AGAIN.
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JULY 1980
C
c
c  mod July 95 - Some multiple lists didn't work.
c  mod 5 Aug 95 - Write "IRIS" header if nsamps > 32767
c  mod 25 Oct 95 - fno <> lno caused fno+1 - lno not to work
c  mod 9 Apr 96 - ranges weren't in headers when values were given.
c  mod 31 Mar 99 - Change the way to count NTVA that allows 0 amplitude
c  mod 8 Jan 03 - Make delay into a 32 bit integer in the segy header.
c  mod 1 May 03 - Account for new param exthdr in the param list.
c  mod 1 May 06 - Add parameter NOINC
c  mod 15 Jun 07 - Add TTVA
c  mod 8 Aug 08 - VALUES are saved in tva, so do not podisc to reread them
c  mod 26 Sep 08 - Do the trace header number of samples as c unsigned short
C
      PARAMETER (MAX=90)                                                ! THE MAXIMUM NUMBER OF ELEMENTS OF THE USER ARRAY SETS
      PARAMETER (NPARS=106)                                             ! THE LENGTH OF EACH PARAMETER LIST
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      DIMENSION TVA(MAX)
      INTEGER*2 ishort(2)
      EQUIVALENCE (ishort(1),llong)
      COMMON /SYN/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD
      COMMON /READT/ ILUN,NUMHDR,NUMDAT,IUNHDR,IREELN,NTRCS
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr, lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr
      INTEGER FNO,NTRGAT
      REAL noise
      LOGICAL FIRST
      DATA FIRST /.TRUE./, itrno/1/, n/0/
      SAVE
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      ISTOP=0                                                           ! SET THE STOP FLAG TO DON'T STOP 
      IF( FIRST ) THEN
          FIRST=.FALSE.
          CALL PODISC(MUNIT,1,0)                                        ! REWIND THE PARAMETER FILE
          CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)                            ! READ THE FIRST PARAMETER LIST
          MLISTS=1
          fno = LSCR(1)
          NTRCS=LSCR(4)
          NTRGAT=LSCR(9)
          noise = scr(11)
          IF(NTRGAT.NE.0) IBUF(15)=2                                    !  SAY THAT THE DATA IS SORTED BY RP
          ISI=SCR(5)*1000000.+.5
          NSAMPS=SCR(6)/SCR(5)+1.+.5
      ELSE
          IF( fno .LE. lno ) THEN
c****          the values are now held in the tva array and are not reread
c              IF( n .NE. 0 ) CALL podisc( munit, 2, -npars )            ! IS IT THE SAME AS THE LAST SHOT (RP)
              GOTO 120
          ENDIF
C****
C****     GET ANOTHER USER PARAMETER LIST FROM DISC
C****
          IF( mlists .EQ. nlists ) GOTO 120
          CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)
          MLISTS=MLISTS+1
      ENDIF
      FNO=LSCR(1)
      LNO=LSCR(2)
      LPRINT=LSCR(3)
      NTRCS=LSCR(4)
      SI=SCR(5)
      SECS=SCR(6)
      NSAMPS=SECS/SI+1.+.5
      X=SCR(7)
      XINC=SCR(8)
      NTRGAT=LSCR(9)
      DELAY=SCR(10)
      noise = scr(11)
      N=LSCR(12)                                                        ! THE NUMBER OF TRACE VALUES GIVEN BY THE USER
      nexthdr = lscr(13)
      noinc = lscr(14)
      ntvas = lscr(15)
      nttvas = lscr(16)
      DO I=1,MAX
         TVA(I)=SCR(I+16)
      ENDDO
      RANGE=X+XINC*(ITRNO-1)
c****
c****   create the trace amplitudes
c****
  120 CONTINUE
      RANGE=X+XINC*(ITRNO-1)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,fno,lno,lprint,ntrcs,si,secs,nsamps,x,xinc,ntrgat,
     *      delay,noise,n, noinc, ntvas, nttvas
          PRINT *,' TVA=',(tva(i),i=1,8)
      ENDIF
c****
c****    Do VALUES here
c****
      IF( N .NE. 0 ) THEN                                               ! DID THE USER GIVE ANY VALUES?
c          CALL RDDISC(MUNIT,BUF(NUMHDR+1),N,ISTAT)                      ! GET THE VALUES WHICH ARE AFTER THE PARAMETER LIST
          DO I = 1, nsamps
             BUF(NUMHDR+I)=0.
          ENDDO
          DO i = 1, n
             buf(numhdr+i) = tva(i)
          ENDDO
          GOTO 500
      ENDIF
c****
c****  DO TVA (time-velocity-amplitude) HERE
c****
      IF( ntvas+nttvas .NE. 0 ) THEN
          IF( nsamps .GT. maxsamps ) THEN
            PRINT *,' ***  WARNING  ***  The output number of samples ',
     &        nsamps,' exceeds the SIOSEIS allocation of ', maxsamps
              nsamps = maxsamps
          ENDIF
      ENDIF
      IF( ntvas .NE. 0 ) THEN
          CALL SSEIS(1,SI,RANGE,TVA,NTVAS/3,BUF(NUMHDR+1),NSAMPS,DELAY)
      ELSEIF( nttvas .NE. 0 ) THEN
          CALL SSEIS(2,SI,RANGE,TVA,NTTVAS/4,BUF(NUMHDR+1),NSAMPS,DELAY)
      ENDIF
c**** 
c****   Create the trace headers here
c****
  500 CONTINUE
      DO 510 I = 1, numhdr
  510 LBUF(I) = 0
      LBUF(3) = fno
      LBUF(4)=ITRNO
      IF( ntrgat .NE. 0 ) THEN
          lbuf(6) = fno
          lbuf(7) = itrno
      ENDIF
      IBUF(itridptr) = 1                                                ! TRACE ID CODE (=1 MEANS A LIVE TRACE)
      LBUF(10)=RANGE
      llong = NINT(delay*1000.)
      ibuf(idelmptr) = llong
c****  The UTIG huge delay gets messy on Intel because ibuf(55) isn't on
c****  a 32 bit word boundary
c      IBUF(idelmptr) = ishort(2)
c      IF( ishort(1) .NE. 0 ) ibuf(idelmptr-1) = ishort(1)
      buf(ldelsptr) = delay
      numdat = nsamps
c      ibuf(isampptr) = nsamps
c      IF( nsamps .GT. 32767 ) THEN
c          ibuf(isampptr) = 32767
c          lbuf(isampptr) = nsamps
c          lbuf(29) = nsamps
c      ENDIF
      CALL long2ushort( nsamps, ibuf(isampptr) )
      IBUF(isiptr)=ISI
      BUF(46)=DELAY
      BUF(49)=SI
      IF( NTRGAT.EQ.ITRNO.AND.NTRGAT.NE.0) LBUF(51)=-1
c****
c****   Add in the noise if there is any
c****
      IF( noise .NE. 0. ) THEN
          idum = fno * 10000 + itrno
          DO i = 1, nsamps
             buf(numhdr+i) = buf(numhdr+i) + gasdev( idum ) * noise
          ENDDO
      ENDIF
c****
c****    finish up
c****
      IN=0                                                              ! MAKE SURE TO TELL EVERYONE THAT THE DATA IS NOT IN THE AP
      ITRNO=ITRNO+1                                                     ! INCREMENT THE TRACES DONE COUNTER
      IF( ITRNO .GT. NTRCS ) THEN                                       ! ARE THERE MORE TRACES IN THIS SHOT
          ITRNO=1                                                       ! NO, START A NEW SHOT
          fno = fno + noinc
          IF( fno.GT.LNO.AND.MLISTS.EQ.NLISTS) ISTOP=1                  ! ANY MORE TO DO AT ALL
      ENDIF
      RETURN
      END
