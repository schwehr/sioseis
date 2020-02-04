      SUBROUTINE umultex(BUF,LBUF,IBUF,SCR,LSCR)
C     UMULTEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS UMULT
C  (USER GIVEN TRACE MULTIPLY).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /UMULT/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORY.   NO SPATIAL VARIATIONS ARE ALLOWED.  ONLY ONE TIME WINDOW
C  MAY BE USED.
C
C  ARGUMENTS:
C  BUF    - THE CURRENT TRACE, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, May 1996
c
c  mod 22 Dec 97 - Change MIN0 to MIN
C
      PARAMETER (MAXPTS=15000)                                          ! /*  THE MAXIMUM NUMBER OF POINTS
      PARAMETER (NPARS=5)                                               ! /* THE NUMBER OF WORDS IN THE PARAMETER LIST
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      COMMON /UMULT/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN, NUMHDR, numdat
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      INTEGER FNO
      DIMENSION PTS(MAXPTS)
      LOGICAL FIRST
      SAVE
      DATA FIRST /.TRUE./
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          ! /* IS IT A DEAD TRACE
      ISIG=0
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          ! /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)                                            ! /* REWIND THE PARAMETER FILE
      CALL RDDISC(MUNIT,scr,NPARS,ISTAT)
      ISIG=1                                                            ! /* SET SIGNAL INDICATING THAT PARAM LIST IS IN SCR
      FNO=LSCR(1)
      LNO=LSCR(2)
      NPTS=LSCR(3)
      stime = SCR(4)
      LPRINT=LSCR(5)
      NWRDS=NPTS
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)                                ! /* READ THE POINTS IN
      DO 40 I=1,NPTS
   40 PTS(I)=SCR(I)
      MLISTS=1
   50 CONTINUE
      LNUM=LBUF(3)                                                      ! /*  IS THE DATA ON TAPE SORTED BY SHOT
   60 IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     ! /*  OR BY RP
      IF(LNUM.EQ.LLNUM.AND.MLISTS.NE.1) GO TO 1000                      ! /* IS IT THE SAME AS THE LAST SHOT (RP)
      LLNUM=LNUM                                                        ! /* NO, IT'S NOT THE SAME - DO WE NEED NEW PARAMS
   70 IF(LNUM.GE.FNO) GO TO 100                                         ! /* IS THIS SHOT BEFORE THIS PARAMETER LIST
      IF(MLISTS.EQ.1) RETURN                                            ! /* If BEFORE THE FIRST LIST, don't do it!
      IF(LNUM.LE.LNO) GO TO 10                                          ! /* IS IT IN OR BEFORE THE LAST LIST
      RETURN                                                            ! /* IT MUST BE BETWEEN THE 2 LISTS
  100 CONTINUE                                                          ! /*  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO)  GO TO 1000                                       ! /* USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.LT.NLISTS) GO TO 110                                    ! /* ANY MORE USER PARAM LISTS ON DISC
      RETURN
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE                                                          ! /* SET THE PRESENT LIST INTO OLD SO WE CAN GET A NEW ONE IN SCR
  130 CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      NPTS=LSCR(3)
      stime = SCR(4)
      LPRINT=LSCR(5)
      NWRDS=NPTS
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      DO 150 I=1,NPTS
  150 PTS(I)=SCR(I)
      MLISTS=MLISTS+1
      GO TO 70
C****
C****     SPATIAL VARIATION IS NOT ALLOWED
C****
c  500 CONTINUE
C****
C****       Do the trace multiplication!
C****
 1000 CONTINUE
c      nsamps = ibuf(isampptr)
      nsamps = numdat
      delay = buf(ldelsptr)
      si = buf(lsisptr)
      istart = 1
      IF( stime .GE. 0. ) istart = NINT((stime - delay) / si) + 1
      IF( istart .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  STIME prior to delay will fail.'
          STOP
      ENDIF
      IF( iuseap .NE. 0 .AND. in .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  UMULT can not use the AP.'
          STOP
      ENDIF
c      n = MIN0(nsamps,npts)
      n = MIN(nsamps,npts)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' stime=',stime,' npts=',npts,' delay=',delay,
     &            ' istart=',istart,' n=',n
      ENDIF
      IF( in .EQ. 0 ) THEN                                              ! is it in the ap simulator?
          DO i = 1, n
             buf(numhdr+istart+i-1) =  buf(numhdr+istart+i-1) * pts(i)
          ENDDO
      ELSE
          DO i = 1, n
             a(istart+i-1) = a(istart+i-1) * pts(i)
          ENDDO
      ENDIF
      RETURN
      END
