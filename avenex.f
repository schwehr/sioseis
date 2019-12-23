      SUBROUTINE AVENEX(BUF,LBUF,IBUF,SCR,LSCR)
C     AVENEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS AVENOR
C  (AVERAGE ABSOLUTE AMPLITUDE NORMALIZE).  THE USER'S PARAMETER'S MUST BE IN
C  DISC FILE MUNIT (IN COMMON /AVEN/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORARY ARRAY BUF.  NORMALIZE WINDOW TIMES FOR TRACES BETWEEN
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE MUTED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MAY 1980
c  mod July 1997 - add VEL
c  mod 10 Oct 02 - Add hold, which means use the multiplier from
c                  the "hold'th" trace.  This adds argument last
c                  to subroutine avenor.f
c  mod 15 Jul 08 - Use numdat rather than ibuf(58) for nsamps because of
c                  very large (> 32768) ibuf(58)
c  mod 7 Jan 09 - Drop the window if the beginning is the same as the end
c                 or the beginning = nsamps
C
C
      PARAMETER (MAX=8)                                                  /* THE MAXIMUM NUMBER OF ELEMENTS OF THE USER ARRAY SETS
      PARAMETER (MAX2=MAX+2)                                            /* TOO BAD FTN DOESN'T LIKE DIMENSION LEVS(MAX+2)
      PARAMETER (NWRDS=18)                                              /* THE LENGTH OF EACH PARAMETER LIST
      DIMENSION BUF(1111),LBUF(1111),IBUF(1111),SCR(1111),LSCR(1111)
      INTEGER*2 IBUF
      DIMENSION OLD(MAX),CUR(MAX),LEVS(MAX2),INDXS(MAX2)
      COMMON /AVEN/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(32766)
      COMMON /READT/ ILUN,NUMHDR, NUMDAT
      INTEGER FNO, hold
      REAL LEVS
      LOGICAL FIRST
      SAVE
      DATA FIRST /.TRUE./, llnum/0/, wbtime/0./, ndone/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          /* IS IT A DEAD TRACE
      ISIG=0
      ndone = ndone + 1
      last = 0
      IF( hold .GT. 0 .AND. ndone .GT. hold ) last = hold
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)                                            /* REWIND THE PARAMETER FILE
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)                                /* READ THE FIRST PARAMETER LIST
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      MLISTS=1
   50 CONTINUE
      LNUM=LBUF(3)                                                      /*  IS THE DATA ON TAPE SORTED BY SHOT
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     /*  OR BY RP
      IF(LNUM.EQ.LLNUM) GO TO 1000
      LLNUM=LNUM
   70 IF(LNUM.GE.FNO) GO TO 100                                         /* IS THIS SHOT BEFORE THIS PARAMTER LIST
      IF(MLISTS.EQ.1) GO TO 500                                         /* IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) GO TO 10                                       /* IS IT IN OR BEFORE THE LAST LIST
      GO TO 500                                                         /* IT MUST BE BETWEEN THE 2 LISTS
  100 CONTINUE                                                          /*  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                         /* USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.LT.NLISTS) GO TO 110                                    /* ANY MORE USER PARAM LISTS ON DISC
      IF(ISIG.EQ.0) GO TO 1000
      GO TO 500
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE                                                          /* SET THE PRESENT LIST INTO OLD SO WE CAN GET A NEW ONE IN SCR
      IF(ISIG.EQ.1) GO TO 118
      DO 115 I=1,MAX
  115 OLD(I)=CUR(I)
      GO TO 130
  118 CONTINUE
      DO 120 I=1,MAX                                                    /* SAVE THE CURRENT PARMETER SET
  120 OLD(I)=SCR(I+4)
  130 CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      LASTNO=LNO
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      GO TO 70
C****
C****     SAVE THE CURRENT LIST IN CUR AND LEVS
C****
  500 CONTINUE
      IF(ISIG.EQ.0) GO TO 1000
      IADDWB=LSCR(3)
      LPRINT=LSCR(4)
      vel = scr(5)
      hold = lscr(6)
      IF(LNUM.LT.FNO) GO TO 600                                         /* DON'T BOTHER IF SPATIALLY VARYING
      DO 510 I=1,8
  510 CUR(I)=SCR(I+6)
      DO 520 I=1,4
  520 LEVS(I)=SCR(I+14)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,fno, lno, iaddwb, lprint, vel, hold
          PRINT *,(cur(i),i=1,8)
          PRINT *,(levs(i),i=1,8)
      ENDIF
      GO TO 1000
C****
C****      SPATIALLY VARY THE NORMALIZE WINDOW TIMES
C****
  600 CONTINUE
      RATIO=(LNUM-LASTNO)/(FNO-LASTNO)
      DO 610 I=1,MAX
  610 CUR(I)=RATIO*(SCR(I+4)-OLD(I))+OLD(I)
      DO 620 I=1,4
  620 LEVS(I)=SCR(I+12)
C****
C****       APPLY THE NORMALIZE
C****
 1000 CONTINUE
c      NSAMPS=IBUF(58)                                                   /* THE NUMBER OF DATA SAMPLES IN THE TRACE
      nsamps = numdat
      DELAY=BUF(46)                                                     /* THE FLOATING POINT DEEP WATER DELAY IN SECONDS
      SI=BUF(49)                                                        /* THE FLOATING POINT SAMPLE INTERVAL IN SECONDS
      wbtime = buf(50)
      range = FLOAT(lbuf(10))
      IOUT=1                                                            /* LEAVE THE DATA IN THE AP
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   /* PUT THE DATA IN THE AP
      DO 1005 I=1,MAX
         INDXS(I)=0
 1005 CONTINUE
      NDOWS = 0                                                         /* THE NUMBER OF NORMALIZE WINDOWS
      DO 1010 I = 1, max, 2
         time1 = cur(i)
         time2 = cur(i+1)
         IF( time1+time2 .EQ. 0. ) GOTO 1020
         ndows = ndows + 1
         IF( iaddwb .NE. 0 ) THEN
             time1 = time1 + wbtime
             time2 = time2 + wbtime
         ENDIF
         IF( vel .NE. 0. ) THEN
             time1 = SQRT(time1*time1+range*range/(vel*vel))
             time2 = SQRT(time2*time2+range*range/(vel*vel))
         ENDIF
         INDXS(I) = NINT((time1 - DELAY) / SI + 1.)                     ! CONVERT TIME TO INDEXES
         INDXS(I+1) = NINT((time2 - DELAY) / SI + 1.)
         IF( indxs(i) .GT. nsamps ) indxs(i) = nsamps
         IF( indxs(i+1) .GT. nsamps ) indxs(i+1) = nsamps
         IF( indxs(i) .LT. 1 ) indxs(i) = 1
         IF( indxs(i+1) .LT. 1 ) indxs(i+1) = 1
         IF( indxs(i+1) .EQ. 1 ) indxs(i+1) = nsamps
         IF( indxs(i) .EQ. nsamps .OR. indxs(i) .EQ. indxs(i+1) )
     &       ndows = ndows - 1
 1010 CONTINUE
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,(indxs(i),i=1, ndows*2 )
 1020 CONTINUE
      IF(NDOWS.LT.1) THEN
         PRINT *,' ***  ERROR  ***  ALL AVENOR WINDOWS BEFORE THE DATA.'
         RETURN
      ENDIF
      IF( ndows .GT. 1 ) THEN
          DO 1029 j = 1, ndows-1
             IF( indxs(j+j-1) .GE. indxs(j+j+1) ) THEN
                 DO 1028 I = 1, 6                                       ! MOVE THE WINDOWS UP
 1028            INDXS(I)=INDXS(I+2)
                 GOTO 1020
             ENDIF
 1029     CONTINUE                                                      ! ALL THE WINDOWS ARE GOOD!!
      ENDIF
      IF(IUSEAP.EQ.0) GO TO 2000
C****
C****   DO IT IN THE AP
      IF(NDOWS.EQ.1) GO TO 1031
C****
      DO 1030 I=2,NDOWS                                                 ! PUT THE DISTANCE BETWEEN WINDOW CENTERS IN THE AP AFTER THE LEVELS
      J=(I)*2
      LEVS(I+3)=(INDXS(J)-INDXS(J-1))/2+INDXS(J-1)-((INDXS(J-2)-
     *   INDXS(J-3))/2.+INDXS(J-3))
 1030 CONTINUE
 1031 CALL APWR                                                         ! WAIT FOR THE AP SO WE DON'T CLOBBER THE DATA IN NEXTAD
      CALL APPUT(LEVS,NEXTAD,MAX,2)                                     ! PUT LEVS IN THE AP
      DO 1040 I=1,NDOWS
      J=I*2
      INDXS(J)=INDXS(J)-INDXS(J-1)+1
 1040 CONTINUE
      IADDR=NEXTAD+MAX                                                  ! AP ADDRESS OF A SCRATCH ARRAY
      IF(IAND(LPRINT,2).EQ.0) GO TO 1060
      PRINT 1050,NEXTAD,IADDR,IN,NSAMPS,(INDXS(I),I=1,MAX)
 1050 FORMAT(' AVENOR ARGS:',/,2(10(1X,I10),/))
      PRINT 1051,(LEVS(J),J=1,8)
 1051 FORMAT(' IN THE AP:',8(1X,F10.4))
 1060 CONTINUE
      CALL APWD                                                         ! WAIT FOR LEVS TO GET THERE
      CALL AVEVFC(NEXTAD,IADDR,IN,NSAMPS,INDXS(1),INDXS(2),
     *  INDXS(3),INDXS(4),INDXS(5),INDXS(6),INDXS(7),INDXS(8))
      RETURN
C****
C****    DO IT IN HOST MEMORY
C****
 2000 CONTINUE
      ITEMP=NDOWS*2+2                                                   ! ZERO THE LAST INDEX ENTRIES SO AVENOR KNOWS TO STOP
      INDXS(ITEMP-1)=0
      INDXS(ITEMP)=0
      IF(IAND(LPRINT,2).NE.0) PRINT 2010,(INDXS(I),I=1,10),
     *    (LEVS(J),J=1,4)
 2010 FORMAT(' AVENOR ARGS:',/,10(1X,I6),/,4(1X,F12.5))
      CALL AVENOR(A(IN),A(NEXTAD),INDXS,LEVS,NSAMPS, lprint, last )
      J=NEXTAD-1
      K=IN-1
      DO 2020 I=1,NSAMPS                                                ! PUT THE OUTPUT OVER THE INPUT
 2020 A(K+I)=A(J+I)
      RETURN
      END
