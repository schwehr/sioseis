      SUBROUTINE UFILEX(BUF,LBUF,IBUF,SCR,LSCR)
C     UFILEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS UFILTR
C  (USER GIVEN FILTER).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /UFILTR/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORY.   NO SPATIAL VARIATIONS ARE ALLOWED.  ONLY ONE TIME WINDOW
C  MAY BE USED.
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE FILTERED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, NOVEMBER 1982
C
c  mod 1 Apr 10 - remove arithmetic IF statements
c  mod 25 Mar 15 - fno/lno wasn't quite right
c
      PARAMETER (MAXPTS=15000)                                          /*  THE MAXIMUM NUMBER OF FILTER POINTS
      PARAMETER (NPARS=5)                                                /* THE NUMBER OF WORDS IN THE PARAMETER LIST
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      COMMON /UFILTR/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER FNO
      DIMENSION FILPTS(MAXPTS)
      LOGICAL FIRST
      SAVE
      DATA FIRST /.TRUE./
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          /* IS IT A DEAD TRACE
      ISIG=0
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)                                            /* REWIND THE PARAMETER FILE
      CALL RDDISC(MUNIT,scr,NPARS,ISTAT)
      ISIG=1                                                            /* SET SIGNAL INDICATING THAT PARAM LIST IS IN SCR
      FNO=LSCR(1)
      LNO=LSCR(2)
      NFPTS=LSCR(3)
      NSHIFT=LSCR(4)
      LPRINT=LSCR(5)
      NWRDS=NFPTS
      CALL RDDISC(MUNIT,LSCR,NWRDS,ISTAT)                                /* READ THE FILTER POINTS IN
      DO 40 I=1,NFPTS
   40 FILPTS(I)=SCR(I)
      MLISTS=1
   50 CONTINUE
      LNUM=LBUF(3)                                                      /*  IS THE DATA ON TAPE SORTED BY SHOT
   60 IF(LBUF(7).NE.0) LNUM=LBUF(6)                                      /*  OR BY RP
      IF(LNUM.EQ.LLNUM.AND.MLISTS.NE.1) GO TO 1000                      /* IS IT THE SAME AS THE LAST SHOT (RP)
      LLNUM=LNUM                                                         /* NO, IT'S NOT THE SAME - DO WE NEED NEW PARAMS
c   70 IF(LNUM.GE.FNO) GO TO 100                                           /* IS THIS SHOT BEFORE THIS PARAMTER LIST
c      IF(MLISTS.EQ.1) GO TO 1000                                        /* IS IT BEFORE THE FIRST LIST
c      IF(LNUM.LE.LNO) GO TO 10                                          /* IS IT IN OR BEFORE THE LAST LIST
c      GO TO 500                                                          /* IT MUST BE BETWEEN THE 2 LISTS
c  100 CONTINUE                                                          /*  THE CURRENT SHOT (RP) IS >= LNO
c      IF(LNUM.LE.LNO) GO TO 1000                                        /* USE THE PARAMETERS OF THIS LIST
c      IF(MLISTS.LT.NLISTS) GO TO 110                                    /* ANY MORE USER PARAM LISTS ON DISC
c      IF(ISIG.EQ.0) GO TO 1000                                          /* IS THERE A LIST IN MEMORY
c      GO TO 1000                                                         /* YES THE LAST LIST IS IN SCR
c
   70 CONTINUE   ! Thanks Alistair
      if (lnum < fno) return     ! not reached the first shot of the list

      if (lnum .ge. fno .and. lnum .le. lno) go to 1000
      if (mlists.ge.nlists) return
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE                                                          /* SET THE PRESENT LIST INTO OLD SO WE CAN GET A NEW ONE IN SCR
  130 CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      NFPTS=LSCR(3)
      NSHIFT=LSCR(4)
      LPRINT=LSCR(5)
      NWRDS=NFPTS
      CALL RDDISC(MUNIT,LSCR,NWRDS,ISTAT)
      DO 150 I=1,NFPTS
  150 FILPTS(I)=SCR(I)
      MLISTS=MLISTS+1
      GO TO 70
C****
C****     SPATIAL VARIATION IS NOT ALLOWED
C****
  500 CONTINUE
      IF(ISIG.EQ.0) GO TO 1000
      PRINT 510
  510 FORMAT(' ***  ERROR  ***   UFILTR DOES NOT ALLOW SPATIAL',
     *   ' VARIATIONS.')
      STOP
C****
C****       SETUP THE INDEXES AND THE AP ARRAYS
C****
 1000 CONTINUE
      NSAMPS=IBUF(58)                                                   /* THE NUMBER OF DATA SAMPLES IN THE TRACE
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                    /* PUT THE DATA IN THE AP
      IF( NEXTAD+NSAMPS+NFPTS*3 .GT. LAPSIZ)  THEN
          PRINT *,' ***  ERROR  ***   NOT ENOUGH AP TO PERFORM FILTER.'
          PRINT *,' nextad=',nextad,' nsamps=',nsamps,' nfpts=',nfpts,
     *        ' lapsiz=',lapsiz
          STOP
      ENDIF
      INFPTS=NFPTS                                                      /* WE NEED AN INTEGER*2 NFPTS FOR AP ARGUMENTS!
      IF(IUSEAP.EQ.0) GO TO 1200
C****
C****  DO IT IN THE AP
C****
      CALL VCLR(NEXTAD,1,INFPTS)                                           /*  ZERO OUT AN AREA BEFORE THE DATA
      ITEMP=NEXTAD+NFPTS-1
      CALL VMOV(IN,1,ITEMP,1,NSAMPS)                                       /* MOVE THE INPUT TO AFTER THE ZEROS
      ITEMP=ITEMP+NSAMPS
      CALL VCLR(ITEMP,1,INFPTS)                                            /* ZERO OUT THE END
      IFILAD=ITEMP+NFPTS+1
      CALL APPUT(FILPTS,IFILAD,INFPTS,2)                                   /* PUT THE FILTER IN THE AP AT IFILAD
      IFILAD=IFILAD+NFPTS-1                                              /* POINT TO THE LAST FILTER POINT
      N=NSAMPS+NFPTS+NFPTS                                              /* HOW LONG SHOULD THE CONVOLUTION OUTPUT BE?
      CALL CONV(NEXTAD,1,IFILAD,-1,NEXTAD,1,N,INFPTS)
c      IF(NSHIFT) 1100,1110,1120
      IF( nshift .EQ. 0 ) GOTO 1110
      IF( nshift .GT. 0 ) GOTO 1120
 1100 ITO=IN                                                              /* NSHIFT<0
      IFROM=NEXTAD-NSHIFT
      N=NSAMPS+NSHIFT
      GO TO 1130
 1110 ITO=IN                                                             /* NSHIFT=0
      IFROM=NEXTAD
      N=NSAMPS
      GO TO 1130
 1120 ITO=IN+NSHIFT                                                      /* NSHIFT>0
      IFROM=NEXTAD
      CALL VCLR(IN,1,NSHIFT)                                                /* ZERO OUT THE HOLE CREATED BY THE POSITIVE SHIFT
      N=NSAMPS-NSHIFT
 1130 CONTINUE
      CALL VMOV(IFROM,1,ITO,1,N)                                            /* MOVE IT!
      IF(NSHIFT.GE.0) RETURN
      ITO=ITO+N
      N=-NSHIFT
      CALL VCLR(ITO,1,N)
      RETURN
C****
C****    DO IT IN HOST MEMORY
C****
 1200 CONTINUE
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT*,' in=',in,' nsamps=',nsamps,' nfpts=',infpts,
     *      ' nextad=',nextad
      ENDIF
      CALL CONVO(-1,A(IN),NSAMPS,FILPTS,INFPTS,A(NEXTAD),nsamps)
      ITOH=IN-1                                                          /* THE INDEX OF T ZERO
      J=NEXTAD-1                                                         /* MOVE THE DATA AS INDICATED BY THE SHIFT
      N=NSAMPS
      IF(NSHIFT)1240,1220,1201
 1201 DO 1210 II=1,NSHIFT                                               /* SHIFT THE DATA TO THE RIGHT
 1210 A(ITOH+II)=0.                                                      /*  ZERO FILL THE BEGINNING OF THE TRACE
      N=N-NSHIFT                                                        /* WE DON'T NEED TO MOVE THE WHOLE TRACE!
      ITOH=ITOH+NSHIFT
 1220 DO 1230 II=1,N                                                    /* MOVE THE TRACE TO THE RIGHT (POSITIVE TIME)
 1230 A(ITOH+II)=A(J+II)
      GO TO 1290
 1240 J=NEXTAD-1-NSHIFT
      N=NSAMPS+NSHIFT
      DO 1250 II=1,N
 1250 A(ITOH+II)=A(J+II)                                                 /* MOVE THE DATA TO THE LEFT (NEGATIVE IN TIME)
      ITOH=ITOH+N
      N=-NSHIFT                                                         /* ZERO FILL THE BACK END
      DO 1260 II=1,N
 1260 A(ITOH+II)=0.
 1290 RETURN
      END
