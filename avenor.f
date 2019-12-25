      SUBROUTINE AVENOR(BUF,OBUF,INDXS,OLEVS,NSAMPS, lprint, last )
C     AVENOR PERFORMS AVERAGE AMPLITUDE NORMALIZATION IN MEMORY.  THE AP VERSION
C  IS AVEVFC.  AVERAGE AMPLITUDE NORMALIZE ADJUSTS THE AVERAGE ABSOLUTE AMPLITUDE
C  WITHIN EACH WINDOW TO A SPECIFIED LEVEL.  IT DOES THIS BY DIVIDING THE
C  LEVEL BY THE AVERAGE ABSOLUTE VALUE OF THE WINDOW IN ORDER TO FORM A
C  MULTIPLIER THAT WILL BE APPLIED TO THE CENTER POINT OF THE WINDOW.
C  DATA UP TO THE FIRST WINDOW MIDPOINT RECEIVE THE FIRST MULTIPLIER.
C  MULTIPLIERS BETWEEN WINDOW MIDPOINTS ARE LINEARLY INTERPOLATED.
C  DATA AFTER THE MIDPOINT OF THE LAST WINDOW RECEIVE THE MULTIPLIER OF THE
C  LAST WINDOW.  (A ONE WINDOW AVENOR RESULTS IS ALL DATA GETTING THE SAME
C  MULTIPLIER).
C      ANY NUMBER OF WINDOWS MAY BE GIVEN, BUT THE LAST WINDOW MUST BE ZERO TIME.
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SEISMIC REFLECTION PROCESSORS, SAN DIEGO, CA.
C  16 JANUARY 1983.  ALL RIGHTS RESERVED.
c
c  mod 11 Oct 02 - Add argument last, which when set non-zero means
c                  to use the multipliers from the last trace rather
c                  than calculate new ones.  The windows/indices may
c                  be different.
c  mod 19 Oct 02 - Print the values on the trace being held (last).
C
C  ARGUMENTS:
C  BUF    - THE INPUT ARRAY
C  OBUF   - THE OUTPUT ARRAY.  THE OUTPUT MAY NOT BE THE SAME AS THE INPUT,
C           EXCEPT WHEN ONLY ONE WINDOW IS GIVEN.
C  INDXS  - AN ARRAY OF INDEXES DEFINING THE WINDOWS.  THE INDEXES MUST BE
C           GIVEN IN PAIRS, START INDEX FOLLOWED BY THE END INDEX.  AS NOTED
C           ABOVE, THE LAST INDEXES MUST BE 0.  THEREFORE, THERE MUST BE
C           2*(THE NUMBER OF WINDOWS)+2 INDEXES.
C  OLEVS  - AN ARRAY OF OUTPUT WINDOW LEVELS.  THERE MUST BE A LEVEL FOR EACH
C           WINDOW.
C  NSAMPS - THE TOTAL NUMBER OF SAMPLES IN THE TRACE
C
C
      DIMENSION INDXS(1),OLEVS(1),BUF(1),OBUF(1)
      DIMENSION rmult_last(4)
      SAVE rmult_last, iprint
      DATA iprint/1/
C
      ICOUNT=-1
      IC=1
   10 ICOUNT=ICOUNT+2                                                    /* SET UP FOR THE NEXT WINDOW
      ISTART=INDXS(ICOUNT)                                              /* THE START INDEX OF THE WINDOW
      IEND=INDXS(ICOUNT+1)                                              /* THE END INDEX OF THE WINDOW
      IF(IEND.LE.ISTART) GO TO 300
      MIDPT=ISTART+(IEND-ISTART)/2                                      /* FIND THE MID POINT OF THE WINDOW
      OLEV=OLEVS(IC)                                                    /* GET THE OUTPUT LEVEL FOR THIS WINDOW
      IC=IC+1
      IF( last .NE. 0 ) THEN
          rmult = rmult_last(ic-1)
          GOTO 110
      ENDIF
      REALN=FLOAT(IEND-ISTART)                                          /* THE WINDOW LENGTH
      AVE=0.
      DO 100 I=ISTART,IEND                                              /* FIND THE AVERAGE ABSOLUTE VALUE
  100 AVE=AVE+ABS(BUF(I))                                               /*  WATCH OUT FOR OVERFLOW !!!!!!!!!
      AVE=AVE/REALN                                                      /* THESE 2 LINES COULD BE REPLACE BY THE SINGLE LINE
C  100 AVE=AVE+ABS(BUF(I))/REALN                                        /* BUT I DISLIKE DIVIDES ($$$$)
      RMULT=1.
      IF(AVE.NE.0.) RMULT=OLEV/AVE                                      /* WATCH OUT FOR DIVIDE BY ZERO!
      rmult_last(ic-1) = rmult
  110 CONTINUE
      IF( IAND(lprint,4) .NE. 0 ) PRINT *,' Average absolute ave  = ',
     &     ave,' multiplier=',rmult
      IF( iprint .NE. 0 .AND. last .NE. 0 )
     &    PRINT *,ic-1,' multiplier =',rmult
      IF(ICOUNT.NE.1) GO TO 200
      DO 120 I=1,MIDPT                                                  /* DO THE FIRST WINDOW
  120 OBUF(I)=BUF(I)*RMULT                                              /* MULTIPLY EVERYTHING UP TO AND INCLUDING THE MIDPT
      RMULT1=RMULT                                                      /* SAVE THE MULTIPLIER
      MIDPT1=MIDPT                                                      /* SAVE THE MIDPOINT
      GO TO 10                                                          /* GO GET THE NEXT WINDOW
C****
C****   DO THE WINDOW IF IT'S NOT THE FIRST OR LAST WINDOW
C****
  200 TEMP=(RMULT-RMULT1)/(MIDPT-MIDPT1+1)                               /* GET THE INTERPOLATOR
      X=0.
      DO 220 I=MIDPT1,MIDPT                                             /* REMEMBER THAT MIDPT1 WAS THE MIDPOINT OF THE LAST WINDOW
      X=X+1.
  220 OBUF(I)=BUF(I)*(RMULT1+X*TEMP)
      RMULT1=RMULT                                                       /* SAVE THE MULTIPLIER AND MIDPOINT FOR THE NEXT WINDOW
      MIDPT1=MIDPT
      GO TO 10                                                          /* GET ANOTHER WINDOW
C****
C****    THIS IS THE LAST WINDOW
C****
  300 DO 320 I=MIDPT1,NSAMPS                                            /* DO IT TO THE END OF THE TRACE
  320 OBUF(I)=BUF(I)*RMULT                                              /* USE THE MULTIPLIER OF THE LAST WINDOW TO THE END
      IF( iprint .NE. 0 .AND. last .NE. 0 ) iprint = 0
      RETURN
      END
