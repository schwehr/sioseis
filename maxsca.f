      SUBROUTINE MAXSCA(TRACE,DEFF,NSAMPS,ITYPE,PEAK,TROUGH)
C**** THIS IS FOR LONG ARGUMENTS AND SHORT INTEGER APS
C****
C    A SUBROUTINE TO SCALE AN ARRAY SO THAT SOME SAMPLE IS +/- DEFF*RNIBS BIG.
C THIS IS A SCALING ROUTINE FOR VERSATEC SEISMIC TRACE PLOTS.
C ARGUMENTS:
C        TRACE  - THE ARRAY CONTAINING THE DATA.  THE SCALED DATA IS PLACED IN
C                 TRACE ALSO, THEREBY CLOBBERING THE ARRAY.
C        DEF    - THE VALUE OF THE LARGEST VALUE AFTER SCALING.  THE LARGEST
C                 VALUE WILL BE SCALED TO DEF.  ( ALSO THE DEFLECTION OF A TRACE
C        NSAMPS - NUMBER OF SAMPLES (16 BIT INTEGER)
C        ITYPE  - TYPE OF SCALE
C               =1, DELECTION IS MEASURED FROM THE PEAK TO THE ZERO LINE
C               =2, DEFLECTION IS MEASURED AS THE DISTANCE BETWEEN PEAK AND
C                    TROUGH.
C     PAUL HENKART, SCRIPPS, DECEMBER 1978
C     MODIFIED BY PCH IN NOVEMBER 1979 TO USE THE ARRAY PROCESSOR
C     MODIFIED 7 SEPTEMBER 1980 TO PRINT THE MULTIPLIER AND USE THE
C     USER'S MULTIPLIER RATHER THAN ONE PICKED BY THE AP.
C****
      DIMENSION TRACE(1)
      COMMON /VERSAT/NIBS,RNIBS
      COMMON /SIOAP/ IASGND,IRELSE,IN,JOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *     IDECIM
      COMMON /MAXSCS/ NPRMUL,RMULT
      SAVE
      DATA IFMT/2/, IONE/1/, N/0/
C****
C****
      CALL INAP(TRACE,NSAMPS)
      IF(RMULT1.NE.0.) RMULT=RMULT1
      PEAK=0.
      TROUGH=0.
      DEF=DEFF*RNIBS
      IADDR1=LAPSIZ-4
      IADDR2=IADDR1+2                                                   ! /* THE AP ADDRESS OF DEF
      IADDR3=IADDR2+1
      CALL APWR
      IF(RMULT.LE.0.) GO TO 100
      CALL APPUT(RMULT,IADDR3,IONE,IFMT)                                ! /* PUT THE USER'S MULTIPLIER IN THE AP
      CALL APWD
      GO TO 400
  100 CALL APPUT(DEF,IADDR2,IONE,IFMT)                                  ! /* PUT DEF IN THE AP
      IF(ITYPE.EQ.2) GO TO 200
      CALL MAXV(IN,IONE,IADDR1,NSAMPS)                                  ! /* FIND THE MAXIMUM VALUE OF THE TRACE
      GO TO 300
  200 CALL MAXMGV(IN,IONE,IADDR1,NSAMPS)                                ! /*   FIND THE MAXIMUM MAGNITUDE VALUE
  300 CALL APWD
      CALL VDIV(IADDR1,IONE,IADDR2,IONE,IADDR3,IONE,IONE)               ! /* DIVIDE DEF BY THE MAX
  400 CALL VSMUL(IN,IONE,IADDR3,IN,IONE,NSAMPS)                         ! /* VECTOR SCALAR MULTIPLY
      N=N+1
      IF(RMULT.LT.0.) GO TO 450
      IF(NPRMUL.EQ.0) GO TO 600
      IF(NPRMUL.LT.N) GO TO 600
  450 CALL APGET(TEMP,IADDR3,IONE,IFMT)                                 ! /* GET THE MULTIPLIER FROM THE AP
      CALL APWD
      PRINT 500,TEMP
  500 FORMAT(' THE PLOT MULTIPLIER IS ',G10.4)
      IF(RMULT.LT.0.) RMULT1=TEMP
  600 CALL RLSEAP(TRACE,NSAMPS)
      RETURN
      END
