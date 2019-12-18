      SUBROUTINE PLOTVS(TIME,TINC,NTIMES,VTUPLE)
C     PLOTVS PLOTS THE VELOCITY SPECTRA COMMON ARRAY VELDAT ON THE LINE PRINTER.
c  mod 17 Oct 95 - The grid was off by 1
c
      PARAMETER (NLINES=56)                                              /* NUMBER OF USABLE LINES OF PRINT PER PAGE
      COMMON /VELDAT/IA(900)
      CHARACTER*132 IA
      SAVE
      DIMENSION JBUF(60)
      LOGICAL FIRST
      DIMENSION VTUPLE(111)
      DATA FIRST/.TRUE./
C
      IF(.NOT.FIRST) GO TO 70
      FIRST=.FALSE.
      COLS=VTUPLE(3)-1.
      NCOLS=VTUPLE(3)
      VINC=(VTUPLE(2)-VTUPLE(1))/COLS*10
      V=VTUPLE(1)
      N=4
      I=1
   60 JBUF(I)=V
      N=N+10
      I=I+1
      V=V+VINC
      IF(V.LE.VTUPLE(2)) GO TO 60
      N2=I-1
   70 CONTINUE
   80 FORMAT(8X,I4,10(6X,I4))
      NPRTED=13                                                          /* INVPLT PRINTS 13 LINES
      T=TIME
      DO 100 I=1,NTIMES
      PRINT 90,T, IA(I)(1:NCOLS)
   90 FORMAT(1X,F7.4,2X,A)
      T=T+TINC
      NPRTED=NPRTED+1
      IF(NPRTED.NE.NLINES) GO TO 100
c      PRINT 95
      WRITE( ia(1), 95 )
   95 FORMAT(21('+    '))
      PRINT 96, ia(1)(1:ncols)
   96 FORMAT(10X,A)
      PRINT 80,(JBUF(J),J=1,N2)                                          /*  PRINT THE VELOCITIES
      PRINT 80,(JBUF(J),J=1,N2)                                          /* PRINT THE VELOCITIES
      PRINT 96, ia(1)(1:ncols)
      NPRTED=2
  100 CONTINUE
      RETURN
      END
