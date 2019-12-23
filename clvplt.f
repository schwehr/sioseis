      SUBROUTINE CLVPLT(ISCR,VTUPLE)
C     CLVPLT CLOSES THE VELOCITY SPECTRA PRINTOUT.
c  mod 17 Oct 95 - The grid was off by 1
      DIMENSION VTUPLE(111)
      INTEGER*4 ISCR(111)
      COMMON /VELDAT/IA(900)
      CHARACTER*132 IA
      CHARACTER*115 LINE
C
      COLS=VTUPLE(3)-1.
      NCOLS=VTUPLE(3)
      N=NCOLS+10
      WRITE(LINE,1)
    1 FORMAT(10X,21('+    '))
      PRINT 10,LINE(1:NCOLS+10)                                         ! PRINT THE GRID LINE
   10 FORMAT(A)
      VINC=(VTUPLE(2)-VTUPLE(1))/COLS*10
      V=VTUPLE(1)
      N=4
      I=1
   60 ISCR(I)=V
      N=N+10
      I=I+1
      V=V+VINC
      IF(V.LE.VTUPLE(2)) GO TO 60
      I=I-1
      PRINT 70, (ISCR(J),J=1,I)
   70 FORMAT(8X,I4,10(5X,I5))
      RETURN
      END
