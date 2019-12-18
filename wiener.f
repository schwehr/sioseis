C*****  WIENER  Wiener-Levinson Algorithm         MATH ADVANTAGE REL 3.0
C
C    ** COPYRIGHT 1984-1985 QUANTITATIVE TECHNOLOGY CORPORATION **
C
C  CALL FORMAT
C
C       CALL WIENER (LR,R,G,F,A,IFLG,IERR)
C
C       where,
C
C       LR      Integer input filter length.
C
C       R       Real input vector,
C               auto-correlation coefficients.
C
C       G       Real input vector, right-hand side
C               coefficients.  Not used if IFLG=0.
C
C       F       Real output vector, filter coefficients if IFLG=1;
C                                   stability indicators if IFLG=0.
C
C       A       Real output vector, prediction error operator.
C
C       IFLG    Integer input processing option flag:
C                   =0 for spike deconvolution.
C                   =1 for general deconvolution.
C
C       IERR    Integer output completion code:
C                  =0 if the routine terminated normally.
C                  >0 if a failure occurred. The value of IERR
C                     is the pass number where failure occurred.
C
C
C  DESCRIPTION
C
C       This routine solves a system of single-channel normal
C       equations which arise in least-squares filtering and
C       prediction problems for single-channel time series.
C       It uses a recursive method of solving simultaneous
C       equations.
C
C       It solves:
C
C            1.  The following set of equations for F:
C                      SUM[F(s)*R(1+ABS(t-s))] = G(t)
C                      for s=1,LR  and  t=1,LR
C
C            2.  The following set of equations for A:
C                      SUM[A(s)*R(1+ABS(t-s))] = V*D
C                      for s=1,LR  and  t=1,LR
C
C                where,
C
C                      A(1) = 1.0,
C
C                      D = 1.0 when t=1,
C
C                      (D = 0.0 otherwise),
C
C                      V = A(1)*R(1)+....+A(LR)*R(LR),
C
C       All arrays are LR long.
C
C
C  REFERENCE
C
C       E. A. Robinson.  Multichannel Time Series Analysis with
C       Digital Computer Programming.
C
C       N. Levinson.  1947.  The Wiener RMS (root-mean-square)
C       Error Criterion in Filter Design and Prediction.
C       J. Math. Phys., Vol. 25,  pp. 261-278.
C
C       A. Jurkevics & R. Wiggins.  Dec 1984.  A Critique of
C       Siesmic Deconvolution Methods.  Geophysics, Vol. 49, No. 12.
C       pp. 2109-2116.
C
C
C  EXAMPLE
C
C       For General Deconvolution:
C
C       CALL WIENER (2,R,G,F,A,1,IERR)
C
C       Input Operands:
C
C       R =  10.000
C             4.000
C
C       G =   2.000
C             0.000
C
C
C       Output Operands:
C
C       F =  0.238
C           -0.095
C
C       A =  1.000
C           -0.400
C
C       IERR = 0
C
C
C       For Spike Deconvolution:
C
C       CALL WIENER (2,R,G,F,A,0,IERR)
C
C       Input Operands:
C
C       R =  10.000
C             4.000
C
C       G =   2.000
C             0.000
C
C
C       Output Operands:
C
C       F =  0.200
C           10.000
C
C       A =  1.000
C           -0.400
C
C       IERR = 0
C
C  HISTORY
C         1) May 85     D. Cooper       Original.
C
      SUBROUTINE WIENER(LR,R,G,F,A,IFLG,IERR)
C
      INTEGER LR,IFLG,IERR,ID,L2,IB,IC,LH
      REAL R(*),G(*),F(*),A(*),V,D,Q,AL,AJOLD,FL
      IF (LR.LE.0 .OR. IFLG.LT.0 .OR. IFLG.GT.1) GOTO 9000
      IERR = 0
      V = R(1)
      D = R(2)
      A(1) = 1.0
      F(1) = G(1) / V
      Q = F(1) * R(2)
      IF (LR.EQ.1) GOTO 9000
C
      DO 999 ID = 2, LR
        AL   = - D / V
        A(ID) = AL
        IF (V.GT.0.0) GOTO 100
          IERR = ID
          GOTO 9000
100     CONTINUE
        IF (IFLG.EQ.0) F(ID) = V
        V = V + AL * D
        IF (ID.EQ.2) GOTO 410
          L2 = ID/2
          IF (L2 .LT. 2) GOTO 350
            DO 299 IB=2, L2
C .......     Do pairs of elements of A:
              IC = ID - IB + 1
              AJOLD = A(IB)
              A(IB) = AJOLD + AL * A(IC)
              A(IC) = A(IC)  + AL * AJOLD
299         CONTINUE
350       CONTINUE
          IF (L2+L2.EQ.ID) GOTO 400
C .......   IF ID is Odd, cover the unpaired element:
            LH = L2 + 1
            A(LH) = A(LH) + AL * A(LH)
400       CONTINUE
410     CONTINUE
        IF (IFLG.EQ.0) GOTO 750
C ....... IF General Deconvolution:
          FL   = (G(ID) - Q) / V
          F(ID) = FL
          DO 699 IB = 1, ID-1
            IC = ID - IB + 1
            F(IB) = F(IB) + FL * A(IC)
699       CONTINUE
750     CONTINUE
        IF (ID.EQ.LR) GOTO 9000
C ....... Compute D and Q for next loop iteration.
C ....... Q is used for General Deconvolution, D for both.
        D = 0.0
        Q = 0.0
        DO 899 IB = 1, ID
          IC = ID - IB + 2
          D = D + A(IB)*R(IC)
          Q = Q + F(IB) * R(IC)
899     CONTINUE
999   CONTINUE
C
9000  RETURN
      END
