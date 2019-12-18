C*****  NZCROS Find Specified Zero Crossing          MTHADV EXT. REL 1.0
C
C    ** COPYRIGHT 1986 QUANTITATIVE TECHNOLOGY CORPORATION **
C
C  CALL FORMAT
C
C       CALL NZCROS (A,IA,IN,IL,NF,N)
C
C       where,
C
C       A       Real input vector.
C
C       IA      Integer input stride for vector A.
C
C       IN      Integer input, zero crossing to find.
C
C       IL      Integer output, displacement of INth zero crossing.
C
C       NF      Integer output, number of zero crossings found.
C
C       N       Integer input element count.
C
C
C  DESCRIPTION
C
C       This routine scans the input vector A, with stride
C       and direction specified by IA, searching for transitions
C       in the signs of sequential elements.  (Zero is considered
C       to be a positive number.)  The scan terminates when
C       the INth zero crossing is found or N elements have
C       been scanned.  If the specified zero crossing is not
C       found, IL is set to zero.  NF is always set, even if
C       the specified zero crossing is not found.
C
C
C  EXAMPLE
C
C       CALL NZCROS (A,1,3,IL,NF,5)
C
C       Input Operands:
C
C       A =  0.000
C           -1.000
C            0.000
C            1.000
C            2.000
C
C
C       Output Operands:
C
C       IL = 0
C
C       NF = 2
C
C
C  HISTORY
C         1) Jul 86     D. Benua        Original.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NZCROS(A,IA,IN,IL,NF,N)
C
      INTEGER IA,IN,IL,NF,N,M,II,ICUR,IPRE
      REAL A(1)
C
C-----------------------------------------------------------------------
C
      IF (N.LE.0) GO TO 14
      IF (IN .LE. 0) GOTO 14
      NF = 0
      II = 1
      DO 12 M=1,N
         IF (A(II) .GE. 0.0) GOTO 6
            ICUR = -1
         GOTO 8
6           ICUR = 1
8        IF (M .EQ. 1) IPRE = ICUR
         IF (ICUR .NE. IPRE) NF = NF + 1
         IF (NF .NE. IN) GOTO 10
            IL = II - 1
            GOTO 14
10       IPRE = ICUR
         II = II + IA
12    CONTINUE
      IL = 0
14    RETURN
      END
