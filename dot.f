      SUBROUTINE DOT(L,X,Y,P)
      DIMENSION X(1),Y(1)                                               DOT40010
C
C     DOT IS THE DOT PRODUCT OF TWO VECTORS
C     REFERENCE: EAR MULTICHANNEL VOLUME, P. 20.
C                                                                       DOT70040
C     INPUTS ARE                                                        DOT70070
C        L=LENGTH OF THE VECTORS X AND Y                                DOT70080
C        X=THE VECTOR X, X(1),X(2),...,X(L)                             DOT70090
C        Y=THE VECTOR Y Y(1),Y(2),...,Y(L)                              DOT70100
C     OUTPUTS ARE                                                       DOT70110
C        P=DOT PRODUCT OF X AND Y
C
C                L                                                      DOT70160
C COMPUTES    P= SUM(X(I)*Y(I))   IF  L  GRTHN 0                        DOT70170
C                I=1                                                    DOT70180
C                                                                       DOT70190
C             P= 0                IF  L  EQLSTHN 0                      DOT70200
C                                                                       DOT70210
      P=0.0                                                             DOT70220
      IF (L)3,3,1                                                       DOT70230
    1 DO 2 I=1,L                                                        DOT70240
    2 P=P+X(I)*Y(I)                                                     DOT70250
    3 RETURN                                                            DOT70260
      END                                                               DOT70270
