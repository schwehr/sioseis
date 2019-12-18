      SUBROUTINE CROSS(LX,X,LY,Y,LG,G)
C
C     CROSS COMPUTES THE CROSS PRODUCT BETWEEN TWO VECTORS.
C     REFERENCE: EAR MULTICHANNEL VOLUME, P. 27.
C
C     INPUTS ARE
C         LX=  LENGTH OF X SERIES
C         X=   X SERIES
C         LY=  LENGTH OF Y SERIES
C         Y= Y SERIES
C         LG=  NUMBER OF LAGS,FROM 0 TO LG-1,DESIRED IN CROSS PRODUCT
C     OUTPUT IS
C         G=   CROSS PRODUCT GIVEN BY (WHERE THE SUM IS OVER ALL I)
C
C
C              G(K) = SUM X(I+K)*Y(I)
C
C              WHERE VALUES OF X AND Y OUTSIDE OF THEIR RANGES ARE
C              TAKEN TO BE ZERO.
C
      DIMENSION X(*),Y(*),G(*)

      if(lg .gt. 0) then

         DO 1 I=1,LG
c    1       call dotpr(x(i),1,y,1,g(i),min0(ly+i-1,lx)-i+1)
   1       CALL DOT(MIN0(LY+I-1,LX)-I+1,X(I),Y,G(I))

      else if(lg .lt. 0) then

            lg2=-lg
            lg1=lg2+1
            do 2 i=1,lg1
c               call dotpr(x(i),1,y,1,g(i+lg2-1),min0(ly+i-1,lx)-i+1)
              call dot(min0(ly+i-1,lx)-i+1,x(i),y,g(i+lg2-1))
    2       continue
            do 3 i=1,lg2
               ii=lg2-i+1
c               call dotpr(y(i),1,x,1,g(ii),min0(lx+i-1,ly)-i+1)
              call dot(min0(lx+i-1,ly)-i+1,y(i),x,g(ii))
    3       continue

      endif

      RETURN
      END

