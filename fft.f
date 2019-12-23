c**********************************************************************
      SUBROUTINE FFT2(C,MM,INV)
c**********************************************************************
C...............................................................................
C Standard Fast Fourier Transform routine from Cooley et al 1969 (reproduced in
C Aki & Richards Quantitative Seismology p596)
C      the inverse part of this from Bob Parker
C MM  - There are 2**MM points on the trace being transformed. This trace is
C       passed to the routine as complex array C
C...............................................................................
      COMPLEX C(*),U,W,T,CI
      SAVE PI
      DATA PI/3.1415927/
C
      N=2**MM
      NBY2=N/2
      NM1=N-1
      XN = FLOAT(N)
      J=1
C
      DO 7 I=1,NM1
      IF(I.GE.J) GO TO 5
      T=C(J)
      C(J)=C(I)
      C(I)=T
   5  K=NBY2
   6  IF(K.GE.J) GO TO 7
      J=J-K
      K=K/2
      GO TO 6
   7  J=J+K
C
      DO 20 L=1,MM
      LE=2**L
      LE1=LE/2
      U=(1.0,0.0)
      W=CMPLX(COS(PI/FLOAT(LE1)),SIN(PI/FLOAT(LE1)))
      DO 20 J=1,LE1
      DO 10 I=J,N,LE
      IP=I+LE1
      T=C(IP)*U
      C(IP)=C(I)-T
  10  C(I)=C(I)+T
  20  U=U*W
C
      IF(INV.NE.0) THEN
        N1=1 + N/2
        DO 30 I = 2,N1
        CI=C(I)
        C(I)=C(N-I+2)/XN
  30    C(N-I+2) = CI/XN
        C(1)=C(1)/XN
      ENDIF
      RETURN
      END
c*********************************************************************
      subroutine realtr(a,b,n,isn)
c*********************************************************************
c              rearranges array for fft of a real series in place
c              called after a forward fft, before an inverse fft
c              example: forward transform of real series, length n
c              call fft (x,n/2)
c              call realtr(x,x(2),n/2,2)
c                   here fft expects complex array with real and imag
c                   parts alternating. the result appears as cos,sin,
c                   cos,sin,... in x(1),x(2),...,x(n+1),x(n+2)
c                   thus array x must be dimensioned at least n+2 in
c                   order to hold the transform of the Nyquist
c             from Bob Parker
c             now with proper scaling!
c           **CALLS NO OTHER SUBROUTINES**
c
      dimension a(1),b(1)
      real im
      inc = iabs(isn)
      nk = n * inc + 2
      nh = nk/2
      sd = 2. * atan(1.)/float(n)
      cd = 2. * sin(sd)**2
      sd = sin(sd+sd)
      sn = 0.
      IF(ISN.GT.0) go to 10
      cn = -1.
      sd = -sd
      go to 20
   10 cn = 1.
      a(nk-1) = a(1)
      b(nk-1) = b(1)
   20 do 30 j=1,nh,inc
      k = nk - j
      aa = a(j) + a(k)
      ab = a(j) - a(k)
      ba = b(j) + b(k)
      bb = b(j) - b(k)
      re = cn*ba + sn*ab
      im = sn*ba - cn*ab
      b(k) = (im - bb)/2.
      b(j) = (im + bb)/2.
      a(k) = (aa - re)/2.
      a(j) = (aa + re)/2.
      aa = cn - (cd*cn + sd*sn)
      sn = (sd*cn - cd*sn) + sn
   30 cn = aa
      return
      end
