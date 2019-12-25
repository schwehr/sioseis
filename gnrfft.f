      subroutine gnrfft(ar,nlen,direct,norm)
c
c-------------------------------------------------------------------------------
c This is the routine that is called by SIOSEIS to perform the fft of a real time
c series. This is a glue routine that defines a common call interface between
c SIOSEIS and FFT routine that have been optimized for different machines.
c
c This version is the general version and uses the simple fft pair & scram
c to perform transform of a real time series.
c
c     integer nlen      The length of the input real time series. A power of two
      real    ar(nlen)
c                       The array containing the time series/DFT trace. In
c                       frequency domain the (real) Nyquist frequency component
c                       is expect to be packed at ar(2) after the zero frequency
c                       component at ar(1). All other frequency components are
c                       as expected i.e. real at ar(2i), imag at ar(2i+1)
      logical direct
c                       Direction of FFT.
c                       = TRUE  forward  FFT time -> frequency.
c                       = FALSE inverse FFT frequency -> time.
c                         sign convention is exp(+iwt) for forward transform
c                                            exp(-iwt) for inverse transform
      logical    norm
c                      = TRUE  normalize the output FFT by 1/nlen
c                      = FALSE no normalization.
c-------------------------------------------------------------------------------
c
c
      integer pow2
c
      np = pow2(nlen) - 1
      if(direct) then                                                   ! forward FFT
        call fftfwd(ar,np)                                              ! A +ve exponent
c        call fftinv(ar,np)
        call scram(ar,nlen/2,+1,.FALSE.)
      else
        call scram(ar,nlen/2,-1,.TRUE.)
        call fftinv(ar,np)
c        call fftfwd(ar,np)
      endif
      if (norm) then
        rlen = float(nlen/2)
        do 10 i = 1, nlen
  10    ar(i) = ar(i)/rlen
      endif
      return
      end
C
C
      subroutine scram(a,n,isgn,inv)
c---------------------------------------------------------------------------
c    This routine enables the DFT of a real time series of length 2n to
c be recoverd from the DFT of the sames series when it is treated as a complex
c time series of length n.
c
c    On the forward transform the elements real series of 2n are packed
c sequentially into the real & imaginary parts of the complex array. i.e.
c the even elements (starting at 0) are packed into the real parts & the
c odd elements into the imaginary parts. The FFT of this complex series
c is not the same as the FFT of the real series; the output must be
c processed further to recover the correct positive frequency values. This
c is the job of scram.
c
c   On inverse transform the positive frequency components must be repacked
c prior to being passed to a complex inverse FFT if the resulting output is
c be a real time series.
c
c   For the forward transform, this routine returns the Nyquist frequency
c component packed into the imaginary part of the zero frequency component.
c It expects the Nyquist frequency component packed here for the inverse
c transform.
c
      complex a(0:n-1)
c                  The array to be transformed
c     Integer n    The length of the complex arrray
      integer isgn
c                   The sign of the current transform
      logical inv
c                 = TRUE  this is an inverse transform. The frequency
c                         components are being packed prior to DFT
c                 = FALSE this is a forward transform. The positive
c                         frequency components are being upacked after
c                         a DFT.
c Modifications:
c     10/20/88 ( Added conjg to atem a(n-i)= )
c
c Last Modified
c     14/3/89 Changed to single precision for CRAY compatibility
c--------------------------------------------------------------------------
c
      parameter (pi = 3.1415926535)
      integer jsgn
      real    rexp, iexp
      real    rphas, iphas, rtem
      real    rtems, items
      complex atem, btem
      complex yi
c
      nby2 = n/2
      jsgn = sign(1,isgn)
      rtem = pi / float(n)
      rexp =  cos(rtem)
      iexp =  sin(jsgn*rtem)
      rphas = 1.0
      iphas = 0.0
      if (inv) then
        yi = cmplx(0.,-0.5)
      else
c      yi   = jsgn * cmplx(0., 0.5)
        yi   = cmplx(0., 0.5)
      endif
c
      do 10 i = 1, nby2
      rtem   = rexp * rphas - iexp * iphas
      iphas  = rexp * iphas + iexp * rphas
      atem   = 0.5 * (a(i) + conjg(a(n-i)))
      btem   = yi * cmplx(rtem, iphas) * ( a(i) - conjg( a(n-i) ) )
      a(i)   = atem - btem
      a(n-i) = atem + conjg(btem)
      a(n-i) = conjg(atem) + conjg(btem)
      rphas  = rtem
   10 continue
c
c.. Assume that the Nyquist frequency is packed/wanted in the imaginary
c    part of the zero frequency component
      rtems = real(a(0))
      items = aimag(a(0))
      a(0)  = cmplx(rtems + items, rtems - items)
      if (inv) a(0) = 0.5 * a(0)
      return
      end
