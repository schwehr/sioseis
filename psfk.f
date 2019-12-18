      subroutine psfk(vskso,cpw,cqw,nw,cscl,rcscl,tadj,td)
C-------------------------------------------------------------------------------
C
C This is the corresponding Phase shifting routine for FKSHFT. It performs the
C appropriate phase shifting to lower the apparent recording level to tlev.
C
C
C Inputs:
C    vsks0 - The value (V*K/DW)**2
C
C    cpw    - The input trace at a given wavenumber k. The trace is arranged
C             with frequencies running as
C                             -Nyq -> 0 -> +Nyq - delta w
C             The trace has been normalized wrt frequency. Thus locally delta w 
C             is 1 & Nyq = nw/2
C
C    nw     - The number of frequencies to migrate.
C
C    cscl   - A scratch array used by HALE to hold nw+1 complexes
C             After interpolation each element of the output trace (cqw) is
C             multiplied by a factor which includes directivity, phase shift &
C             an interpolation constant.
C    rcscl  - A real array equivalent to cscl.
C
C             The index of iw & dw corresponds to the output frequencies from
C             -Nyq to Nyq (nw+1 values). iw & dw give the integer & fractional
C             part of the corresponding input frequency. The values are used
C             in the sinc interpolation. dw is made to satisfy
C                  DELTA <= dw >= 1. - DELTA
C             dw is bounded away from 0 & 1 to ensure numerical stability
C
C
C Outputs:
C   cqw    - The migrated output frequency data (complex).
C            *** CPW  must not be the same as CQW.  ***
C            **** The input array must not be the same as the output array
C
C Call route:
C     contro:fkshft:psfk
C
C Externals:
C     calls no other routines
C
C Last Modified:
C     11/10/88  ajh.
C
C-------------------------------------------------------------------------------
C
      parameter (PI      = 3.141592654)
      parameter (OPI     = 1./PI)
      parameter (ZERO    = 0.0)
C
      complex cpw(nw+2),cqw(nw+1)
      complex cscl(nw+1)
      real    rcscl(nw+nw+2)          ! The real equivalent to cscl
      complex czero
      integer rindxl, rindxh
c
      czero  = cmplx(0.0,0.0)
C
C..                  Calculate scaling factors & frequency indices for +ve freqs
      nwo2   = nw / 2              ! The nyquist frequency
      indx   = nw - 1              ! The index to the array rcscl
      do 100 iwv = 0, nwo2
        indx = indx + 2
        qsq   = iwv * iwv - vskso
        if (qsq.lt.0.) then
          rcscl(indx)   = ZERO
          rcscl(indx+1) = ZERO
        else
          phase  =   sqrt(qsq) * td + float(iwv) *  tadj
          rcscl(indx)   =  cos(phase)          ! Real part of scaling
          rcscl(indx+1) =  sin(phase)          ! & the imaginary part
        endif
  100 continue
C
C..                       Generate the -ve frequency elements from the +ve ones
      rindxl = nw + 1
      rindxh = rindxl
      do 110 iwv = 1, nwo2
        rindxl          = rindxl - 2
        rindxh          = rindxh + 2
        rcscl(rindxl)   =   rcscl(rindxh)           ! real part of scaling
        rcscl(rindxl+1) =  -rcscl(rindxh+1)         ! & imaginary part
  110 continue
c
C..
      do 600 i = 1, nw
        cqw(i) = cpw(i) * cscl(i)
  600 continue
c
      cqw(nwo2+1) = czero                     ! Remove D.C. frequency component 
      RETURN
      end
