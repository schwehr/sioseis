        SUBROUTINE apsID
C******************************************************************************
C This routine prints a string identifying the current APS simulator being
C used. This version is the CRAY vectorized version and uses /TRANSP/ as a
C scratch array.
C******************************************************************************
       print *,'VAPCRY : CRAY vectorized simulator version 1.0'
       return
       end

        SUBROUTINE apsVADD(a,i,b,j,c,k,n)
C******************************************************************************
C Vector Addition simulator
C******************************************************************************
        IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVADD ',a,i,b,j,c,k,n
      DO 100 ii = 0, n-1
  100   apscr(ii) = apdata(a+ii*i) + apdata(b+ii*j)
      DO 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

        SUBROUTINE apsVSADD(a,i,b,c,k,n)
C******************************************************************************
C Vector Scalar Addition simulator.
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

      real temp
c        print *,' apsVSADD ',a,i,b,j,c,k,n
      temp = apdata(b)
      do 100 ii = 0, n-1
  100   apscr(ii) = apdata(a+ii*i) + temp
      DO 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

      SUBROUTINE apsVSUB(a,i,b,j,c,k,n)
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVSUB ',a,i,b,j,c,k,n
      DO 100 ii = 0, n-1
  100   apscr(ii) = apdata(b+ii*j) - apdata(a+ii*i)
      DO 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

      SUBROUTINE apsVMUL(a,i,b,j,c,k,n)
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVMUL ',a,i,b,j,c,k,n
      DO 100 ii = 0, n-1
  100   apscr(ii) = apdata(a+ii*i) * apdata(b+ii*j)
      DO 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

      SUBROUTINE apsVSMUL(a,i,b,c,k,n)
C******************************************************************************
C Vector-Scalar Multiply simulator.
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

      real temp
c
c     print *,' apsVMUL ',a,i,b,c,k,n
      temp = apdata(b)
      do 100 ii = 0, n-1
  100   apscr(ii) = apdata(a+ii*i) * temp
      do 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

        SUBROUTINE apsVNEG(a,i,c,k,n)
C******************************************************************************
C Vector negation simulator.
C******************************************************************************
        IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVNEG ',a,i,c,k,n
      do 100 ii = 0, n-1
  100   apscr(ii) = -apdata(a+ii*i)
      do 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

      SUBROUTINE apsVMOV(a,i,c,k,n)
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVMOV ',a,i,c,k,n
      if (i.eq.1.and.k.eq.1) then
CDIR$ IVDEP
        do 50 ii = 0,n-1
          apdata(c+ii) = apdata(a+ii)
   50   continue
      else
        iinc = 0
        kinc = 0
        DO 100 ii = 0, n-1
          apdata(c+kinc) = apdata(a+iinc)
          iinc = iinc + i
          kinc = kinc + k
  100   CONTINUE
      endif
      return
      end

      SUBROUTINE apsVFLT(a,i,c,k,n)
C******************************************************************************
      IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVFLT ',a,i,c,k,n
      DO 100 ii = 0, n-1
  100   apscr(ii) = FLOAT(iapdata(a+ii*i))
      do 200 ii = 0, n-1
  200   apdata(c+ii*k) = apscr(ii)
      return
      end

        SUBROUTINE apsVFIX(a,i,c,k,n)
C******************************************************************************
        IMPLICIT INTEGER (a-z)
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

c        print *,' apsVFIX ',a,i,c,k,n
        iinc = 0
        DO 100 ii = 0, n-1
           iapscr(ii) = apdata(a+iinc)
           iinc = iinc + i
  100   CONTINUE
        kinc = 0
        DO 200 ii = 0, n-1
           iapdata(c+kinc) = iapscr(ii)
           kinc = kinc + k
  200   CONTINUE
        return
        end

      SUBROUTINE apsCONV(a,i,b,j,c,k,n,m)
c-------------------------------------------------------------------------------
c AP convolution simulator
c-------------------------------------------------------------------------------
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp

      INTEGER a, b, c
c      print *,' apsCONV ',a,i,b,j,c,k,n,m
      kk = 0
      DO 200 ii = 1, n
         answ = 0.
         iindex = ii - 1
         jindex = 0
         DO 100 jj = 1, m
            answ = apdata(a+iindex) * apdata(b+jindex) + answ
            iindex = iindex + i
            jindex = jindex + j
  100    CONTINUE
         apdata(c+kk) = answ
         kk = kk + k
  200 CONTINUE
      RETURN
      END
C
        SUBROUTINE apsVCLR(a,i,n)
C******************************************************************************
        INTEGER a
C Include file VapCry.inc
C Used to by Low Level AP simulator routines
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
      integer SZAPMEM, MAXNX, NAUX, SZAPSCR
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (MAXNX   =  16384)                                       ! Max. no of traces as given in FDMIEX
      parameter (NAUX    =     6)                                       ! No. of time slices in APSCR
      parameter (SZAPSCR = NAUX*MAXNX)                                  ! Scratch array size

      real         apdata(0:SZAPMEM-1)
      integer     iapdata(0:SZAPMEM-1)
      common /APMEM/ apdata                                             ! Veritas called this ap120bmd
      equivalence (APDATA,IAPDATA)
c
      real     apscr(0:SZAPSCR)
      integer iapscr(0:SZAPSCR)
      common/SCRAP/apscr
      equivalence (apscr,iapscr)
c
c Define some offsets into the scratch array. These are set by SETAUX
      integer
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
      common/AUXCM/
     $   auxscr1, auxscr2, auxscr3, auxscr4, auxscr5, auxscr6
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        integer apsp(0:15)
        common/AP120BSP/apsp


c        print *,' apsVCLR ',a,i,n
        k = 0
        DO 100 ii = 0, n-1
           apdata(a+k) = 0.
           k = k+i
  100 CONTINUE
        RETURN
        END
