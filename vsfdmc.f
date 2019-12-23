C File VAPMN_CRAY
C  This file contains the mid-level routines for the Finite Difference Migration
C They are responsible for the calculating the next time slice at the current
C Tau step.
C
C  This is the CRAY version of the routines which have been rewritten so that
C they vectorize on the CRAY. Also the order of calculation has been modified
C to reduce the Number of floating point operations per time slice. This
C is at the expense of greater memory use through APSCR
C
C  VSFDMCAL - Entry routine from FDMLIN. Responsible for calling routines to
C             calculate RHS & LHS of difference equation. Also shift time
C             slices in memory in preparation for next calculation.
C  VSFDMCAP - Responsible for calculating g on LHS of Recurrence relation.
C             Calls routines to calculate P at this time slice and Tau Step
C  VSFDMCAR - Calculates the RHS of the Difference equation.
C
C  VSFDMRUV - A glue routine that calls VSRUVAPS
C  VSRUVAPS - AP Simulator routine to recusively calculate U & V. This is an
C             intermediate stage to calculating the new Pressure
C
C  VSFDMPN1 - A glue routine that calls VSPN1APS
C  VSPN1APS - AP Simulator routine that calculate the new P given U and V
C
C===============================================================================


        subroutine VSFDMCAL (ac, apn, apnj1, apnj2, apn1, apn1j1,
     +          apn1j2, as3, mx)
        INTEGER ac, apn, apnj1, apnj2, apn1, apn1j1, apn1j2, as3, mx
C-------------------------------------------------------------------------------
C FDM: Calculate the cR" terms and the next time slice, etc. (simulator)
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: April, 1985
C
C ENTRY NAMES:  APSFDMCAL
C
c FUNCTION:     1- Calculates the three "R" terms. (See FDMCAR)
c               2- Recursively calculates the next Tau sample P(n+1).
c                  (See FDMCAP)
c               3- Moves the calculated sample back in place and "rotates" the
c                  samples for calculating the next Tau sample.
c
c PARAMETERS:   All parameters beginning with the letter "A" refer to main data
c               (MD) memory locations. (Further notes in FDMCAR and FDMCAP.)
c               AC      = C'
c               APN     = P(n,j)
c               APNJ1   = P(n,j+1)
c               APNJ2   = P(n,j+2)
c               APN1    = P(n+1,j)
c               APN1J1  = P(n+1,j+1)
c               APN1J2  = P(n+1,j+2)
c               AS3     = scratch
c               MX      = Number of traces in each time slice.
c
C CALLED BY:    FDMCAL (which is called by FDMLIN)
C
C EXTERNALS:    FDMCAR, FDMCAP, VMOV
C
C ERRORS & LIMITATIONS: none
C
C REFERENCES:   1- H.Brysk, GEOPHYSICS, volume 48, no.5 (May 1983), p(532-542).
C               2- FDMCAR and FDMCAP notes and FDM design notes.
C REVISIONS:
C
C AUTHOR:   a.j.h                           DATE:  4/11/89
C DESCRIPTION:   Added Call to blkmov to shift the Time slices in APSCR
C-------------------------------------------------------------------------------
C
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

C
        INTEGER ar, ag, au, av, as1
C
C....   Assign scratch arrays and do it.
C
        ar = apn1                                                       ! Setup AP memory location equivalences
        ag = as3
        au = apn1j2
        av = apnj2
        as1= apn1j2
C..                                        Calculate RHS of Recursion relation
        CALL VSFDMCAR (ac, apn, apnj1, apnj2, ar, apn1j1, apn1j2, as3,
     +   mx)
C..                                        Calculate updated P at this time step
        CALL VSFDMCAP (apn, apn1, ag, au, av, ac, ar, as1, mx)
C
        CALL apsVMOV (apnj1,1,  apnj2,1,  mx)                           ! Shift time slices in AP memory
        CALL apsVMOV (apn,1,    apnj1,1,  mx)
        CALL apsVMOV (apn1j1,1, apn1j2,1, mx)
        CALL apsVMOV (apn1,1,   apn1j1,1, mx)
        CALL apsVMOV (apn1,1,   apn,1,    mx)
        CALL blkmov  (auxscr3, auxscr5, mx)                             ! Move   (P(n+1,j+1) - P(n,j+1))
        CALL blkmov  (auxscr4, auxscr6, mx)                             ! Move T*(P(n+1,j+1) - P(n,j+1))
        RETURN
        END                                                             ! of routine VSFDMCAL

C===============================================================================


        SUBROUTINE vsfdmcap (apn, apn1, ag, au, av, ac, ar, as1, mx)
        INTEGER apn, apn1, ag, au, av, ac, ar, as1, mx
C-------------------------------------------------------------------------------
C$R     FDM: Calculate the next time slice for the current Tau step. (simulator)
C
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  APSFDMCAP
C
c FUNCTION:     For the current sample we require the following recursions:
c
c               (1) P'(m) = v(m) - u(m)P'(m+1)
c
c                   P(n+1,j,m) = P(n,j,m) + P'(m), for m = MX to 1.
c
c               (2)                g(m)
c                   u(m) = ----------------------,
c                          1 - 2g(m) - g(m)u(m-1)
c
c                                (         R(m) )
c                   v(m) = -u(m) (v(m-1) - ---- ), m = 1, MX.
c                                (         g(m) )
c
c               Where:  g(m)    = gamma - a*theta - b
c                       a       = delta-tau * C'
c                       b       = delta-t / 4 * C'
c                       C'      = (C**2 * delta-t) / (4 * delta-x**2)
c                       C       = 1/2 of the RMS velocity for the Tau step.
c                       P(n+1,j,m) = samples calculated at the current Tau step.
c                       P(n,j,m)= samples at the previous Tau step.
c                       P'(m)   = intermediate result (not saved - see FDMPN1)
c                       u(m)    = intermediate result (not saved - see FDMRUV)
c                       v(m)    = intermediate result (not saved - see FDMRUV)
c                       R(m)    = sum of 3 terms (see FDMCAR)
c
c               The processing order is
c                       1 - Calculate g.
c                       2 - Calculate u and v using equation (2) (see FDMRUV).
c                       3 - Calculate P(n+1,j) using equation (1) (see FDMPN1).
c
c PARAMETERS:   All parameters beginning with the letter "A" refer to main data
c               (MD) memory arrays.
c               All arrays are input except for APN1 (output) and AS1 (scratch).
c
c               APN     = P(n,j) - Sample j from previous Tau step n for all m.
c               APN1    = P(n+1,j) - Output sample j for Tau step n+1 for all m.
c               AG      = g.
c               AU      = u.
c               AV      = v.
c               AC      = C'.
c               AR      = R.
c               AS1     = Scratch.
c               MX      = X dimension of time slice (number of traces).
c
c               The following constants are assumed to be in lower main data
c               memory locations when this routine is called:
c               parameter       location        reference name
c               gamma            3              AGAMMA
c               theta            7              ATHETA
c               delta-tau        8              ADTAU
c               delta-t / 4     10              ADT4
c
C CALLED BY:    FDMCAP (which is called by FDMLIN)
C
C EXTERNALS:    VCLR, VMUL, VSUB, FDMRUV and FDMPN1.
C
C ERRORS & LIMITATIONS: Refer to FDMCAP.VFC
C
C REFERENCES:   Refer to FDMCAP.VFC
C
C REVISIONS:
C AUTHOR:  a.j.h                                     DATE:  4/11/89
C DESCRIPTION:  Changed calculation of g(m) so that it is calculated in a
C               single vector loop. CDIR$ IVDEP forces CRAY to ignore possible
C               vector dependencies.
C-------------------------------------------------------------------------------
C
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

C
        INTEGER agamma, atheta, adtau, adt4
C
        agamma =  3                                                     ! Set up AP low memory locations
        atheta =  7
        adtau  =  8
        adt4   = 10
C
C....   1 - Calculate g.
          tpscal = apdata(adtau) * apdata(atheta) + apdata(adt4)
          tpgam  = apdata(agamma)
c
c.. Ignore vector dependency
CDIR$ IVDEP
          do 10 i = 0, mx-1
           apdata(ag + i) = tpgam - tpscal * apdata(ac+i)
c	 if( apdata(ag + i) .EQ. 0 ) THEN
c	 print *,' i=',i,' ag=',ag,apdata(ag + i),tpgam,tpscal,apdata(ac+i)
c  i=  0 ag=  4999950  0.   0.125000    5.06000E-02    2.47036
c         ac = C'
c                       C'      = (C**2 * delta-t) / (4 * delta-x**2)
c                       C       = 1/2 of the RMS velocity for the Tau step.
c	 stop
c	 endif
  10      continue
C
C....   2- Calculate u and v.
        CALL vsfdmruv (ag, ar, au, av, mx)
C
C....   3- Calculate P(n+1,j).
        CALL vsfdmpn1 (apn, au, av, apn1, mx)
        return
        END                                                             ! of routine vsfdmcap

C===============================================================================

        SUBROUTINE vsfdmcar (ac, apn, apnj1, apnj2, ar, apn1j1,
     +          apn1j2, as3, mx)
        INTEGER ac, apn, apnj1, apnj2, ar, apn1j1, apn1j2, as3, mx
C------------------------------------------------------------------------------
C$R     FDM: Calculate the 3 cR" terms. (Simulator)
C
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  APSFDMCAR
C
c FUNCTION:     This routine evaluates the terms on the right-hand side of the
c               following equation:
c               R(m) = [1 - (gamma - a*theta - b) * T] (P(n+1,j,m) - P(n,j,m)
c                =   2*rho [1 - (gamma+b) * T]
c                               (P(n+1,j+1,m) - P(n,j+1,m)              (term 1)
c                  - rho**2 [1 - (gamma + a*theta -b) * T]
c                               (P(n+1,j+2,m) - P(n,j+2,m))             (term 2)
c                  + a*T (rho**2 * P(n,j+2,m) - P(n,j,m)),              (term 3)
c               where
c                       m       = 1 to MX.
c                       a       = delta tau * C'
c                       b       = delta t/4 * C'
c                       C'      = (C**2 * delta t) / (4 * delta x**2)
c                       C       = 1/2 of the RMS velocity for the Tau interval.
c                       P(n,j..j+2) = Samples from the previous Tau step, n.
c                       P(n+1,j..j+2) = Samples of the current Tau step, n+1.
c                                 (P(n+1,j) is the desired time slice.)
c                       T       = 3 point convolutional operator = (-1, 2, -1).
c                       R       = The calculated value of the three terms.
c
c PARAMETERS:   All parameters begining with the letter "A" refer to main data
c               (MD) memory locations and arrays. All arrays are input except
c               AR and AS3. (NOTE: All arrays are assumed to have 0-elements at
c               at either end so that the convolution with the 3 point "T"
c               operator will work. Ie. APN(-1)=0 and APN(MX)=0.)
c               AC      = C'.
c               APN     = P(n,j)
c               APNJ1   = P(n,j+1)
c               APNJ2   = P(n,j+2) (will be scratched)
c               AR      = R. (output)
c               APN1J1  = P(n+1,j+1)
c               APN1J2  = P(n+1,j+2) (will be scratched)
c               AS3     = scratch.
c               MX      = X-dimension of each time slice (number of traces).
c
c               The following constants are expected in lower MD memory:
c               parameter       location        referenced as
c               T               0..2            AT
c               gamma            3              AGAMMA
c               rho              4              ARHO
c               rho**2           5              ARHOSQ
c               2 * rho          6              A2RHO
c               theta            7              ATHETA
c               delta tau        8              ADTAU
c               delta t          9              ADT
c               delta t / 4     10              ADT4
c
C
C CALLED BY:    FDMCAR (which is called by FDMLIN).
C
C EXTERNALS:    TCONV
C
C ERRORS & LIMITATIONS: None.
C
C REFERENCES:   Refer to FDMCAR.VFC
C
C REVISIONS:
C AUTHOR:  a.j.h.                                   DATE: 11/4/89
C DESCRIPTION:
C    This routine was almost completely rewritten although it is functinally
C equivalent to the previous AP simulator code.
C    This version calculates most of R in a single vector do loop. However, it
C does calculate the majority of term 3 separately
C    It uses (P(n+1,j+2) - P(n,j+2)) and T*(P(n+1,j+2) - P(n,j+2)) stored in
C APSCR from the previous time step
C   The final do loop is somewhat ungainly and even on the Cray it may be better
C to split into smaller vector loops
C-------------------------------------------------------------------------------
C
        INTEGER agamma, arhosq, a2rho, atheta, adtau, adt4
C
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

C
C....   Define some other array names and constants.
C
        agamma  = 3                                                     ! gamma
        arhosq  = 5                                                     ! rho ** 2
        a2rho   = 6                                                     ! 2 * rho
        atheta  = 7                                                     ! theta
        adtau   = 8                                                     ! delta tau
        adt4    =10                                                     ! delta t / 4
C
C
      sc2rho  = apdata(a2rho)
      sc2     = sc2rho * apdata(agamma)
      sc3     = sc2rho * apdata(adt4)
      scrhosq = apdata(arhosq)
      scdtau  = apdata(adtau)
      sc6     = scrhosq * apdata(agamma)
      sc7     = scrhosq * (scdtau * apdata(atheta) - apdata(adt4))
C
C....   Calculate term 3 T* (rho**2 * P(n,j+2) - P(n,j)):
      do 10 ii = -1, mx+1                                               ! Must ensure end traces are 0. for T*
        apscr(auxscr1+ii) = scrhosq * apdata(apnj2+ii)
     $                                      - apdata(apn+ii)
   10 continue
C
      call TCONV(apscr(auxscr1-1),apscr(auxscr2),mx)
C
C.. The next two terms are reused at the next time step (see VSFDMCAL)
C
C...  Calculate term P(n+1,j+1) - P(n,j+1)
      do 20 ii = -1, mx+1                                               ! Must ensure end traces are 0. for T*
        apscr(auxscr3 + ii) = apdata(apn1j1 + ii) - apdata(apnj1 + ii)
   20 continue
C
C.... Calculate term T* (P(n+1,j+1) - P(n,j+1))
      call TCONV(apscr(auxscr3-1),apscr(auxscr4),mx)
C
C                                      Calculate R
C Ignore apparent Vector dependencies
CDIR$ IVDEP
      do 30 ii = 0, mx-1
        apdata(ar+ii) =
     $                           sc2rho * apscr(auxscr3+ii)
     $    - (sc2 + sc3 * apdata(ac+ii)) * apscr(auxscr4+ii)
     $    -                     scrhosq * apscr(auxscr5+ii)
     $    + (sc6 + sc7 * apdata(ac+ii)) * apscr(auxscr6+ii)
     $    +     scdtau * apdata(ac+ii)  * apscr(auxscr2+ii)
   30 continue
      RETURN
      END                                                               ! of routine VSFDMCAR

C===============================================================================

        SUBROUTINE vsfdmpn1 (pn, u, v, pn1, mx)
        INTEGER pn, u, v, pn1, mx
C-------------------------------------------------------------------------------
C$R     FDM: Recursively calculate the next time slice, P(n+1,j). (Simulator)
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  APSFDMPN1
C
C COMMON:       /AP120B_MD/ - Main Data (MD) memory simulator buffer.
C
C CALLED BY:    FDMPN1.APL (which is called by FDMCAP.VFC)
C
C EXTERNALS:    FDMPN1APS.FOR
C
C ERRORS & LIMITATIONS: Efficiency of this code may be improved by writing in
C               Macro.
C-------------------------------------------------------------------------------
C
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

        CALL vspn1aps (apdata(pn), apdata(u), apdata(v),
     +     apdata(pn1), mx)
        RETURN
        END                                                             ! of routine VSFDMPN1

C===============================================================================

        SUBROUTINE vsfdmruv (g, r, u, v, mx)
        INTEGER g, r, u, v, mx
C-------------------------------------------------------------------------------
C$R     FDM: Recursively calculate u and v. (Simulator)
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  APSFDMRUV
C
C FUNCTION:     Refer to FDMRUV.APL
C
C PARAMETERS:   Refer to FDMRUV.APL
C
C COMMON:       /AP120B_MD/ - Main Data (MD) memory buffer for simulator.
C
C CALLED BY:    FDMRUV.APL from APSFDMCAP.FOR from FDMCAP.FOR.
C
C EXTERNALS:    FDMRUVAPS.FOR
C
C ERRORS & LIMITATIONS: Efficiency may be improved by writing in Macro.
C
C REFERENCES:   Refer to FDMRUV.APL
C
C-------------------------------------------------------------------------------
C
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

C
        CALL vsruvaps (apdata(g), apdata(r), apdata(u), apdata(v),
     +       mx)
        RETURN
        END                                                             ! of routine VSRUVAPS

C===============================================================================

        SUBROUTINE vsruvaps (g, r, u, v, mx)
        REAL    g(mx), r(mx), u(mx), v(mx)
        INTEGER mx
C-------------------------------------------------------------------------------
C$R     FDM: Recursively calculate u and v. (Simulator)
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  VSRUVAPS
C
C FUNCTION:     Refer to FDMRUV.APL
C
C PARAMETERS:   Refer to FDMRUV.APL
C
C CALLED BY:    (Subroutine call tree:)
C               - FDM.FOR
C                - FDMLIN.FOR
C                 - FDMCAR.VFC
C                  - APSFDMCAR.FOR
C                   - FDMRUV.APL
C                    - APSFDMRUV.FOR
C                     - FDMRUVAPS.FOR
C
C EXTERNALS:    None
C
C       REVISIONS:
C AUTHOR:  a.j.h.                                     DATE:  4/11/89
C DESCRIPTION:
C    The single vector loop was split into 4 loop for the CRAY. This split
C the vector dependent part of the calculations from the vectorizable parts
C-------------------------------------------------------------------------------
c Mod: PCH, 8 June 2004, Check for divide by zero.  Gamma can become zero!
C
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

        INTEGER m
        REAL    um1, vm1
C
c****   If you get a divide by zero fault, it probably here.  g = gamma,
c****   which can become zero if the velocity doesn't increase.  I hate
c****   to put this check in, but I can't tell you how much time it took
c****   me to find this quirk.
        DO 50  m = 1,mx
           apscr(m) = 1./g(m) - 2.
           IF( g(m) .EQ. 0. ) THEN
               PRINT *,' FDMIGR fault - divide by zero.'
               PRINT *,' Some velocity is not increasing.'
               STOP
           ENDIF
   50   CONTINUE
C
        um1 = 0.
        DO 100 m = 1, mx
          u(m) = 1. / (apscr(m) - um1)
          um1  = u(m)
  100   CONTINUE
C
        DO 150 m = 1, mx
           apscr(m) = u(m) * r(m)/g(m)
  150   continue
c
        vm1 = 0.
        DO 200 m = 1, mx
          v(m) = -u(m) * vm1 + apscr(m)
          vm1  = v(m)
  200   CONTINUE
c       call dbgap(1)
        RETURN
        END                                                             ! of VSRUVAPS
C
C==============================================================================
C
        SUBROUTINE vspn1aps (pn, u, v, pn1, mx)
        REAL    pn(mx), u(mx), v(mx), pn1(mx)
        INTEGER mx
C-------------------------------------------------------------------------------
C$R     FDM: Recursively calculate the next time slice, P(n+1,j). (Simulator)
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C
C AUTHOR:       N.M.M.                          DATE: March, 1985
C
C ENTRY NAMES:  FDMPN1APS
C
C FUNCTION:     Refer to FDMPN1.VFC
C
C PARAMETERS:   Refer to FDMPN1.VFC
C
C CALLED BY:    (Tree of calls to this routine:)
C               - FDMIEX
C                - FDMLIN
C                 - FDMCAR.VFC
C                  - APSFDMCAR.FOR
C                   - FDMPN1.APL
C                    - APSFDMPN1.FOR
C                     - FDMPN1APS.FOR
C
C EXTERNALS:    None
C
C ERRORS & LIMITATIONS: None
C
C REVISIONS:
C AUTHOR: a.j.h.                                     DATE:  4/11/89
C DESCRIPTION:  The single do loop was split into two to allow vectorization
C on the CRAY
C-------------------------------------------------------------------------------
C
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

C
      INTEGER m
C
      apscr(mx+1) = 0.
c
      DO 100 m = mx, 1, -1
          apscr(m) = v(m) - u(m)*apscr(m+1)
  100 CONTINUE
c
      DO 200 m = mx, 1, -1
          pn1(m) = pn(m) + apscr(m)
  200 CONTINUE
c     call dbgap(2)
      RETURN
      END                                                               ! of routine VSPN1APS
C
C==============================================================================
C
      subroutine setaux(lslice)
      integer lslice
C-------------------------------------------------------------------------------
C   Setaux is responsible for setting up the auxilary AP scratch array APSCR.
C This array holds intermediate results between time slices as well as scratch
C results during calculation.
C   This routine must be called once a Tau step so as to initialize the j+2 time
C slicesat auxscr5/6 to zero.
C
C In
C   lslice : The length of a time slice
C
C  Located at:
C  auxscr1 - Scratch time slice
C  auxscr2 - Scratch time slice
C  auxscr3 - Holds    P(n+1,j+1) - P(n,j+1)
C  auxscr4 - Holds T*(P(n+1,j+1) - P(n,j+1))
C  auxscr5 - Holds    P(n+1,j+2) - P(n,j+2)
C  auxscr6 - Holds T*(P(n+1,j+2) - P(n,j+2))
C-------------------------------------------------------------------------------
C
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

      logical first
      save    first
      data    first /.TRUE./
C
      if (first) then
        first = .FALSE.
        if (lslice.gt.MAXNX) then
          print *,' Size of time slice: ',lslice,
     $            ' exceeds Maximum allowed ', MAXNX
          STOP
        endif
C
        auxscr1 = 1                                                     ! offset to allow for zero pads
        auxscr2 = auxscr1 + lslice
        auxscr3 = auxscr2 + lslice
        auxscr4 = auxscr3 + lslice
        auxscr5 = auxscr4 + lslice
        auxscr6 = auxscr5 + lslice
      endif
C
      do 10 i = auxscr5, auxscr6 + lslice - 1
        apscr(i) = 0.
  10  continue
      return
      end
C
C==============================================================================
C
      subroutine TCONV(a,c,n)
      real    a(0:n-1), c(0:n-1)
      integer n
c-------------------------------------------------------------------------------
c This routines does the 3 pt convolution horizontal convolution (-1,2,-1) for
c FDM. This is the CRAY version which computes it as a single vector loop.
c
c IN:
c   n   : length of data to be convolved
c   a() : array to be convolved.
c OUT
c   c() : output array
c-------------------------------------------------------------------------------
c
      DO 100 ii = 0, n-1
        c(ii) = -a(ii) + 2. * a(ii+1) - a(ii+2)
  100 CONTINUE
      return
      end
C
C==============================================================================
C
      subroutine blkmov(a,c,n)
      integer a,c,n
c-------------------------------------------------------------------------------
c This routine moves data within the AP scratch array.
c   IN
c    a  : location of start of data to shift
c    n  : Number of data to shift
c  OUT
c   c   : Location to shift to
c
c   The CRAY is told to ignore vector dependencies. There should OK for a
c straight shift of the data.
c
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

CDIR$ IVDEP
      do 100 ii = 0,n-1
        apscr(c+ii) = apscr(a+ii)
  100 continue
      return
      end
