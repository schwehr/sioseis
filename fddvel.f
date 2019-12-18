      SUBROUTINE fddvel (strzon, ntrzon, dt, dx, dtpool, rtpool, ntau,
     +  taustp, nx, lunvel, lenint, vpadss, vpadse, vpadgs, vpadge,
     +     line3d, ncrbln, nline, insamp, crbdum)
c
C-------------------------------------------------------------------------------
C$R   FDM___: Calculate RMS velocities over each Tau interval for the interval.
c     This returns a velocity function for EVERY trace (ntrzon traces).
C
C     VERITAS SOFTWARE LTD.                  CALGARY, ALBERTA, CANADA
C Author:      N.M.M.                        Date:      February, 1985
C Inputs:      RMS velocities for each trace at control points. (See array
C              DTPOOL.) Note that this RMS velocity is from time = 0 to length
C              of data, and this is NOT the same as the RMS velocity to be
C              calculated over the Tau intervals.
C
C Function:    1- Calculate the interval velocity V(n) at all samples of each
C                 CRB:
C                            -----------------------------------------
C                           / Vin(n)**2 * t(n+1) - Vin(n-1)**2 * t(n)
C                 V(n) =   /  ---------------------------------------
C                       /\/               t(n+1) - t(n)                ,
C                     n = 0 .. length of data (in steps of sample rate),
C                     Vin = input RMS velocity for the CRB.
C
C              2- For each Tau interval calculate 1/2 the RMS velocity over the
C                 interval times a factor for use in the migration calculation:
C                             --------------------------------
C                            /        sum (V(n)**2)
C                 C(n) =    /   -----------------------------
C                        /\/    4 * (t(i+1)/DT - t(i)/DT + 1) ,
C                     i = 1 .. the number of TAU intervals,
C                     n = t(i) .. t(i+1),
C                     DT = Sample rate (in seconds).
C
C                 Note that this is equivalent to:
C                            ------------------------------------------------
C                           /      1             ( V(n)**2                 )
C                          / ------------- * sum ( ------- * (t(n+1)-t(n)) )
C                       /\/  t(i+1) - t(i)       (    4                    )
C
C              3- Partially calculate g. G is used in the migration calculation
C                 and requires: (Refer to FDM programmers' notes.)
C                 a = delta-tau * C'
C                 b = DT/4 * C'
C                         C(n)**2 * DT
C                 C'(n) = ------------
C                           4 * DX**2 ,
C                     DX = Trace separation.
C
C              4- Determine the velocity at the zone of interest at the section
C                 ends. (This is for the trace pad calculation.)
C
C              *****************************************************************
C              * NOTE THE FOLLOWING:                                           *
C              *   A) The square root in point 1 above is NOT taken because of *
C              *      "V(n)**2" in point 2.                                    *
C              *   B) The square root in point 2 above is NOT taken because of *
C              *      "C(n)**2" in point 3.                                    *
C              *****************************************************************
C
C Outputs:     Array VRMS (2D calculation) or file 'FDMV' (3D).
C
C Calling Sequence:
C  STRZON = Start CRB of current zone (line).
C  NTRZON = # of CRB's in the zone.
C  DT     = Sample rate (seconds).
C  DX     = Trace separation (for 3D this is in the "in-line" direction).
C  DTPOOL = Array in which the velocity parameters will be contained. DTPOOL is
C           defined (230,200) to hold 198 control points (the first 2 rows have
C           a special use). The format of DTPOOL is now described:
C    (1,1)= 3 (Always. This is the row index of the first control point.
C    (2,1)= Index of the last control point. (The number of control points =
C           DTPOOL(2,1)-2.
C    (3..230,1) = Unused.
C    (1..230,2) = Interpolation buffer (used in FDMVEL).
C    (1,ix) = Type of input function. (1=TRMS, 2=TVI, 3=TNMO, 4=TDEP)
C    (2,ix) = Control point interpolation code (1="S",2="C" or 3="E")
C    (3,ix) = CRB number of control point.
C    (4,ix) = Distance at which NMO function picked (TNMO only).
C    (5,ix) = Number of time, velocity pairs read at this control point.
C    (6....117,ix) = Control times of function.
C    (118..230,ix) = Velocities at control times.
C  NTAU   = # of Tau steps.
C  TAUSTP = Time of each Tau step.
C  TAUSIZ = Delta-tau for each Tau step.
C  VRMS   = Output velocity (2D only).
C           (NOTE: This is referred to as " C' " in the notes.)
C  LENINT = Greatest time in zone of interest.
C  VPADSS = Interval velocity (V(n)) for 1st CRB at zone of interest.
C  VPADSE = Interval velocity (V(n)) for last CRB at zone of interest.
C           (For 3D VPADSS & VPADSE are the maximum V(n) in the zone of interest
C           at all CRB's that are the start or end of a section.)
C  VPADGS = Same as VPADSS but for 3D cross-lines.
C  VPADGE = Same as VPADSE but for 3D cross-lines.
C  LINE3D = Flag for 3D.
C  NCRBLN = # of CRB's in a line (3D).
C  NLINE  = # of lines (cross-lines) in the 3D survey.
C  INSAMP = # of samples input.
C  CRB    = Current CRB #.
C          
C EXTERNALS:
C     AVBUFIN
C
C REVISIONS:
C  Author:   N.M.M.                           Date: August, 1985
C Description: Use interval velocity at the bottom of each Tau step (rather than
C             the middle) so that velocities may be tapered over the Tau step
C             samples to the previous Tau step samples to properly merge steps.
C
C Revised by:   N.M.M.                          Date:   May, 1987
C Reason:       Add 8192 sample limit with no sample rate or length restriction.
c
c  17 June 1988, by pch to make f77 for non-vms, non-ap,  and non-veritas!
c  August - pch.  the vrms array is a virual array, not in Cray!  So,
c                vrms is now stored on disc.  vrms contains a velocity
c                for every tau step of every trace.  It must be 
c                transposed too, all of each time steps next to each
c                  other
c           Also think in seconds rather than milleseconds.
c  Sept - pch   CRB is an argument, but it is used as a do loop
c               counter, so I changed the argument to crbdum
c
C Non-standard Fortran-66 items:
C    1- IMPLICIT NONE statement.
C    2- Common block names and/or external routines with names > 6 characters.
C    3- Use of %REF or %VAL functions.
C-------------------------------------------------------------------------------
C
      INTEGER crbdum
C
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *               iuseap, idecimf, mdsize
      COMMON /apmem/ apdata(0:65536)                                      ! it is really mdsize big
c      IMPLICIT NONE
C
      PARAMETER ( MAXSMP = 8192 )
      INTEGER  strzon,   ntrzon,   ntau,     lenint,   ncrbln,   nline,
     +         insamp,   crb,
     +         dtpool(230,200),    taustp(ntau)
      REAL     rtpool(230,200)
C     equivalence(dtpool,rtpool)

      REAL     dt,       dx,       vpadss,   vpadse,   vpadgs,   vpadge
      LOGICAL  line3d
C
      REAL     vin(112)
c      integer   VIN(112)
      INTEGER  crbst,    crben,    list(5),  time(112),
     +         nv,       i,        lsamp,    n,        esamp,
     +         sr,       tau,      sp,       sc,       s,        c
      REAL     gbase,    vn,       v,        vl,       tl,
     +         tc,       tp,       velint,   veltau,   veloc(MAXSMP)
C
      DATA     list      /118, 6, 5, 4, 0/
      DATA nsofar/1/, icol/1/
C
C
C.... Calculate some loop indeces and partially calculate G. (see notes above)
c      print *,' strzon=',strzon,' ntrzon=',ntrzon,' dt=',dt,' dx=',
c     * dx,' lunvel=',lunvel,' lenint=',lenint
c      print *,vpadss,vpadse,vpadge,vpadge
c      print *,' line3d=',line3d,' ncrbln=',ncrnln,' nline=',nline,
c     *   ' insamp=',insamp,' crbdum=',crbdum
c
      IF( insamp .LE. 0 ) STOP
      maxcol = mdsize / insamp
      maxco  = maxcol
      sr     = nint(dt * 1000.0)
      crbst  = strzon
      crben  = strzon + ntrzon - 1
      vpadss = 0.0
      vpadse = 0.0
      vpadgs = 0.0
      vpadge = 0.0
      gbase  = dt / (4.0 * dx**2)
C
C.... Get the velocity for each CRB in the zone.
      DO 100 crb = crbst, crben
C
C....     Interpolate the input (possibly bulked) RMS velocity functions to get
C....     the velocity for this CRB.
c****     crb can be sent in dtpool(2,2) or as an argument - I chose argument
          CALL avbufin (dtpool, rtpool, 230, 2, -3, list, crb)
C
C....     Convert RMS to interval velocity and fill a velocity array with the
C....     interval velocity at every sample. (Refer to note 1 above.)
          nv = dtpool(5,2)
c
          DO 10 i = 1, nv
             time(i) = dtpool(5+i,2)
             vin(i) = rtpool(117+i,2)
   10     CONTINUE
c
          v     = vin(1)
          tim   = time(1)
          lsamp = 1
          DO 110 n = 2, nv
C
C....          Calculate the interval velocity.
               vl  = v
               tl  = tim
               v   = vin(n)
               tim = time(n)
               vn  = (v**2 * tim - vl**2 * tl) / (tim-tl)
c              call fdmChck1(crb, n-1, tl, tim, vn)
C
C....          Now fill the velocity array with the interval velocity at all
C....          samples in the interval.
               esamp = nint(tim / sr) + 1
               if (esamp .gt. insamp) esamp = insamp
               if (esamp .ge. lsamp) then
                   DO 80 i = 1, esamp-lsamp+1
                      veloc(lsamp+i-1) = vn
   80              CONTINUE
                   lsamp = esamp + 1
               END IF
  110     CONTINUE
c
c          IF (LSAMP .LE. INSAMP)
c     +         CALL SET (VELOC(LSAMP), INSAMP-LSAMP+1, VN)
c
          IF ( lsamp .le. insamp) THEN                                  ! Pad to end of trace with vint
              DO 120 i = 1, insamp-lsamp+1
                 veloc(lsamp+i-1) = vn
  120         CONTINUE
          ENDIF
C
C....     Get V(n) at the zone of interest.
          velint = sqrt(veloc(lenint/sr))
C
C....     Start the Tau step loop to calculate C' (1/2 the RMS velocity over the
C....     Tau interval). Refer to the note above and note that the square root
C....     is never actually taken. (Refer to notes 2 and 3 above.) 
c**** GMK had to change velocity taustep loop to include tau step at t = 0
c**** GMK thus, no taustp exists at the bottm of the section ( t = insamp)
c**** GMK because of this, averaging loop was modified below 
          sp = 1
          DO 200 tau = 1, ntau
c
               tc = taustp(tau+1)                                       ! Update End of tau interval
               sc = tc / float(sr) + 1.0                                ! Sample number
               sc = min (sc, insamp)        

c**** GMK Since there is not a (tau+1) taustp need to set sc = to last samp
c**** GMK thereby averaging over entire taustep
               if (tau.eq.ntau) then
                 sc = insamp
               endif
c
               veltau = 0.0                                             ! Average velocity over tau step
               DO 300 s = sp, sc
                  veltau = veltau + veloc(s)          
  300          CONTINUE
               veltau     = veltau / (4.0 * (sc-sp+1))
               veloc(tau) = veltau * gbase                              ! Store average back in veloc 
c              call fdmChck2(crb,tau,sp,sc,veltau,dt,dx)
               sp = MIN (sc+1, insamp)                           
  200     CONTINUE
                  
C....     If the line is a 2D line then move C' into the array VRMS for use by
C....     routine FDMLIN. Don't forget to find the velocity of the zone of
C....     interest at the end CRB's.
          IF (.not.line3d) THEN
c****                  Multiplex the velocity function and save it on disk
               jndex = crb - crbst + 1
               index = jndex-((jndex-1)/maxcol)*maxcol
               DO 400 tau = 1, ntau
c                    VRMS(CRB-CRBST+1,TAU) = VELOC(TAU)
                  apdata(index) = veloc(tau)
                  index         = index + maxco
  400          CONTINUE
               icol = icol + 1
               IF( jndex .EQ. ntrzon )
     $            maxcol = icol - 1                                     ! Force O/P: Last CDP done.
               IF( icol .GT. maxcol ) THEN
                   index = 1
                   n = icol - 1
                   DO 420 i = 1, ntau
                      ipos = (i-1)*nx+nsofar
                      CALL podisc( lunvel, 1, ipos)
                      CALL wrdisc( lunvel, apdata(index), n )
                      index = index + maxco
  420              CONTINUE
                   nsofar = nsofar + maxcol
                   icol   = 1
               ENDIF
               
               IF (crb .eq. crbst) vpadss = velint
               IF (crb .eq. crben) vpadse = velint
C
C....     But if this is a 3D line then write C' to file 'FDMV' and check for
C....     the maximum velocity in the zone of interest at the boundaries of the
C....     grid.
          ELSE
c               CALL FILEWRIT ('FDMV', CRB, VELOC)
C
C....          Check for the maximum velocity in the zone of interest in the
C....          first and last lines of the survey.
               IF (CRB .LE. NCRBLN)       VPADGS = MAX (VPADGS, VELINT)
               IF (CRB .GT. CRBEN-NCRBLN) VPADGE = MAX (VPADGE, VELINT)
C
C....          Now check the start and end sections (first and last
C....          "cross-lines").
               DO 501 c = crbst, crben, ncrbln
                  IF (crb .eq. c) vpadss = max (vpadss, velint)
  501          CONTINUE
               DO 502 c = crbst+ncrbln-1, crben, ncrbln
                  IF (crb .eq. c) vpadse = max (vpadse, velint)
  502          CONTINUE
          END IF
  100 CONTINUE
      RETURN
      END
