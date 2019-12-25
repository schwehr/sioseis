      subroutine fdmdif (gamma, rho, theta, dt, ntr, pads, pade, ntau,
     +    taustp, tausiz, nsamp, lunvel, nx, lunt, nrows,
     +    ncols, nrho, fcrho, big)
C-------------------------------------------------------------------------------
C     VERITAS SOFTWARE LTD.                  CALGARY, ALBERTA, CANADA
C
C AUTHOR:      N.M.M.                        DATE: April, 1985
C
C ENTRY NAMES: FDMLIN
C
C FUNCTION:    Apply 45-degree finite-difference migration algorithm to one
C              line. This routine provides the looping through all Tau steps,
C              migrating seismic data from and back to array buf in the
C              process. This routine also handles data transfers to/from memory
C              and AP. The maximum number of time slices that can be held
C              in the AP will be packed in a large array and transferred to the
C              AP to minimize costly AP transfers.
C
C PARAMETERS:  GAMMA (R4) = Constant in migration calculation.
C              RHO   (R4) = Constant in migration calculation.
C              THETA (R4) = Constant in migration calculation.
C              DT    (R4) = Delta t - sample rate in seconds.
C              NTR   (I4) = Number of traces to migrate in line.(no pads)
C              PADS  (I4) = Trace pad for start of line (always >= 1).
C              PADE  (I4) = Trace pad for end of line (always >= 1).
C              NTAU  (I4) = Number of Tau steps.
C              TAUSTP(NTAU) (I4) = Times of velocity functions for all Tau
C                                  steps.
C              TAUSIZ(NTAU) (I4) = Delta Tau for all Tau steps (ms.). (The time
C                                  between adjacent velocity functions.)
C              NSAMP (I4) = Number of samples.
C              VRMS(NTR,NTAU) (R4) = Velocity functions for all Tau steps.
C              PFDMO  = (unused)
C              BLOCKO = (unused)
C              TRC_INFO(NROWS,NCOLS) = (R4) Multiplexed data buffer.
C              NROWS  (R4) = Spatial dimension of line = PADS + NTR + PADE.
C              NCOLS  (R4) = Temporal dimension of line = NSAMP.
C              BLOCKX (R4) (unused)
C              SLICES (R4) (unused)
C              NRHO (R4) = Constant for tapering Rho.
C              FCRHO (R4) = Constant for tapering Rho.
C
C INPUTS:      Array buf - This virtual memory array is defined in program
C                            FDM. This array contains the multiplexed traces
C                            of the line that is to be migrated. Each pass
C                            through the line will cause buf to be updated
C                            with the sample values at the next Tau step. (Ie.
C                            on entry the array is considered to contain sample
C                            values of Tau step 0.)
C
C OUTPUTS:     Array buf - On exit from this routine array buf will
C                            contain multiplexed, migrated traces. The array is
C                            built from the top down - samples from Tau step 0
C                            are added to the top of the section; samples from
C                            Tau step NTAU are added to the bottom of the
C                            section. (See the references for a complete
C                            description.)
C
C CALLED BY:   FDM
C
C EXTERNALS:   ABORT, DBUG3, FDMCAP, FDMCAR, MAPAPRA, MOVE, SET, VCLR
C
C COMMON BLOCKS: /APLOCS/  - Contains the AP-addressing for the current line
C                            being processed. (This is used for debugging only
C                            and should not be used elsewhere.)
C
C ERRORS & LIMITATIONS: (See FDM)
C
C REFERENCES:  -H.Brysk, GEOPHYSICS, Vol.48, No.5 (May 1983); P.532-542.
C              -Design notes for FDM, etc.
C===============================================================================
C      REVISIONS:
C AUTHOR:      N.M.M.                        DATE: August-September, 1985
C DESCRIPTION: 1 - Remove the Tau step interpolation of adjacent samples (into
C              file 'FDMO') and replace it with a taper upon the current Tau
C              step velocity function.
C              2 - Apply a Butterworth taper to Rho over the current Tau step:
C
C                               (              1              )
C                               ( --------------------------- )
C                  RHO1 = RHO * (     (   f   ) ** (2 * NRHO) )
C                               ( 1 + ( ----- )               )
C                               (     ( FCRHO )               )
C                  Where,
C                  f = (J - TAUP) / (TAUC - TAUP)
C                  J = sample being processed in current Tau step.
C                  TAUP = previous Tau step sample
C                  TAUC = current Tau step sample (velocity information  for Tau
C                         steps are accurate for Tau step samples, TAUC)
C                  Note: TAUC >= J >= TAUP
C
C AUTHOR:                                    DATE: August, 1986
C DESCRIPTION: Optimizations involving mostly changing file storage to virtual
C              memory storage.
C===============================================================================
C
C This subroutine was changed to progated energy in the opposite direction of migration
C (i.e. the receivers are upward continued through the section) thus calculating the response
C of an exploding reflector. To do this, the Tau step loop was altered to start at the bottom
C of the section and increment upward through the section. Tapering was also modified because the
C the sense of the tau step was changed.
C Modified by GM KENT 7/90
C ================================================================================
c mod Jan 1999 to change sr, an integer, to dt*1000, a float.
C
      integer    DEBUG, PRSTEP
      parameter (DEBUG  = 0)
      parameter (PRSTEP = 1)
c
      integer  ntr, pads, pade, ntau, taustp(ntau), tausiz(ntau), nsamp,
     +         nrows, ncols
      real     gamma, rho, theta, dt, nrho, fcrho
C
C.... AP-locations (All AP MD-locations start with "A". Except some locations
C     are referred to starting with the letter "P" - these refer to the next
C     location of the same mnemonic starting with "A". Ie. PC=AC+1.)
      INTEGER  at, agamma, arho, arhosq, a2rho, atheta, adtau, adt,
     +         adt4, ac, apn, apnj1, apnj2, apn1, apn1j1, apn1j2,
     +         as3, as2, av, ar, as1, au, ag, apnj0,
     +         pc, ppnj1, ppnj2, ppn1, ppn1j1, ppn1j2, ps3
      COMMON   /APLOCS/
     +         at, agamma, arho, arhosq, a2rho, atheta, adtau, adt,
     +         adt4, ac, apn, apnj1, apnj2, apn1, apn1j1, apn1j2,
     +         as3, as2, av, ar, as1, au, ag, apnj0
C
      INTEGER  lslice, mxslic, sr, taup, tauc, taun, jstart, jend,
     +         esamp, tau, j, lslic2, i
      REAL     buffap(8011), scale, rho1, f
      LOGICAL  big
C
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
        REAL    APDATA(0:5000000)
        INTEGER IAPDATA(0:5000000)
        COMMON /apmem/ apdata                                           ! Veritas called this ap120bmd
        EQUIVALENCE (APDATA,IAPDATA)
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        INTEGER APSP(0:15)
        COMMON/AP120BSP/APSP
C
      PARAMETER (maxnx = 4096)                                          ! the maximum number of traces allowed
      DIMENSION vrms(maxnx)                                             ! room for 1 velocity slice
c   transp holds the transpose matrix t
      PARAMETER (isize = 262144 )                                        ! 512x512
      COMMON /transp/ t(isize)
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *               iuseap, idecimf, mdsize                            ! see inap.f for description
C
C.... Define AP-arrays and constant locations. (Note that the conventions using
C.... MAPCLR, MAP and MAPRES have been discarded so that the entire AP main data
C.... memory may be used.)
C
C.... Constants go in the 1st 11 locations.
      at     = 0                                                        ! 3-point convolutional operator (-1, 2, -1)

      arho   = 4                                                        ! Rho
      arhosq = 5                                                        ! Rho**2
      a2rho  = 6                                                        ! 2*Rho
      atheta = 7                                                        ! Theta
      adtau  = 8                                                        ! Delta Tau
      adt    = 9                                                        ! Delta t
      adt4  = 10                                                        ! Delta t / 4
C
C.... Define the time slice length and the number that may be held at one time
C.... in the AP (in array APN). (Note that 7 additional vector locations are
C.... required.)
      lslice = ntr + pads + pade
      mxslic = (mdsize-11) / lslice - 7
      print *, mxslic,' Time slices of length ', lslice,
     *         ' traces can be held in the AP'
      if ( mxslic .le. 1 ) then
          print *,' Time slices + pads are too long for processing!'
          STOP
      endif
c      print *,' dt=',dt,' ntr=',ntr,' pads=',pads,' pade=',pade,
c     *        ' ntau=',ntau,' nsamp=',nsamp,' nx=',nx
c      print *,' nrows=',nrows,' ncols=',ncols,' big=',big
c
c.... Now define the 8 vector locations.
      ac    = 11                                                        ! Velocity information (C')
      apn   = ac + lslice                                               ! Samples P(n,j) overwritten with P(n+1,j).
      apnj1 = apn + lslice*mxslic                                       ! Sample P(n,j+1).
      apnj2 = apnj1 + lslice                                            ! Sample P(n,j+2).
      apn1  = apnj2 + lslice                                            ! Sample P(n+1,j).
      apn1j1= apn1  + lslice                                            ! Sample P(n+1,j+1).
      apn1j2= apn1j1 + lslice                                           ! Sample P(n+1,j+2).
      as3   = apn1j2 + lslice                                           ! Scratch.
C
C.... For clarity define some arrays that overwrite the defined arrays above.
      as2   = apnj2                                                     ! Scratch.
      av    = apnj2                                                     ! v. (See FDMPN1 & FDMRUV)
      ar    = apn1                                                      ! R. (See FDMCAR)
      as1   = apn1j2                                                    ! Scratch.
      au    = apn1j2                                                    ! u. (See FDMPN1 & FDMRUV)
      ag    = as3                                                       ! g. (See FDMCAR, FDMCAP)
C
C.... Set up "next locations" of some AP-arrays. This is done here to take this
C.... code out of the "200" loop below.
      pc = ac+1
      ppnj1  = apnj1+1
      ppnj2  = apnj2+1
      ppn1   = apn1+1
      ppn1j1 = apn1j1+1
      ppn1j2 = apn1j2+1
c
      ps3 = as3+1
      lslic2 = lslice-2
C
      sr   = nint( dt * 1000.)
C**** GMK modified due to the opposite sense of the tau steps in modelling
      taup = 1
c      taun = nsamp - (taustp(ntau)/sr + 1) + 1
      taun = nsamp - (taustp(ntau)/(dt*1000.) + 1) + 1
C
C.... Initialize the Tau step loop - set up the constants. (Delta Tau, the 9th
C.... item, will be filled inside the loop.)
      buffap(1) = -1.
      buffap(2) =  2.
      buffap(3) = -1.
      buffap(4) = gamma
      buffap(5) = rho
      buffap(6) = rho**2
      buffap(7) = 2. * rho
      buffap(8) = theta
      buffap(10)= dt
      buffap(11)= dt * .25
C
      if (DEBUG.ne.0) then
        print '(/A)',' About to Start Tau Step Loop'
        call timer
      endif

C**** GMK modified to start at the bottom of the seismic section and work its way up
c**** GMK thus all tau steps will end at the bottom and begin at bottom - nth (taustp)
C**** GMK This is opposite to migration that always starta at the bottom and stops at
C**** GMK the top - nth (taustp) --> tau = ntau, 1, -1 (DIFF) not tau = 1, ntau (MIG)

C.... Now loop on all Tau steps.(modified by GMK)
      do 100 tau = ntau, 1, -1
C
C....     Temporary: Recalculate Rho each time through the Tau loop (because
C....     Rho is scaled at the end of each step).
          buffap(5) = rho
          buffap(6) = rho**2
          buffap(7) = 2. * rho
C
c         dbgtau = tau                                                  ! Used by AP debugging
c         slclen = lslice                                               ! Used by AP debugging
          tauc = taup
          taup = taun
c**** GMK taun calculated for the next taustep
c          taun = nsamp - (taustp(tau-1)/sr + 1) + 1
          taun = nsamp - (taustp(tau-1)/(dt*1000.) + 1) + 1
c          write(*,*) 'tauc=', tauc, 'taup =', taup, 'taun =', taun

c**** GMK do not need to go through loop if not samples are going to be processed
          if (tauc.eq.taup) goto 100

c**** GMK esamp represents top of tau step
          esamp = taup
          jend  = esamp + 1                                             ! force the data to come from disc the first time
C
C
C....     Load the AP with constants and C' (velocity information). Get Delta
C....     Tau here.
          buffap(9) = tausiz(tau) / 1000.

c**** GMK since we counted down the tau step backwards velocity here needed not be changed
          ipos = (tau-1)*nx+1
          call podisc( lunvel, 1, ipos )
          call rddisc( lunvel, vrms(pads+1 ), ntr, istat )
          if( istat .ne. ntr ) then
              print *,' rddisc error at 160 in fdmlin, ipos=',ipos,
     *                ' ntr=',ntr
              stop
          endif
          if( pads .GT. 1 ) then
              do 160 i = 1, pads-1
  160         vrms(1+i) = vrms(pads+1)
          endif
c
          IF( pade .GT. 1 ) THEN
              DO 165 i = 1, pade-1
  165         vrms(pads+ntr+i) = vrms(pads+ntr)
          ENDIF
c
          vrms(1)             = 0.
          vrms(pads+ntr+pade) = 0.
c
          DO 170 i = 1, 11
  170     apdata(at+i-1) = buffap(i)
c
          DO 171 i = 1, nrows                                           !  Transfer velocity slice
  171     apdata(at+10+i) = vrms(i)

          DO 172 i = 1, lslice*6                                        !  Zero time slices at j+1, j+2
  172     apdata(apnj1+i-1) = 0.
c
          call setaux(lslice)                                           ! Zero auxiliary slices
c
c         call fdmChck3(3,esamp,tauc)
C
C....     Process all samples for this Tau step.
C**** GMK go from sample esamp(top of taustep) to 1(bottom of section) - remember that
C**** GMK the data is read in unflipped so the bottom of section(nsamp) is really samp = 1
C**** GMK so the loop goes from j = esamp to 1 not nsamp to esamp as in migration program

          DO 200 j = esamp, 1, -1
c         dbgtim = j                                                    ! used by ap debugging
C
C....          Get the next sample. If we don't have the sample we need then
C....          load up the AP with as many samples as will fit.
               if (jend .gt. j) then
                 jstart = j
                 jend   = max (j-mxslic+1, 1)                           ! GMK 1 replaces esamp in max(a,b)
c                  write(*,*) 'load ap jstart = ', jstart, 'jend = ', jend
                   nslice = jstart - jend + 1
                   call getslice(jstart,nslice,nsamp,nx,apn,lunt,big)
                   apnj0 = apn                                            ! Reset pointer to 1st slice
               else
                 apnj0 = apnj0 + lslice
               endif
C

C....          Once the current Tau step samples are reached then the velocity
C....          function for this step must be scaled so as to correctly merge
C....          this step with the previous.
C**** GMK      Taper has been changed so the velocity and rho taper to zero at j = taup ( so f = 1 there)
c**** GMK      It was changed because the tau steps were re-ordered from the migration process to forward modelling
C
               IF (j.le.taup .and. j.ge.tauc) THEN
                    scale = float(taup-j) / float(taup-tauc)

                    DO 194 i = 1, lslice
  194               apdata(ac+i-1) = vrms(i) * scale
C
C....               Butterworth taper Rho over the same interval.
                    f         = float(j-tauc) / float(taup-tauc)
                    rho1      = rho / (1. + (f/fcrho) ** (2.*nrho))
                    buffap(5) = rho1
                    buffap(6) = rho1 ** 2
                    buffap(7) = 2. * rho1
c
                    DO 195 i = 1, 3
  195               apdata(arho+i-1) = buffap(4+i)
               END IF

C
C....          Do the migration.
C....          Note that each AP-array is padded with a 0 at either end to avoid
C....          problems with the 3-point convolution in FDMCAR. Most of the
C....          calculations should not include the 0's, and the array addresses
C....          reflect this.
C....          FDMCAL does the following:
C....          1- Call FDMCAR to calculate the R terms for this Tau sample.
C....          2- Call FDMCAP to recursively calculate U, V and finally the Tau
C....             sample, P(n+1).
C....          3- Shuffle AP-buffers (move up in time for the next sample) and
C....             save the new P(n+1,j) time slice.
C
               CALL vsfdmcal (pc, apnj0+1, ppnj1, ppnj2, ppn1, ppn1j1,
     +              ppn1j2, ps3, lslic2)

C
C....          If all time slices in the AP are done then rewrite them to file
C....          FDMX.
             IF (j .EQ. jend) THEN
c                write(*,*) 'unload ap jstart = ', jstart, 'nslice= ', nslice
                 call putslice(jstart,nslice,nsamp, nx, apn,lunt,big)
             ENDIF
  200     CONTINUE
c
        if ( (DEBUG.ne.0) .and. (mod(tau,PRSTEP).eq.0) ) then
          print '(A,I3,2(A,F8.3),A,I4,A)',
     $     'Step: ',ntau-tau+1,' Tau: ',(nsamp-taup)*dt,
     $     ' - ',(nsamp-tauc)*dt,'s ', esamp, ' Samples'
          call timer
        endif
  100 CONTINUE
      RETURN
      END
