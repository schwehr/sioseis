      SUBROUTINE tx2tex( buf, lbuf, ibuf, scr, istop, nready )
c     tx2tex is the execution phase of SIOSEIS proces TX2TP or the
c  transformation from time-space domain to the tau-p space using the
c  slant stack technique developed by Mary Kappus = HOP.
c            HOP is the application of the paper "A new
c            method for slant stacking refraction data" by Henry,
c            Orcutt, and Parker (GRL, Dec 1980).  The problem of
c            slant-stacking seismic records at a number of ranges to
c            synthesize a tau-p curve is posed as a linear inverse 
c            problem for fixed frequency.  Using an inner product
c            weighted by (k^2 + b^2)*k (where k is wavenumber and b
c            some real positive number), then the representers are
c            bessel functions of k*range, scaled by 1/(k^2 + b^2),
c            and the model is U(k,w) (vert comp only).  The inverse
c            of the Gram matrix can be found analytically as sums
c            and differences of products of modified bessel functions
c            of b*range. If desired, the tau-p representers can be
c            used to compute the T-X predictions and the misfit of
c            these predictions to the original data.
c                        M.E. KAPPUS 10/86
c                        latest revision 7/88
c
c 
c            The program is dimensioned for a max of 200 input
c            seismograms (set in parameter statement mdist in routines
c            HOP,BESFL,GBINV,GGINV,MODEL,SPACE.
c     The following has changed and is no longer true  *************
c            Parameter nmax is (2*200*1026) for data and
c            coefficients + (3*nx) for Gram matrix + 1026 for model
c            + 513 for spectrum.  Allows 200 seismograms at 1024 points
c            each producing 200 tau-p grams of the same length.
c            Parameter ndist limits number of tau-p grams to 300.
c                                               
c                                               
c                             VARIABLES
c
c            tmin,tmax - start and end times to read in from input 
c                    traces
c            digit - digitization rate (# samples/sec) of input file -
c            tdpt - time delay per trace - difference in data start 
c                   times for adjacent traces in input data.  If input
c                   data all have same data start time tdpt = 0.
c                   The delay represents a constant reduction velocity,
c                   i.e. start time for each trace must be delayed
c                   by same amount relative to previous trace
c            pmin,pmax,np - first,last and number of p's at which to
c                        calculate models (locations of output traces)
c            b - scaling factor for representers
c            fc - cutoff frequency for calculating models
c            pcnti - % taper applied to input data before FFT
c            pcnto - % taper applied to models before inverse FFT
c            irev -  input t-x traces (1=are, 0=are not) in reverse 
c                    order (decreasing slowness)
c
c            CALCULATED -
c
c            ipntr(n),igpnt,iapnt,iwmod - pointers for starting loca-
c                      tions of transformed seismograms(ss), gram
c                      matrix(gi), coefficients(a), and model(wmod)
c                      in array s (in COMMON ssgia)
c            kf - point number for end freq at which to do calcs
c            limi,limo - number of points on which to apply taper,
c                        based on the % taper chosen by the user
c            lens - the amount of storage required for array s
c            lub2 - closest power of 2 greater than nt - used for FFT
c            mm - the power associated with lub2
c            nt - number of time points to be read in from each trace
c            nx - number of input traces
c            tapi,tapo - the tapers applied to time(freq) series
c
c          **calls subroutines SPACE,PREFORM,
c          **FFT2,REALTR,GGINV,BALPH,MODEL
c
c            NOTE 1.  times actually start at selected tmin (or taumin)
c                     + 1/digit - ie length of time does not include 
c                     start time and does include end time
c            NOTE 2.  values of range (or slowness) may be + or - , but 
c                     must all be of the same sign, and must be of
c                     increasing(irev=0) or decreasing(irev=1) order    
c              The SEGY trace header pointers:
c
c  mod 17 sep 94 - make percents percents by dividing by 100.
c  mod 3  Oct 94 - Change the memory allocation.  Store the data in the
c              transpose array (COMMON/transp/ss(nmax)) until all data
c              have been read and transformed.  Then use the larger ap
c              memory (COMMON /apmem/ s(1)).
c  mod 7 Oct 94 - change mdist = 200 to mdist = 300 to allow 300 input
c              traces. Should allow 300 output traces as well. Bumped
c              up nmax to reflect larger input size from 262144 to 393216
c              Updated tx2ted.f to reflect larger sizes, ***GMK     
c  mod 26 Dec 96 - Allow prestack
c  mod 11 May 07 - Do np preset (to intrcs when np = 0)
c                - make the headers file a segy file.
c  mod 29 May 07 - Use ABS(range) so increasing negative ranges works.
c                - Reset oldrange = 0 after all are done so prestack works
c
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, nsampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr
c
c             set the TX2TP COMMON blocks
c
      parameter (mdist = 300, ndist = 300, nmax = 262144)
      COMMON/bxs/b,ifx,ilx,nx,x(mdist)
      COMMON/ts/tmin,tmax,nt,tdpt 
      COMMON/ps/pmin,pmax,np,dp,p(ndist) 
      COMMON/digs/digit,fc,lub2,mm,df,kf,dw,ishift,limo
      COMMON/arbs/icomp,irev,ispec,imft
      COMMON/points/ipntr(mdist),igpnt,iapnt,imod(2*mdist)
      COMMON/transp/ss(nmax)
      COMMON /tx2tp/ sshift, sep(2), nnp, setau(2), bb, ffc, ppcnti, 
     *               ppcnto, iirev, fon, dummy, iimft, set(2), lprint,
     *               lunhdr, txprestk, ntx2tp
      INTEGER txprestk
      INTEGER fon
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, intrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *               iuseap, idecim, mdsize
c****  apmem is 5,000,000
      COMMON /apmem/ s(1)
      DIMENSION buf(111), lbuf(111), ibuf(111), scr(111)
      INTEGER*2 ibuf
      SAVE
      DATA oldrange/0./, eps/.0001/, nrps/0/
c            set variable types
c
      data da1,da2,pi/3.75,80.0,3.14159265/
      DATA nx/0/
c
c    assume that we only get those traces meant for us, so we don't need
c  a first trace - last trace
c    also assume that process plot does the plotting
c    calculate pointers for array s in COMMON data which holds the data,
c   its spectrum, Gram matrix, the coefficients, and models (SPACE)
c
      IF( istop .LT. 0 ) GOTO 1000                                      ! istop = -1 means there isn't a trace in buf!
      IF( ibuf(itridptr) .GT. 2 ) RETURN
      nx = nx + 1                                                       ! count the traces as they come in
      range = FLOAT(lbuf(ldisptr)) / 1000.                              ! the range (distance) in kilometers
      IF( range .LT. 0. .AND. range .LT. oldrange ) range = ABS(range)
      IF( range .EQ. 0. ) THEN
          range = .00001
          PRINT *,' range of 0. changed to .0001'
      ENDIF
      IF( range .EQ. oldrange ) THEN
          range = range + .00001
          PRINT *,' ***  WARNING  ***  duplicate range ',oldrange,
     *            ' becoming ',range
      ENDIF
      IF( range .LT. oldrange ) THEN                                    ! subroutine gginv requires increasing ranges
          PRINT *,' ***  ERROR  ***  tx2tp requires increasing ranges.'
          STOP
      ENDIF
      oldrange = range
      x(nx) = range
      nsamps = ibuf(nsampptr)
      delay = buf(ldelsptr)                                              ! the time of the first sample in seconds
      si = buf(lsisptr)                                                 ! the sample interval in seconds
      IF( tmin .LT. 0. ) tmin = delay
      IF( ntx2tp .EQ. 0 ) THEN
          itrno = 0
          tmin = set(1) 
          IF( tmin .LT. 0. ) tmin = delay                               ! if user didn't give stime, use the delay of the first trace 
          tmax = set(2)
          IF( tmax .LT. 0. ) tmax = delay + FLOAT(nsamps-1) * si
          digit = 1. / si                                               ! the sample rate = 1./(sample interval)
c OSU Begin July, 1990 Daniel Sattel
c OSU Deleted the next line.  tdpt gets reset later again.
c OLD        tdpt = sshift
c OSU End
          pmin = sep(1)                                                 ! first p value
          pmax = sep(2)                                                 ! last p value
          np = nnp                                                      ! the number of p's to do
          IF( np .EQ. 0 ) np = intrcs
          b = bb
          fc = ffc
          IF( fc .LE. 0 ) fc = digit / 2.
          pcnti = ppcnti / 100.
          pcnto = ppcnto / 100.
          irev = iirev
          imft = iimft
c      compute least power of 2 over seismogram length for the FFT program
          lub2 = 1
          itemp = (nsamps + 1) / 2
          DO 110  j = 1, 12
             mm = j
             lub2 = lub2 + lub2
             IF( lub2 .GE. itemp ) GOTO 120
  110     CONTINUE
          PRINT *,'trace too long - must be less than 2**12'
          STOP
  120     CONTINUE
          taumin = setau(1)
          taumax = setau(2)
          IF( taumin .GT. 99990. ) THEN
              taumin = tmin
              taumax = tmin + (lub2+lub2)*si
          ENDIF
          df = digit/FLOAT(lub2 + lub2)
          nt = lub2 + lub2
          kf = int(fc/df)
          dw = pi * digit/FLOAT(lub2)
          limi = NINT(pcnti * float(nsamps))
          limo = NINT(pcnto * float(kf))
          ishift = MOD(NINT((taumin-tmin)*(digit+eps)),2*lub2)
          IF (ishift .LT. 0) ishift = ishift + 2*lub2
          tdpt = 0.
          IF( IAND(lprint,2) .NE. 0 ) THEN
              PRINT *,'min t  ',tmin,' max t ',tmax,'  nt ',nt
              PRINT *,'1st p  ',pmin,' last p ',pmax,'  np ',np
              PRINT *,'min tau ',taumin,' max tau ',taumax,' dig ',digit
              PRINT *,'beta ',b,' %input taper',pcnti,' % output ',pcnto
              PRINT *,'seis len ',nsamps,' padded to ',2*lub2
              PRINT *,'cutoff freq',fc,' cutoff pnt',kf,' df=',df,
     &         ' ishift=', ishift
          ENDIF
          CALL podisc( ihunit, 1, 0 )
          CALL podisc( lunhdr, 1, 0 )
          CALL rddiscb( ihunit, scr, 3200, istat )
          CALL wrdiscb( lunhdr, scr, 3200 )
          CALL rddiscb( ihunit, scr, 400, istat )
          CALL wrdiscb( lunhdr, scr, 400 )
      ENDIF
      ndone = 0
      ntx2tp = ntx2tp + 1
c****
c****  make sure the first sample is at tmin and the last is at tmax
c****
      IF( tmin .LT. delay ) THEN
          n = (tmin-delay) / si
          DO 190 i = 1, n
  190     scr(i) = 0.                                                   ! zero fill from tmin to delay
          ndone = n
      ENDIF
      itemp = ibuf(58)
      ibuf(58) = 0
      CALL wrdisc( lunhdr, lbuf, numhdr )                               ! save the trace header on disk
      ibuf(58) = itemp
      iout = 0                                                          ! tell rlseap to move the data
      CALL rlseap( buf(numhdr+1), nsamps )                              ! get the data out of the ap or ap simulator
c
c     in loop through all traces read the data, taper and
c     pad (PREFORM), Fourier transform (FFT2), and unscramble
c     the real and imaginary parts (REALTR), account for phase 
c     shift if there is varying data start time (FAZE)
c     note:  nt data points are read in and padded
c     out to a power of 2 in PREFORM, but next trace is 
c     written over starting at 2*(kf+1) +1, which is how the
c     pointers were set up in SPACE
c        
      IF( tmin .GT. delay ) THEN
          istart = (delay - tmin) / si
      ELSE
          istart = 1
      ENDIF
      CALL preform( buf(numhdr+istart), limi, nsamps-istart+1, lub2 )
      DO 200 i = 1, nsamps
  200 scr(ndone+i) = buf(numhdr+istart+i-1)
      ndone = ndone + nsamps
      IF( ndone .LT. nt+2 ) THEN
          DO 210 i = 1, nt-ndone+2
  210     scr(ndone+i) = 0.
      ENDIF
      CALL FFT2( scr, mm, 0 )
      ipntr(nx) = (nx-1) * (kf+1) * 2 + 1
      iptr = ipntr(nx) - 1
      DO 220 i = 1, nt+2
  220 ss(iptr + i) = scr(i)
      CALL REALTR( ss(ipntr(nx)), ss(ipntr(nx)+1), lub2, 2 )
      IF(IAND(lprint,2) .NE. 0) PRINT *,' CALL REALTR. ipntr=',ipntr(nx)
c OSU Begin July, 1990 Daniel Sattel
c OSU Added the following line...
      if (nx.gt.1) tdpt=sshift
c OSU End
      IF ( TDPT .NE. 0. ) THEN
           CALL FAZE( nx, ss(ipntr(nx)))
           IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL FAZE. tdpt=',tdpt,
     &           ' ipntr=',ipntr(nx)
      ENDIF
      nready = 0                                                        ! no output traces ready yet!
      IF( txprestk .LE. 0 ) THEN
          IF( istop .EQ. 0 ) RETURN
      ELSE
          IF( lbuf(51) .LT. 0 ) nrps = nrps + 1
          IF( nrps .NE. txprestk ) RETURN
          nrps = 0
      ENDIF
c
c            test for enough room in arrays s,x, and p
c
 1000 CONTINUE
      IF( np .EQ. 0 ) np = nx
      dp = (pmax-pmin)/(np-1)
      DO 1100 i = 1, np
         p(i) = pmin + (i-1)*dp
 1100 CONTINUE
c

      CALL SPACE(nx,np,lens)
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL SPACE( ',nx,np,lens
      CALL inap(s,10000)                                                   ! get the ap array allocated
      DO 1101 i = 1, nmax
 1101 s(i) = ss(i)
      IF (NX.GT.MDIST.OR.NP.GT.NDIST.OR.LENS.GT.lapsiz) THEN
          PRINT *,'max storage exceeded - decrease nx or np.'
          PRINT *,' nx=',nx,' np=',np,' lens=',lens
          STOP
      ENDIF
c
c            test for size of arguments of Bessel function to 
c            determine which subroutine to use or if arguments are
c            too large to do at all
c
      CALL BESFL(ymin,ymax,ydif)
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,
     &   ' CALL BESFL, b=',b,' ymin=',ymin,' ymax=',ymax,' ydif=',ydif
c
c            use appropriate subroutine to compute Gram matrix inverse
c            and print it out
c
      IF (ymax.LT.da2) THEN
          ibr = 0
          CALL GGINV(s(igpnt))
          IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL GGINV( ',igpnt
      ELSE IF (ymax.GE.da2.AND.ymin.GT.da1.AND.ydif.LT.da2) THEN
          ibr = 1
          CALL GBINV(s(igpnt))
          IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL GBINV( ',igpnt
      ELSE 
          PRINT *,' ******  TX2TP error   ******'
          PRINT '(a,/,a,/,a)','incompatible range of Bessel fx arguments 
     &        b*x','if b*xmax>80, b*xmin must be >3.75 and b*(2*dx)',
     &        ' must be<80 - adjust ranges of x or b to conform'     
          stop
      ENDIF
c
c            compute the alpha vector of coefficients, putting the 
c            result in s, starting at pointer iapnt
c
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL BALPH(nx,kf',nx,kf,
     &      ipntr(1), igpnt, iapnt
      CALL BALPH(nx,kf,s(ipntr(1)),s(igpnt),s(iapnt))
c
      nready = np
      itrno = 0
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' p=',(p(i),i=1,5)
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' x=',(x(i),i=1,5)
      CALL podiscb( lunhdr, 1, 3600 )
      RETURN
c
      ENTRY getntp( buf, lbuf, ibuf )
c****
c****   return a trace, which is really a tau-p gram
c****
c
c             For each output p, build the model for each 
c             frequency, putting these (for fixed p) in s vector
c             starting at pointer imod (MODEL),
c             taper and pad the models (PREFORM),
c             unscramble the real and imaginary parts (REALTR), 
c             inverse Fourier transform (FFT2).
c
      itrno = itrno + 1                                                 ! increment the trace number
      CALL MODEL(itrno,s(iapnt),s(imod(1)))
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL MODEL( ',
     &    itrno,iapnt,imod(1)
      CALL PREFORM(s(imod(1)),limo,2*(kf+1),lub2)  
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL PERFORM( ',
     &    imod(1), limo,2*(kf+1),lub2
      CALL REALTR(s(imod(1)),s(imod(1)+1),lub2,-2)
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL REALTR( ',
     &     imod(1),imod(1)+1,lub2
      CALL FFT2(s(imod(1)),mm,1)
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL FFT2( ',imod(1),mm
c****
c****   create the output trace headers - make it up as we go
c****
      CALL rddisc( lunhdr, buf, numhdr, istat )
      IF( istat .NE. numhdr ) THEN
          CALL podisc( lunhdr, 2, -numhdr )
          CALL rddisc( lunhdr, buf, numhdr, istat )
          IF( istat .NE. numhdr ) PRINT *,
     * ' ***  WARNING  ***  tx2tp had a problem with the trace headers.'
      ENDIF
      IF( lbuf(7) .NE. 0 ) THEN
          lbuf(7) = itrno
      ELSE
          lbuf(4) = itrno
      ENDIF
      ibuf(itridptr) = 1                                                ! it's a live trace
      lbuf(ldisptr) = NINT( p(itrno)*1000.)                             ! put the p value in the range, but make it * 1000.
      itemp = NINT((taumax-taumin)/si)
      numdat = MIN( lub2+lub2, itemp )
      ibuf(nsampptr) = numdat
      ibuf(isiptr) = NINT( si*1000000. )                                ! the sample intervat in milliseconds
      buf(lsisptr) = si                                                 ! sample interval in seconds
      buf(ldelsptr) = taumin                                            ! the tau of the first sample
      ibuf(idelmptr) = NINT(taumin*1000.)
c****
c****   move the trace to buf.  The trick here is that the tau data might
c****  not have the same origin as the time data.  e.g.  If the data is
c****  4 mil, from 9 - 13 secs; after forward and inverse fft, the data
c****  goes from 9 to 13.096 (9+1024*.004). Because of "wraparound", the
c****  same data is for times 0.808 to 4.900, 4.904 to 8.996, 9.000 to 13.092
c****  13.096 to 17.192.
c****  variable ishift was calculated earlier - 
c****          ishift = MOD(NINT((taumin-tmin)*(dig+eps)),2*lub2)
c****
      index = numhdr
      jndex = imod(1) + ishift -1
      n = lub2 + lub2 - ishift
      DO 5200 i = 1, n
 5200 buf(index+i) = s(jndex+i)
      index = numhdr + n
      jndex = imod(1) - 1
      n = numdat - n
      IF( n .GT. 0 ) THEN
          DO 5210 i = 1, n
 5210     buf(index+i) = s(jndex+i)
      ENDIF
      lbuf(51) = 0
      IF( itrno .EQ. np ) THEN                                          ! if no more, reset the trace header
          itrno = 0
          CALL podisc( lunhdr, 1, 0 )
          nx = 0
          ntx2tp = 0
          oldrange = 0.
          IF( txprestk .NE. 0 ) lbuf(51) = -1
      ENDIF
c****
c****   set stuff for the SEGY binary tape header
c****
      in = 0                                                            ! the trace is not in the ap!
      intrcs = np                                                       ! each output record (shot) will contain np traces
      idtype = 7                                                        ! the data type is tau-p
      itxsi = NINT( 1./digit*1000000.)                                  ! the tx domain sample interval in mics
      itxdel = NINT( tmin*1000. )                                       ! the time of start of data in tx in mils
      oldrange = 0
      RETURN
      end
c*********************************************************************
      subroutine balph(nx,kf,ss,gi,a)
c*********************************************************************
c            builds the complex matrix of coefficients, alpha,stored 
c            in array s starting at iapnt.  here it is stored in 3-d
c            rep - 2(real+complex) by nx by kf+1 (number of freqs)
c            called a for alpha.  alpha = sum over x of gram matrix
c            element times fourier-transformed data 
c
c          **CALLS NO OTHER SUBROUTINES**
c
      dimension a(2,nx,kf+1), gi(3*nx-2), ss(2,kf+1,nx)
      do 630 j = 1,kf+1
c
c            compute first element (real+complex) outside of loop
c
      a(1,1,j) = gi(1)*ss(1,j,1) + gi(2)*ss(1,j,2)
      a(2,1,j) = gi(1)*ss(2,j,1) + gi(2)*ss(2,j,2)
      ig = 2
c
c            compute rest (except last) element in loop
c
         do 620 n = 2,nx-1
         a(1,n,j) = 0.0
         a(2,n,j) = 0.0
             do 610 m = 1,3
             im = n + m - 2
             ig = ig + 1
             a(1,n,j) = a(1,n,j) + gi(ig)*ss(1,j,im)
             a(2,n,j) = a(2,n,j) + gi(ig)*ss(2,j,im)
  610        continue
  620    continue
c
c            compute last element (real+complex) outside of loop
c
      a(1,nx,j) = gi(3*nx-3)*ss(1,j,nx-1) + gi(3*nx-2)*ss(1,j,nx)
      a(2,nx,j) = gi(3*nx-3)*ss(2,j,nx-1) + gi(3*nx-2)*ss(2,j,nx)
  630 continue
      return
      end
c*********************************************************************
      subroutine besfl(ymin,ymax,ydif)
c*********************************************************************
c            This subroutine tests for arguments (b*x) of the Bessel
c            functions which will be too large for the inverse Gram 
c            matrix subroutines to handle. Actually the difference
c            between two of these is required, and the failure 
c            conditions depend on this.  Subroutine GGINV can handle
c            computations when both arguments are <80.  Subroutine
c            GBINV can handle the computations if one argument is >80
c            as long as the other is >3.75, and the difference between
c            the two is <80.  This subroutine determines which 
c            Gram matrix subroutine to call.
c
c         ** CALLS NO OTHER SUBROUTINES**
c
      parameter (mdist = 300)
      COMMON/bxs/b,ifx,ilx,nx,x(mdist)
      ymin = b * x(1)
      ymax = b * x(nx)
      ydif = b * 2.0 * (x(nx) - x(1))/float(nx-1)
      return
      end      
c**********************************************************************
      subroutine faze(n,ss)
c**********************************************************************
c            This subroutine corrects for a phase shift if the input
c            data does not have a constant start time - tdpt is the 
c            delay in start time between adjacent traces
c
c          **CALLS NO OTHER SUBROUTINES**
c
      COMMON/ts/tmin,tmax,nt,tdpt
      COMMON/digs/digit,fc,lub2,mm,df,kf,dw,ishift,limo
      dimension ss(2,lub2+1)
      data pi/3.14159265/
      do 400 i = 1,lub2+1
c OSU Begin July, 1990 Daniel Sattel
c OSU Added the "2x"
         arg = (2*dw * (i-1)) * (tdpt * (n-1))
c OLD         arg = (dw * (i-1)) * (tdpt * (n-1))
c OSU End
         tre = cos(arg) * ss(1,i) - sin(arg) * ss(2,i)
         tim = sin(arg) * ss(1,i) + cos(arg) * ss(2,i)
         ss(1,i) = tre
         ss(2,i) = tim
  400 continue
      return
      end
c**********************************************************************
      subroutine gbinv(gi)                                        
c**********************************************************************
c            for the case of large arguments b*x
c            computes the inverse of the gram matrix gi directly.
c            as this is tridiagonal, it is stored as a 3*nx - 2 vector
c          **CALLS FUNCTION ABO**
c
      parameter (mdist = 300)
      COMMON/bxs/b,ifx,ilx,nx,x(mdist)
      dimension gi(3*nx-2),c(2),d(2),b1(mdist),b2(mdist),b3(mdist),
     &          b4(mdist)
c
c            compute products of modified Bessel functions
c
      do 500 n = 2,nx-1
      b1(n) = ABO(b*x(n),b*x(n+1))
      b2(n) = ABO(b*x(n+1),b*x(n))
      b3(n) = ABO(b*x(n+1),b*x(n-1))
      b4(n) = ABO(b*x(n-1),b*x(n+1))
  500 continue
c
c            use AIO type routine to compute factors for term g1
c            and AKO type routine to compute factors for term gn
c
      do 510 i = 1,2
      t = 3.75/(b*x(i))
      d(i)=.39894228+t*(.01328592+t*(.00225319-t*(.00157565-t*(.00916281
     &     -t*(.02057706-t*(.02635537-t*(.01647633-t*.00392377)))))))
      s = 2.0/(b*x(nx-2+i))
      c(i) = 1.25331414-s*(.07832358-s*(.02189568-s*(.01062446-
     &       s*(.00587872-s*(.00251540-s*.00053208))))) 
  510 continue
c
c            compute ratios of 1st two bessel fxs of first kind and
c            last two of 2nd kind.  used to scale first and last
c            matrix elements.
c
      g1 = -(d(2)/d(1)) * exp(b*(x(2)-x(1))) * sqrt(x(1)/x(2))
      gn = -(c(1)/c(2)) * exp(b*(x(nx)-x(nx-1))) * sqrt(x(nx)/x(nx-1))
c
c            compute elements of inverse matrix, using tridiagonality
c
      gi(2) = 1.0/(ABO(b*x(2),b*x(1)) - ABO(b*x(1),b*x(2)))
      gi(1) = g1 * gi(2)
c
      do 520 i = 2,nx-1
      kp = (3*i) - 3
      kd = (3*i) - 2
      km = (3*i) - 1
      gi(kp) = gi(km-3)
      gi(km) = 1.0/(b2(i) - b1(i))
      gid = b4(i) - b3(i)
      gi(kd) = gi(km) * gi(kp) * gid
  520 continue
c
      gi(3*nx-3) = gi(3*nx-4)
      gi(3*nx-2) = gn * gi(3*nx-3)
      return
      end
c**********************************************************************
      subroutine gginv(gi)                                        
c**********************************************************************
c            computes the inverse of the gram matrix gi directly.
c            as this is tridiagonal, it is stored as a 3*nx - 2 vector
c
c          **CALLS FUNCTIONS AIO AND AKO**
c
      parameter (mdist = 300)
      COMMON/bxs/b,ifx,ilx,nx,x(mdist)
      dimension bi(mdist),bk(mdist),gi(3*nx-2)
c
c            compute modified bessel fxs(beta*x)
c
      do 500 n = 1,nx
      bi(n) = AIO(b*x(n))
      bk(n) = AKO(b*x(n))
  500 continue
c
c            compute ratios of 1st two bessel fxs of first kind and
c            last two of 2nd kind.  used to scale first and last
c            matrix elements.
c
      g1 = (-bi(2)/bi(1))
      gn = (-bk(nx-1)/bk(nx))
c
c            compute elements of inverse matrix, using tridiagonality
c
      gi(2) = 1.0/((bi(1)*bk(2)) - (bi(2)*bk(1)))
      gi(1) = g1 * gi(2)
c
      do 520 i = 2,nx-1
      kp = (3*i) - 3
      kd = (3*i) - 2
      km = (3*i) - 1
      gi(kp) = gi(km-3)
      gi(km) = 1.0/((bi(i)*bk(i+1)) - (bi(i+1)*bk(i)))
      gid = ((bi(i+1)*bk(i-1)) - (bi(i-1)*bk(i+1)))
      gi(kd) = gi(km) * gi(kp) * gid
  520 continue
c
      gi(3*nx-3) = gi(3*nx-4)
      gi(3*nx-2) = gn * gi(3*nx-3)
      return
      end
c*****************************************************************
      subroutine model(i,a,wmod)                     
c*****************************************************************
c            forms the representer = Jo(kx)/(k**2 + b**2) and the
c            model = sum of representer times coefficient (alpha)
c            for each freq separately for fixed p.  result is a vector
c            of models, one for each freq.
c            the fourier transform of the models gives tau-p.
c
c          **CALLS FUNCTION AJO**
c                      
      parameter (mdist = 300, ndist = 300)    
      COMMON/bxs/b,ifx,ilx,nx,x(mdist)
      COMMON/ps/pmin,pmax,np,dp,p(ndist)
      COMMON/digs/digit,fc,lub2,mm,df,kf,dw,ishift,limo
      dimension wmod(2,lub2+1),a(2,nx,kf+1)
c             
      do 860 j = 1,kf+1      
      wmod(1,j) = 0.0
      wmod(2,j) = 0.0
      w = float(j-1) * dw
      wp2b2 = (w*p(i))**2 + b**2
c
c            form the nth representer, rep.  multiply by alpha and sum
c            to form the model (for fixed freq), wmod.
c
          do 820 n = 1,nx
          bf = AJO(x(n)*w*p(i))      
          rep = bf/wp2b2
          wmod(1,j) = wmod(1,j) + a(1,n,j)*rep
          wmod(2,j) = wmod(2,j) + a(2,n,j)*rep
  820     continue                        
  860 continue
      return
      end
c*********************************************************************
      subroutine preform(ss,lim,nn,lub2)
c*********************************************************************
c
c           tapers and pads data before Fourier transform
c
c         **CALLS NO OTHER SUBROUTINES**
c          
      dimension ss(2*(lub2+1))                         
      data pi/3.14159265/
c
c           taper with cos**2 taper, amount specified by user
c
      IF (LIM.GT.0) THEN
          do 310 j = 1,lim
             tap = 0.5 * (1.0 - cos((j-1.)*pi/float(lim)))
             ss(j) = ss(j) * tap
             ss(nn-j+1) = ss(nn-j+1) * tap
  310     continue
      ENDIF
c
c           pad end with z1eroes to power of two
c
      IF (NN.LT.2*LUB2) THEN
          do 320 j = nn+1,2*(lub2+1)
             ss(j) = 0.0
  320     continue
      ENDIF
      return
      end
c******************************************************************
      subroutine space(nx,np,lens)
c******************************************************************
c           computes the values of the pointers for array s in COMMON
c           data.  IPNTR locates the beginning of each data trace - 
c           one for each p.  only 2*(kf+1) points are allowed for each
c           trace - accounting for the real and imaginary part of the
c           Fourier-transformed data up to some cutoff point kf assoc-
c           iated with user-selected cutoff frequency fc. one extra
c           'trace' is allowed in order to store the spectrum of the
c           data (it is later overwritten by the spectrum of the model)
c           IGPNT locates the beginning of the inverse Gram matrix
c           IAPNT locates the beginning of the vector of coefficients.
c           IMOD locates the beginning of each model
c           trace - one for each p. 2*(lub2+1) points are allowed.
c
c      **CALLS NO OTHER SUBROUTINES**
c
c            set COMMONs and variable types
c                                                       
      parameter (mdist = 300)
      COMMON/digs/digit,fc,lub2,mm,df,kf,dw,ishift,limo
      COMMON/points/ipntr(mdist),igpnt,iapnt,imod(2*mdist)
c
c            compute pointers
c
      igpnt = ipntr(nx) + lub2+1
      iapnt = igpnt + 3*nx-2
      imod(1) = iapnt + ((kf+1)*2*nx)
c
c             compute overall length
c
      lens = imod(1) + 2*(lub2+1)
      return
      end
c******************************************************************
      FUNCTION AJO(X)                                              
c*********************************************************************
c$$$$ CALLS NO OTHER ROUTINES
C  BESSEL FUNCTION OF THE 1ST KIND, 0TH ORDER, REAL ARGUMENT.
C  POLYNOMIAL APPROXIMATIONS FROM ABRAMOWITZ+STEGUN PP369-370.
      Y=ABS(X)
      IF (Y.GT. 3.0) GO TO 2000
      T=(X/3.0)**2
      AJO=1.0-T*(2.2499997-T*(1.2656208-T*(.3163866-T*(.0444479-T*
     +  (.0039444-T*.00021)))))
      RETURN
 2000 T=3.0/Y
      F=.79788456+T*(-.00000077+T*(-.0055274+T*(-.00009512+T*
     +  (.00137237+ T*(-.00072805+T*.00014476)))))
      A=.78539816-T*(-.04166397+T*(-.00003954+T*(.00262573+T*
     +  (-.00054125 +T*(-.00029333+T*.00013558)))))
      AJO=F*COS(Y-A)/SQRT(Y)
      RETURN
      END                                                               AJO
c*****************************************************************
      FUNCTION AIO(X)                                             
c*****************************************************************
C$$$$$ CALLS NO OTHER ROUTINES
C  MODIFIED BESSEL FUNCTION OF THE 1ST KIND, 0TH ORDER, REAL ARGUMENT
C  POLYNOMIAL APPROXIMATIONS FROM ABRAMOWITZ+STEGUN, P378.
      Y=ABS(X)
      IF (Y.GT. 3.75) GO TO 2000
      T=(X/3.75)**2
      AIO=1.0+T*(3.5156229+T*(3.0899424+T*(1.2067492
     +  +T*(.2659732+T*(.0360768+T*.0045813)))))
      RETURN
 2000 T=3.75/Y
      A=.39894228+T*(.01328592+T*(.00225319-T*(.00157565-T*(.00916281
     +  -T*(.02057706-T*(.02635537-T*(.01647633-T*.00392377)))))))
      AIO=A*EXP(Y)/SQRT(Y)
      RETURN
      END                                                               AIO
c                                                                 
c*****************************************************************
      FUNCTION AKO(X)                                             
c*****************************************************************
C$$$$ CALLS NO OTHER ROUTINES
C COMPUTES MODIFIED BESSESL FUNCTIONS OF THE 2ND KIND (K), REAL ARGUMENT
C  WITH TRUNCATED EXPRESSIONS OF ABRAMOWITZ + STEGUN PP378- 9.
      IF (X.GT. 2.0) GO TO 2000
      T=X/3.75
      T=T*T
      AIO=1.0+T*(3.5156229+T*(3.0899424+T*(1.2067492+T*(.2659732+
     +  T*(.0360768+T*.0045813)))))
      Y=.25*X*X
      AKO=-AIO*ALOG(.5*X)-.57721566+Y*(.42278420+Y*(.23069756+
     +  Y*(.03488590+Y*(.00262698+Y*(.00010750+Y*.00000740)))))
      RETURN
 2000 Y=2.0/X
      T=1.25331414-Y*(.07832358-Y*(.02189568-Y*(.01062446-
     +  Y*(.00587872-Y*(.00251540-Y*.00053208)))))
      AKO=T*EXP(-X)/SQRT(X)
      RETURN
      END                                                               AKO
c*******************************************************************
      FUNCTION ABO(x,y)
c*******************************************************************
C        ***CALLS NO OTHER ROUTINES***
C        COMPUTES PRODUCT OF MODIFIED BESSEL FUNCTION OF 1ST KIND (AIO) 
C        ARGUMENT Y WITH MODIFIED BESSEL FUNCTION OF 2ND KIND (AKO)
C        ARGUMENT X WHEN BOTH ARGUMENTS ARE GREATER THAN 3.75
C        IS JUST COMBINATION OF AIO AND AKO ABOVE
C
C        preliminary part of AKO(x)
      s = 2.0/x
      c = 1.25331414-s*(.07832358-s*(.02189568-s*(.01062446-
     +  s*(.00587872-s*(.00251540-s*.00053208))))) 
C        preliminary part of AIO(y)
      t = 3.75/y
      d = .39894228+t*(.01328592+t*(.00225319-t*(.00157565-t*(.00916281
     +  -t*(.02057706-t*(.02635537-t*(.01647633-t*.00392377)))))))
C        combine fintal expression
      ABO = c*d*exp(y-x)/sqrt(x*y)
      return
      end

