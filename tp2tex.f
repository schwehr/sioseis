      SUBROUTINE tp2tex( buf, lbuf, ibuf, scr, lscr, iscr,  
     &                   istop, nready )
c     tp2tex is the execution phase of SIOSEIS process TP2TX or the
c  transformation from tau-p space to the time-space domain using the
c  slant stack technique developed by Mary Kappus = HOP/REHOP.  SEE
c  subroutine tx2tex for additional prose.
c            Parameter nmax is (2*200*1026) for data and
c            coefficients + (3*nx) for Gram matrix + 1026 for model
c            + 513 for spectrum.  Allows 200 seismograms at 1024 points
c            each producing 200 tau-p grams of the same length.
c                                               
c                                               
c                             VARIABLES
c
c            tmin,tmax - start and end times to read in from input 
c                    traces
c            digit - digitization rate (# samples/sec) of input file -
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
c  mod May 07 - Require the HEADERS file and get the X from it, rather
c               than uxing xmin, xmax, and making X uniform increments.
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
      parameter (mdist = 350, nmax = 3500000 )
      common/bps/b,ifp,ilp,np,p(mdist)
      COMMON/bxs/bbxs,ifx,ilx,nxbxs
      common/xs/xmin,xmax,ntp2tx,dx,x(2*mdist)
      COMMON/ts/tmin,tmax,nt,tdpt 
      COMMON/digs/digit,fc,lub2,mm,df,kf,dw,ishift
      COMMON/arbs/icomp,irev,ispec,imft
      COMMON/points/ipntr(mdist),igpnt,iapnt,imod(2*mdist)
      COMMON/apmem/s(nmax)
      COMMON /tp2tx/ sshift, sex(2), nnx, setau(2), bb, ffc, ppcnti, 
     *               ppcnto, iirev, fon, dummy, iimft, set(2), lprint,
     &               tpprestk
c****  we need tx2tp common because of lunhdr - see tx2ted for some notes
      COMMON /tx2tp/ sshift1, sep1(2), nnp1, setau1(2), beta1, ffc1, 
     *               ppcnti1, ppcnto1, iirev1, ffon1, dummy1, iimft1, 
     *               set1(2), lprt1, lunhdr, txprestk
      INTEGER fon, tpprestk
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, intrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *               iuseap, idecim
c              The SEGY trace header pointers:
      INTEGER*2 ibuf(111), iscr(111)
      DIMENSION buf(111), lbuf(111), scr(111), lscr(111)
      SAVE
      DATA itrno/0/
c            set variable types
c
      data da1,da2,pi/3.75,80.0,3.14159265/
      DATA np/0/
c
c    assume that we only get those traces meant for us, so we don't need
c  a first trace - last trace
c    also assume that process plot does the plotting
c    calculate pointers for array s in COMMON data which holds the data,
c   its spectrum, Gram matrix, the coefficients, and models (SPACE)
c
      IF( istop .LT. 0 ) GOTO 1000                                      ! istop = -1 means there isn't a trace in buf!
      np = np + 1                                                       ! count the traces as they come in
      p(np) = FLOAT(lbuf(ldisptr)) / 1000.
      ntau = ibuf(nsampptr)                                             ! the number of taus
      delay = buf(ldelsptr)                                             ! the time (tau) of the first sample in seconds
      si = buf(lsisptr)                                                 ! the sample interval in seconds
      IF( ntp2tx .EQ. 0 ) THEN
          ntp2tx = ntp2tx + 1
          IF( set(1) .NE. -1. ) tmin = set(1) 
          IF( tmin .LT. 0. ) tmin = itxdel / 1000.                      ! if user didn't give stime, use the delay of the first trace 
          IF( set(2) .NE. -1. ) tmax = set(2)
          IF( tmax .LT. 0. ) tmax = tmin + FLOAT(ntau-1) * si
          digit = 1. / si                                               ! the sample rate = 1./(sample interval)
          IF( sex(1) .NE. -1. ) xmin = sex(1)                           ! first x value
          IF( sex(2) .NE. -1. ) xmax = sex(2)                           ! last x value
          nx = nxbxs
          b = bb
          fc = ffc
          IF( fc .LE. 0 ) THEN
              IF( ffc1 .NE. 0 ) THEN
                  fc = ffc1
              ELSE
                  fc = digit / 2.
              ENDIF
          ENDIF
          pcnti = ppcnti
          pcnto = ppcnto
          irev = iirev
          imft = iimft
c             compute least power of 2 over seismogram length for
c             the FFT program
          lub2 = ntau / 2
          itemp = 1
          DO 110  j = 1, 12
             mm = j
             itemp = itemp + itemp
             IF( lub2 .LE. itemp ) GOTO 120
  110     CONTINUE
          PRINT *,'trace too long - must be less than 2**12'
          STOP
  120     CONTINUE
          lub2 = itemp                                                  ! this ensures that lub2 is a power of 2!
          IF( idtype .NE. 7 ) THEN
              PRINT *,' ***  ERROR  *** The input to TP2TX must be in',
     *                ' tau-p - i.e. run thru process TP2TX.'
              STOP
          ENDIF
          IF( setau(1) .EQ. 99999. ) THEN
              setau(1) = setau1(1)
              setau(2) = setau1(2)
          ENDIF
          taumin = setau(1)
          taumax = setau(2)
          IF( taumin .GT. 99990. ) THEN
              taumin = delay
              taumax = taumin + (lub2+lub2)*si
          ENDIF
          df = digit/FLOAT(lub2 + lub2)
          nt = lub2 + lub2
          kf = int(fc/df)
          dw = pi * digit/FLOAT(lub2)
          limi = NINT(pcnti * float(ntau))
          limo = NINT(pcnto * float(kf))
          ishift = MOD(NINT((tmin-taumin)/si),2*lub2)
          IF (ishift .LT. 0) ishift = ishift + 2*lub2
c****     if the user didn't give us a file with the SEGY trace headers,
c****     we will save the incoming trace headers for later, so get the
c****     trace header unit opened.  
c          IF( lunhdr .EQ. 0 ) THEN
c              CALL getfil( 1, luntemp, token, istat )                   ! so nonzero luntemp means save the incoming headers
c          ELSE
c              luntemp = 0                                               ! zero means use the user's header file
c          ENDIF
      ENDIF
c*********************
c   not used:    pmin, pmax
c*********************
c****
c****
      ndone = 0
      IF( taumin .LT. delay ) THEN
          n = (taumin-delay) / si
          DO 190 i = 1, n
  190     scr(i) = 0.                                                   ! zero fill from tmin to delay
          ndone = n
      ENDIF
c      IF( luntemp .NE. 0 ) CALL wrdisc( luntemp, lbuf, numhdr )         ! save the trace header on disk
      iout = 0                                                          ! tell rlseap to move the data
      CALL rlseap( buf(numhdr+1), ntau )                                ! get the data out of the ap or ap simulator
      IF( taumin .GT. delay ) THEN
          istart = (delay - taumin) / si
      ELSE
          istart = 1
      ENDIF
      CALL preform2( buf(numhdr+istart), limi, ntau-istart+1, lub2 )
      DO 200 i = 1, ntau
  200 scr(ndone+i) = buf(numhdr+istart+i-1)
      ndone = ndone + ntau
      IF( ndone .LT. nt+2 ) THEN
          DO 210 i = 1, nt-ndone+2
  210     scr(ndone+i) = 0.
      ENDIF
      CALL FFT2( scr, mm, 0 )
      ipntr(np) = (np-1) * (kf+1) * 2 + 1
      iptr = ipntr(np) - 1
      DO 220 i = 1, lub2+lub2+2
  220 s(iptr + i) = scr(i)
      CALL REALTR( s(ipntr(np)), s(ipntr(np)+1), lub2, 2 )
      nready = 0                                                        ! no output traces ready yet!
      IF( tpprestk .LE. 0 ) THEN
          IF( istop .EQ. 0 ) RETURN
      ELSE
          IF( lbuf(51) .NE. -1 ) RETURN
      ENDIF
c
c            test for enough room in arrays s,x, and p
c                (from Mary's userd routine)
c
 1000 CONTINUE
      IF( nnx .NE. 0 ) THEN
          nx = nnx
          dx = (xmax-xmin)/(nx-1)
          DO i = 1, nx
             x(i) = xmin + (i-1)*dx
          ENDDO
      ELSE
          CALL podiscb( lunhdr, 1, 3600 )                                   ! rewind the header disk file
          nx = 0
          DO i = 1, 100000
             CALL rddiscb( lunhdr, scr, 240, istat )
             IF( istat .NE. 240 ) GOTO 1100
             nx = i
             x(i) = FLOAT(lscr(10)) / 1000.
          ENDDO
      ENDIF
 1100 CONTINUE
	 IF( IAND(lprint,2) .NE. 0 ) PRINT *,' nx=',nx,' np=',np,
     &' ntau=',ntau
      CALL podisc( lunhdr, 1, 3600 )                                    ! rewind the header disk file
c      xmin = 9999999.
c      xmax = 0.
c      DO i = 1, nx
c         IF( x(i) .LT. xmin ) xmin = x(i)
c         IF( x(i) .GT. xmax ) xmax = x(i)
c      ENDDO
c	 IF( IAND(lprint,2) .NE. 0 ) PRINT *,' nx=',nx,' xmin=',xmin,
c     &    ' xmax=',xmax
c      space2 computes indeces igpnt, iapnt, imod(nx) of the s array
c       index ipntr is recomputed 
      CALL SPACE2(np,nx,lens)
      IF( np .GT. mdist ) THEN
          PRINT *,' TP2TX - Too many p requested.  Max is ',mdist
          PRINT *,' Please report this to phenkart@ucsd.edu'
          STOP
      ENDIF
      IF( nx .GT. 2*mdist ) THEN
          PRINT *,' TP2TX - Too many x requested.  Max is ',2*mdist
          PRINT *,' Please report this to phenkart@ucsd.edu'
          STOP
      ENDIF
      IF( lens .GT. nmax ) THEN
          PRINT *,' TP2TX - Too much memory requested.'
          PRINT *,' Please report this to phenkart@ucsd.edu'
          STOP
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' Ps ',(p(i),i=1,np)
          PRINT *,' Xs ',(x(i),i=1,nx)
      ENDIF
      nready = nx
      itrno = 0
      RETURN
c
      ENTRY gettptx( buf, lbuf, ibuf )
c****
c****   return a tx trace
c****
      IF( itrno .EQ. 0 ) THEN                                           ! first timer?
          CALL podiscb( lunhdr, 1, 3600 )
c
c            test for size of arguments of Bessel function to 
c            determine which subroutine to use or if arguments are
c            too large to do at all
          CALL BESFL2(j2)
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL BESFL2( ',j2
c
c            before start loop in frequency, calculate the model for
c            zero frequency separately. this requires a different
c            algorithm for the Gram matrix computations
c
          CALL GOINV2(s(igpnt))
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL GOINV2 ',igpnt
          CALL BALPH2(1,np,kf,s(ipntr(1)),s(igpnt),s(iapnt))
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL BALPH2( ',
     &        1,np,kf,ipntr(1),igpnt,iapnt
          w = 0.
          IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL MODEL2 - ',nx
          DO 1225 i = 1,nx
             CALL MODEL2(i,1,w,s(iapnt),s(imod(1)))
 1225     CONTINUE
c         start BIG LOOP(s) in frequency to compute models using GGINV
c         until Bessel function arguments exceed da2, then using GBINV
          j2 = MIN(j2,kf)
          DO 1250 j = 2, j2
             w = (j-1) * dw
c            compute the inverse Gram matrix for the fixed frequency
c            then compute the vector of coefficients alpha for fixed 
c            frequency, then loop in x to build the model
             CALL GGINV2( s(igpnt), w )
             CALL BALPH2( j, np, kf, s(ipntr(1)), s(igpnt), s(iapnt) )
             DO 1230 i = 1, nx
                CALL MODEL2( i, j, w, s(iapnt), s(imod(1)) )
 1230        CONTINUE
 1250     CONTINUE
          IF (KF.GT.J2) THEN
             DO 1260 j = j2+1, kf
                w = (j-1) * dw
                CALL GBINV2( s(igpnt), w )
                CALL BALPH2( j, np, kf, s(ipntr(1)), s(igpnt), s(iapnt))
                DO 1255 i = 1,nx
                   CALL MODEL2( i, j, w, s(iapnt), s(imod(1)) )
 1255           CONTINUE
 1260        CONTINUE
          ENDIF
      ENDIF
      itrno = itrno + 1                                                 ! increment the trace number
      CALL PREFORM2( s(imod(itrno)), limo, 2*(kf+1), lub2 )  
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL PREFORM2 ',
     &     imod(itrno), limo, 2*(kf+1), lub2
      CALL REALTR( s(imod(itrno)), s(imod(itrno)+1), lub2, -2 )
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL REALTR( ',
     &     imod(itrno), imod(itrno)+1, lub2, -2
      CALL FFT2( s(imod(itrno)), mm,  1)
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,' CALL FFT2( ',
     &    imod(itrno), mm,  1
c****
c****   create the output trace headers
c****
      CALL rddisc( lunhdr, buf, numhdr, istat )
      IF( istat .NE. numhdr ) THEN
c****    this can happen in nx was give and is bigger than the original.
          CALL podisc( lunhdr, 2, -numhdr )
          CALL rddisc( lunhdr, buf, numhdr, istat )
          IF( istat .NE. numhdr ) PRINT *,
     * ' ***  WARNING  ***  tx2tp had a problem with the trace headers.'
      ENDIF
      IF( nnx .NE. 0 ) THEN
          IF( lbuf(7) .NE. 0 ) THEN
              lbuf(7) = itrno
              lbuf(51) = 0
          ELSE
              lbuf(4) = itrno
          ENDIF
      ENDIF
      IF( itrno .EQ. nx ) THEN
          ntp2tx = 0
          np = 0
      ENDIF
      ibuf(itridptr) = 1                                                ! it's a live trace
      lbuf(ldisptr) = NINT( x(itrno) * 1000. )                          ! put the range in
      itemp = (tmax-tmin)/si                                            ! the Cray doesn't like this in the MIN0 function!
      numdat = MIN(ntau,itemp+1)
      ibuf(nsampptr) = numdat
      ibuf(isiptr) = NINT( si*1000000. )                                ! the sample intervat in milliseconds
      buf(lsisptr) = si                                                 ! sample interval in seconds
      ibuf(idelmptr) = tmin * 1000.                                     ! the delay in mils
      buf(ldelsptr) = tmin                                              ! the delay in seconds
c****
c****   move the trace to buf.
c****
      index = numhdr
      jndex = imod(itrno) - 1 + ishift
      n = lub2 + lub2 - ishift
      DO 5200 i = 1, n
 5200 buf(index+i) = s(jndex+i)
      index = numhdr + n
      jndex = imod(itrno) - 1
      n = numdat - n
      IF( n .GT. 0 ) THEN
          DO 5210 i = 1, n
 5210     buf(index+i) = s(jndex+i)
      ENDIF
c****
c****   set stuff for the SEGY binary tape header
c****
      in = 0                                                            ! the trace is not in the ap!
      intrcs = nx                                                       ! each output record (shot) will contain nx traces
      idtype = 1                                                        ! the data type is time
      itxsi = si
      itxdel = 0.
      RETURN
      END
c*********************************************************************
      subroutine balph2(j,np,kf,ss,gi,a)
c*********************************************************************
c            builds the complex matrix of coefficients, alpha,stored 
c            in array s starting at iapnt.  here it is stored in 2-d
c            rep - 2(real+complex) by np 
c            called a for alpha.  alpha = sum over p of gram matrix
c            element times fourier-transformed data at frequency w.  
c
c          **CALLS NO OTHER SUBROUTINES**
c
      dimension a(2,np), gi(3*np-2), ss(2,kf+1,np)
c
c            compute first element (real+complex) outside of loop
c
      a(1,1) = gi(1)*ss(1,j,1) + gi(2)*ss(1,j,2)
      a(2,1) = gi(1)*ss(2,j,1) + gi(2)*ss(2,j,2)
      ig = 2
c
c            compute rest (except last) element in loop
c
         do 620 n = 2,np-1
         a(1,n) = 0.0
         a(2,n) = 0.0
             do 610 m = 1,3
             im = n + m - 2
             ig = ig + 1
             a(1,n) = a(1,n) + gi(ig)*ss(1,j,im)
             a(2,n) = a(2,n) + gi(ig)*ss(2,j,im)
  610        continue
  620    continue
c
c            compute last element (real+complex) outside of loop
c
      a(1,np) = gi(3*np-3)*ss(1,j,np-1) + gi(3*np-2)*ss(1,j,np)
      a(2,np) = gi(3*np-3)*ss(2,j,np-1) + gi(3*np-2)*ss(2,j,np)
      return
      end
c*********************************************************************
      subroutine besfl2(j2)
c*********************************************************************
c            This subroutine tests for arguments of the Bessel
c            functions which will be too large for later subroutines 
c            to handle.  The argument is b*w*p which is evaluated in
c            computation of the Gram matrix.  Actually the difference
c            between two of these is required, and the failure 
c            conditions depend on this.  Subroutine GGINV can handle
c            computations when both arguments are <87.  Subroutine
c            GBINV can handle the computations if one argument is >87
c            as long as the other is >3.75, and the difference between
c            the two is <87.  THis subroutne gives the frequency at
c            which neither subroutine can work, and allows the user to
c            choose whether or not to continue with a new cutoff freq.
c
c         ** CALLS NO OTHER SUBROUTINES**
c
      parameter (mdist = 350)
      common/digs/digit,fc,lub2,mm,df,kf,dw,ishift            
      common/bps/b,ifp,ilp,np,p(mdist)
      character*50 reason,cure
      data da1,da2,pi/3.75,80.0,3.14159265/
c
c            compute point corresponding to frequency where value of
c            Bessel function argument crosses over key values da1,da2
c
      dp = (p(np) - p(1))/(np-1)
      j1 = int(da1/(dw*b*p(1)))
      j2 = int(da2/(dw*b*p(np)))
      j3 = int(da2/(dw*b*2*dp))
c
c            tests for where failures occur
c
      IF (j2.LT.j1.and.j2.LT.kf) THEN
          jj = j2
          reason = 'because arg2 > 80 when arg1 < 3.75                '
          cure = 'possible fix is to decrease b or range of p-values'
          GO TO 50
      ENDIF
      IF (j3.LT.kf) THEN
          jj = j3
          reason = 'because diff btwn 2 args > 80                     '
          PRINT *,' j3=',j3,' kf=',kf
          cure = 'possible fix is to decrease dp or b              '
          GO TO 50
      ENDIF
      return
   50 print *,' ****   TP2TX error **** Bessel fx failure at j = ',
     &     jj,' which is freq ', (jj*dw)/(2.*pi),' hz'
      print '(a)', reason                              
      print '(a)',cure
      STOP
      end      
c**********************************************************************
      subroutine gbinv2(gi,w)                                        
c**********************************************************************
c            for the case of large arguments w*p**b, 
c            computes the inverse of the gram matrix gi directly.
c            as this is tridiagonal, it is stored as a 3*np - 2 vector
c          **CALLS FUNCTION ABO**
c
      parameter (mdist = 350)
      common/bps/b,ifp,ilp,np,p(mdist)
      dimension gi(3*np-2),c(2),d(2),b1(mdist),b2(mdist),b3(mdist),
     &          b4(mdist)
c
c            compute products of modified Bessel functions
c
      do 500 n = 2,np-1
      b1(n) = ABO(w*b*p(n),w*b*p(n+1))
      b2(n) = ABO(w*b*p(n+1),w*b*p(n))
      b3(n) = ABO(w*b*p(n+1),w*b*p(n-1))
      b4(n) = ABO(w*b*p(n-1),w*b*p(n+1))
  500 continue
c
c            use AIO type routine to compute factors for term g1
c            and AKO type routine to compute factors for term gn
c
      do 510 i = 1,2
      t = 3.75/(w*b*p(i))
      d(i)=.39894228+t*(.01328592+t*(.00225319-t*(.00157565-t*(.00916281
     &     -t*(.02057706-t*(.02635537-t*(.01647633-t*.00392377)))))))
      s = 2.0/(w*b*p(np-2+i))
      c(i) = 1.25331414-s*(.07832358-s*(.02189568-s*(.01062446-
     &       s*(.00587872-s*(.00251540-s*.00053208))))) 
  510 continue
c
c            compute ratios of 1st two bessel fxs of first kind and
c            last two of 2nd kind.  used to scale first and last
c            matrix elements.
c
      g1 = -(d(2)/d(1)) * exp(w*b*(p(2)-p(1))) * sqrt(p(1)/p(2))
      gn = -(c(1)/c(2)) * exp(w*b*(p(np)-p(np-1))) * sqrt(p(np)/p(np-1))
c
c            compute elements of inverse matrix, using tridiagonality
c
      gi(2) = 1.0/(ABO(w*b*p(2),w*b*p(1)) - ABO(w*b*p(1),w*b*p(2)))
      gi(1) = g1 * gi(2)
c
      do 520 i = 2,np-1
      kp = (3*i) - 3
      kd = (3*i) - 2
      km = (3*i) - 1
      gi(kp) = gi(km-3)
      gi(km) = 1.0/(b2(i) - b1(i))
      gid = b4(i) - b3(i)
      gi(kd) = gi(km) * gi(kp) * gid
  520 continue
c
      gi(3*np-3) = gi(3*np-4)
      gi(3*np-2) = gn * gi(3*np-3)
      return
      end
c**********************************************************************
      subroutine gginv2(gi,w)                                        
c**********************************************************************
c            computes the inverse of the gram matrix gi directly.
c            as this is tridiagonal, it is stored as a 3*np - 2 vector
c            This is done for the fixed frequency w.
c
c          **CALLS FUNCTIONS AIO AND AKO**
c
      parameter (mdist = 350)
      common/bps/b,ifp,ilp,np,p(mdist)
      dimension bi(mdist),bk(mdist),gi(3*np-2)
c
c            compute modified bessel fxs(beta*w*p)
c
      do 500 n = 1,np
      bi(n) = AIO(b*w*p(n))
      bk(n) = AKO(b*w*p(n))
  500 continue
c
c            compute ratios of 1st two bessel fxs of first kind and
c            last two of 2nd kind.  used to scale first and last
c            matrix elements.
c
      g1 = (-bi(2)/bi(1))
      gn = (-bk(np-1)/bk(np))
c
c            compute elements of inverse matrix, using tridiagonality
c
      gi(2) = 1.0/((bi(1)*bk(2)) - (bi(2)*bk(1)))
      gi(1) = g1 * gi(2)
c
      do 520 i = 2,np-1
      kp = (3*i) - 3
      kd = (3*i) - 2
      km = (3*i) - 1
      gi(kp) = gi(km-3)
      gi(km) = 1.0/((bi(i)*bk(i+1)) - (bi(i+1)*bk(i)))
      gid = ((bi(i+1)*bk(i-1)) - (bi(i-1)*bk(i+1)))
      gi(kd) = gi(km) * gi(kp) * gid
  520 continue
c
      gi(3*np-3) = gi(3*np-4)
      gi(3*np-2) = gn * gi(3*np-3)
      return
      end
c**********************************************************************
      subroutine goinv2(gi)                                        
c**********************************************************************
c            computes the inverse of the gram matrix gi directly.
c            as this is tridiagonal, it is stored as a 3*np - 2 vector
c            this is done for the fixed frequency w = 0 only. this
c            uses the small argument approximation of AIO(z) = 1.0 and
c            AKO(z) = -ln(z). AIO and AKO are not computed separately,
c            but only in the combination of their differences, which
c            reduces to ln(arg1/arg2), which is finite.
c
c          **CALLS NO OTHER SUBROUTINES**
c
      parameter (mdist = 350)
      common/bps/b,ifp,ilp,np,p(mdist)
      dimension gi(3*np-2)
c
c            compute elements of inverse matrix, using tridiagonality
c
      gi(2) = 1.0/log(p(1)/p(2))
      gi(1) = -gi(2)
c
      do 520 i = 2,np-1
      kp = (3*i) - 3
      kd = (3*i) - 2
      km = (3*i) - 1
      gi(kp) = gi(km-3)
      gi(km) = 1.0/log(p(i)/p(i+1))
      gid = log(p(i+1)/p(i-1))
      gi(kd) = gi(km) * gi(kp) * gid
  520 continue
c
      gi(3*np-3) = gi(3*np-4)
      gi(3*np-2) = -gi(3*np-3)
      return
      end
c***************************************************************
      subroutine model2(i,j,w,a,wmod)                     
c*****************************************************************
c            forms the representer = Jo(kx)/(x**2 + b**2) and the
c            model = sum of representer times coefficient (alpha)
c            for each freq separately for fixed x.  result is a vector
c            of models, one for each freq.
c            the fourier transform of the models gives T-X.
c
c          **CALLS FUNCTION AJO**
c                      
      parameter (mdist = 350)    
      common/bps/b,ifp,ilp,np,p(mdist)
      common/xs/xmin,xmax,nx,dx,x(2*mdist)
      common/digs/digit,fc,lub2,mm,df,kf,dw,ishift      
      dimension wmod(2,lub2+1,nx),a(2,np)
      wmod(1,j,i) = 0.0
      wmod(2,j,i) = 0.0
      x2b2 = x(i)**2 + b**2
c
c            form the nth representer, rep.  multiply by alpha and sum
c            to form the model (for fixed freq), wmod.
c
          do 820 n = 1,np
          bf = AJO(x(i)*w*p(n))      
          rep = bf/x2b2
          wmod(1,j,i) = wmod(1,j,i) + a(1,n)*rep
          wmod(2,j,i) = wmod(2,j,i) + a(2,n)*rep
  820     continue                
      return
      end
c*********************************************************************
      subroutine preform2(ss,lim,nn,lub2)
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
c           pad end with zeroes to power of two
c
      IF (NN.LT.2*LUB2) THEN
          do 320 j = nn+1,2*(lub2+1)
             ss(j) = 0.0
  320     continue
      ENDIF
      return
      end
c******************************************************************
      subroutine space2(np,nx,lens)
c******************************************************************
c           computes the values of the pointers for array s in common
c           data.  IPNTR locates the beginning of each data trace - 
c           one for each p.  only 2*(kf+1) points are allowed for each
c           trace - accounting for the real and imaginary part of the
c           Fourier-transformed data up to some cutoff point kf assoc-
c           iated with user-selected cutoff frequency fc. one extra
c           'trace' is allowed in order to store the spectrum of the
c           data (it is later overwritten by the spectrum of the model)
c           IGPNT locates the beginning of the inverse Gram matrix for
c           fixed frequency (overwritten by the matrix for each
c           subsequent frequency).  IAPNT locates the beginning of the
c           the vector of coefficients (also overwritten for each
c           frequency).  IMOD locates the beginning of each model
c           trace - one for each x. 2*(lub2+1) points are allowed.
c
c      **CALLS NO OTHER SUBROUTINES**
c
c            set commons and variable types
c                                                       
      parameter (mdist = 350)
      common/digs/digit,fc,lub2,mm,df,kf,dw,ishift
      common/points/ipntr(mdist),igpnt,iapnt,imod(2*mdist)
c
c            compute pointers
c
      do 220 n = 1,np+1
         ipntr(n) = ((n-1) * (kf+1) * 2) + 1         
  220 continue
      igpnt = ipntr(np+1) + lub2+1
      iapnt = igpnt + 3*np-2
      do 230 i = 1,nx
         imod(i) = iapnt + 2*np + ((i-1)*(lub2+1)*2)
  230 continue
c
c             compute overall length
c
      lens = imod(nx) + 2*(lub2+1)
      return
      end
