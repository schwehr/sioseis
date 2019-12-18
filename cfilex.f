      SUBROUTINE cfilex( buf, lbuf, ibuf, scr, lscr, iscr,
     &                    istop, nready )
c  Sept 1998 - Put into sioseis.
c  Mod 21 Mar 00 - Change to Harm's uinc and vinc calculation
c
c  coher_filt.f:  Coherence filter based on semblance over a 
c                 sweep of linear trends about a point.
c
c     Dan Lizarralde, 2/92
c
c     A sweep of Nvel velocities ranging from vmin to vmax is
c     performed across a window of N_xwin traces centered about
c     each sample of each trace.  The slope of the velocities are
c     determined from the approximate intertrace spacing dx and the
c     sign of the trace offset if Vsign=1. (A usefull upgrade would
c     be to include actual trace spacing.) 
c
c     If Vsign=1, then the positive direction is defined as the direction 
c     of increasing trace number, i.e. traces are assume to have increasing 
c     offset values.  The sign of the velocities is determined from the 
c     sign of the header offset.
c
c      If the traces are ordered in the correct  sense and the header
c       offset sign is also correct, then choose ixsign_ch=1.
c       ex.   trace # in file:  1  2  3  4  5  6
c            offset in header: -5 -4 -2  0  1  4
c
c      If the traces are ordered in the opposite sense and the header 
c       offset sign is correct, then choose ixsign_ch=-1.
c       ex.   trace # in file:  1  2  3  4  5  6
c            offset in header:  4  1  0 -2 -4 -5
c
c      If the traces are ordered in the correct sense and the header 
c       offset sign is incorrect, then choose ixsign_ch=-1.
c       ex.   trace # in file:  1  2  3  4  5  6
c            offset in header:  5  4  2  0 -1 -4
c
c      If the traces are ordered in the opposite sense and the header 
c       offset sign is incorrect, then choose ixsign_ch=1.
c       ex.   trace # in file:  1  2  3  4  5  6
c            offset in header: -4 -1  0  2  4  5
c
c     If Vsign=0, then both positive and negative velocities between
c     vmin and vmax will be used.  In this case, however, only Nvel/2
c     velocities for each sign are used. Use this option for coherency
c     filtering stacked data, for instance.
c
c                     N1       : first trace of INFILE to filter
c                     N2       :  last trace of INFILE to filter
c                     dx       : approx. distance between traces in km
c                                used for velocity trajectories
c                 iq_pad_front : write traces 1 to N1-1 to OUTFILE
c
c                 iq_pad_back, Nend
c                 iq_pad_back  : write traces 1 to N1-1 to OUTFILE
c                    Nend      : write traces N2+1 to Nend to OUTFILE
c
c                    T1        : start time of filter [secs]
c                    T2        :   end time of filter [secs]
c
c                   nwin       : odd number of samps for the semblace window
c
c                 iq_U_or_V    : scans range over slownes (1) or velocity (0)
c                    Nvel      : number of velocities to scan
c                    vmin      : minimum velocity to scan
c                    vmax      : maximum velocity to scan
c
c                    Vsign     : Process as signed-offset  (1) 
c                                or unsigned (eg mcs) data (0)
c                    vred      : reduction velocity of the data [km/s]
c                  ixsign_ch   : w/Vsign=1, use -1*(header offsets) to
c                                determine velocity scans
c
c
c                 i_wt_type, spower
c                 i_wt_type = 0 -> use only wt(i) stack 
c                             1 -> linear semblance wt.
c                             2 -> hstrong sigmoidal wt.
c
c
c
c 8 Apr 09 - Use common numdat rather than segy header word ISAMPPTR
c
      PARAMETER (MAX_SAMPS=5000,MAX_NVELS=50,MAX_XWIN=20)
      PARAMETER (IAPSIZ = 500000)
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      DIMENSION set(2)
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /cofilt/ junit, nlists, nwrds, nmulti
      COMMON /apmem/ data(MAX_SAMPS,MAX_XWIN)
      PARAMETER (isize = 262144 )
      COMMON /transp/ tarray(isize)
      EQUIVALENCE (semb,tarray(1))

        real*4
     .               semb(MAX_SAMPS,MAX_NVELS)  ,
     .              fracX(MAX_NVELS,MAX_XWIN),
     .            fracpos(MAX_NVELS,MAX_XWIN),
     .            fracneg(MAX_NVELS,MAX_XWIN), 
     .           wt(MAX_XWIN),wtin(MAX_XWIN),
     .            z(MAX_SAMPS), pi
c****   S1 has number of samples to move per trace per velocity, which
c**** is the same often (like 0 0 0 1 1 would not be uncommon), so 
c**** fracX is used for interpolating the samples (amplitudes).
        Integer*4 S1pos(MAX_NVELS,MAX_XWIN),
     .            S1neg(MAX_NVELS,MAX_XWIN),
     .               S1(MAX_NVELS,MAX_XWIN)

      DIMENSION ranges(MAX_XWIN)
      INTEGER Nvel, Ipt, nT1, nT2
      DATA mlists/0/, iq_median/0/
      SAVE

      IF( istop .EQ. -1 ) GOTO 90                                       ! means end, but no trace in buf
      nready = 0
      IF( lbuf(7) .EQ. 0 ) THEN
          no = lbuf(3)
          itrno = lbuf(4)
      ELSE
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
      si_sec = buf(lsisptr)
c      npts = ibuf(isampptr)
      npts = numdat
      delay = buf(ldelsptr)
      idelay = NINT(delay / buf(lsisptr))
      iout = 0                                                          ! force the data out of the ap
      CALL rlseap( buf(numhdr+1), namps )
      IF( mlists .EQ. 0 ) THEN
          CALL podisc( junit, 1, 0 )                                    ! get the first parameter list from disk
          CALL rddisc( junit, lscr, nwrds, istat )
          mlists = mlists + 1
          lprint = lscr(1)
          dx = scr(2)
          nxpad = lscr(3)
          winlen = scr(4)
          nwin = winlen / si_sec
          ftr = lscr(5)
          ltr = lscr(6)
          nprestk = lscr(7)
          i_wt_type = lscr(8)
          spower = scr(9)
          N_xwin = lscr(10)
          vred = lscr(11)
          vsign = lscr(12)
          ixsign_ch = lscr(13)
          nwts = lscr(14)
          set(1) = scr(nmulti+1)
          set(2) = scr(nmulti+2)
          DO i = 1, nwts
             wtin(i) = scr(nmulti+2+i)
          ENDDO
          IF( scr(nmulti+2+MAX_XWIN+1) .NE. 0 ) THEN                    ! vmmi or velocity
              vmin = scr(nmulti+2+MAX_XWIN+1)
              vmax = scr(nmulti+2+MAX_XWIN+2)
              vinc = scr(nmulti+2+MAX_XWIN+3)
          ENDIF
          IF( scr(nmulti+2+MAX_XWIN+3+1) .NE. 0 ) THEN                  ! ummi or slowness
              vmin = scr(nmulti+2+MAX_XWIN+3+1)
              vmax = scr(nmulti+2+MAX_XWIN+3+2)
              vinc = scr(nmulti+2+MAX_XWIN+3+3)
          ENDIF
          Nvel = NINT((vmax-vmin)/vinc+1.)
          iq_U_or_V = 0
          IF( scr(nmulti+2+MAX_XWIN+3+1) .NE. 0 ) iq_U_or_V = 1
          ntraces = 0
          last = 0
          mindel = 999999.
          CALL getfil( 1, lunidata, lscr, istat )
          CALL getfil( 1, lunhdata, lscr, istat )
          CALL getfil( 1, lunodata, lscr, istat )
          IF(mod(nwin,2)   .eq. 0)   nwin=nwin+1
          IF(mod(N_xwin,2) .eq. 0) N_xwin=N_xwin+1
          IF(mod(Nvel,2)   .ne. 0)   Nvel=Nvel+1
          nwts2 = nwts/2
          Nx2   = N_xwin/2
          Nxmid = N_xwin/2+1
          scale = float(N_xwin)
          pi=3.14159
c        nT1  = int(T1/si_sec)+1
c        nT2  = int(T2/si_sec)+1
c        if(nT2 .gt. npts)nT2 = npts

c         Define semblance scans in terms of slowness or velocity
          ured = 0.
          IF (vred .ne. 0) ured=1./vred
          Nxmid= 1+N_xwin/2
          umax = 1./vmin
          umin = 1./vmax
          if(iq_U_or_V .eq. 1)then
c****        Slowness
             if(Vsign .eq. 1)then
c                uinc = (umax-umin+1)/(float(Nvel))   *****  Dan
c                uinc = (umax-umin)/(float(Nvel-1))   *****  Harm
                uinc = (umax-umin)/(float(Nvel-1))
                do ivel=1,Nvel
                   u=umin+float(ivel-1)*uinc
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=(u-ured)*x
                      S1pos(ivel,ix)=int(t/si_sec)
c	 print *,' ivel=',ivel,' x=',x,' u',u,' t',t,' S1p=',S1pos(ivel,ix)
                      fracpos(ivel,ix)=t-float(S1pos(ivel,ix))*si_sec
                   end do
                end do
                do ivel=1,Nvel
                   u=-1.*(umin+float(ivel-1)*uinc)
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=(u+ured)*x
                      S1neg(ivel,ix)=int(t/si_sec)
c	 print *,' ivel=',ivel,' x=',x,' u',u,' t',t,' S1n=',S1neg(ivel,ix)
                      fracneg(ivel,ix)=t-float(S1neg(ivel,ix))*si_sec
                   end do
                end do
             else if(Vsign .eq. 0)then
c                uinc = (umax-umin+1)/(float(Nvel)*.5)  *****  Dan
c                uinc = (umax-umin)/(float(Nvel-1)*.5)  *****  Harm
c                uinc = (umax-umin)/(float(Nvel-2)*.5)  *****  Paul
                uinc = (umax-umin)/(float(Nvel-2)*.5)
                do ivel=1,Nvel/2
                   u=umin+float(ivel-1)*uinc
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=u*x
                      S1(ivel,ix)=int(t/si_sec)
                      fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
                   end do
                end do
                do ivel=Nvel/2+1,Nvel
                   ivv=ivel-Nvel/2
                   u=-1.*(umin+float(ivv-1)*uinc)
                   do ix=1,N_xwin
                      x=float(ix-Nxmid+1)*dx  
                      t=u*x
                      S1(ivel,ix)=int(t/si_sec)
                      fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
                   end do
                end do
             end if
          else
c****        Velocity
             if(Vsign .eq. 1)then
c                vinc = (vmax-vmin+1)/(float(Nvel))   ****   Dan
c                vinc = (vmax-vmin)/(float(Nvel-1))   ****   Harm
                vinc = (vmax-vmin)/(float(Nvel-1))
                do ivel=1,Nvel
                   v=vmin+float(ivel-1)*vinc
                   u=1./v
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=(u-ured)*x
                      S1pos(ivel,ix)=int(t/si_sec)
                      fracpos(ivel,ix)=t-float(S1pos(ivel,ix))*si_sec
                   end do
                end do
                do ivel=1,Nvel
                   v=vmin+float(ivel-1)*vinc
                   u=-1./v
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=(u+ured)*x
                      S1neg(ivel,ix)=int(t/si_sec)
                      fracneg(ivel,ix)=t-float(S1neg(ivel,ix))*si_sec
                   end do
                end do
             else if(Vsign .eq. 0)then
c                vinc = (vmax-vmin+1)/(float(Nvel)*.5)     *****  Dan
c                vinc = (vmax-vmin)/(float(Nvel-1)*.5)     *****  Harm
c                vinc = (vmax-vmin)/(float(Nvel-2)*.5)     *****  Paul
                vinc = (vmax-vmin)/(float(Nvel-2)*.5)
                do ivel=1,Nvel/2
                   v=vmin+float(ivel-1)*vinc
                   u=1./v
                   do ix=1,N_xwin
                      x=float(ix-Nxmid)*dx
                      t=u*x
                      S1(ivel,ix)=int(t/si_sec)
                      fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
                   end do
                end do
                do ivel=Nvel/2+1,Nvel
                   ivv=ivel-Nvel/2
                   v=vmin+float(ivv-1)*vinc
                   u=-1./v
                   do ix=1,N_xwin
                      x=float(ix-Nxmid+1)*dx
                      t=u*x
                      S1(ivel,ix)=int(t/si_sec)
                      fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
                   end do
                end do
             end if
          end if
      ENDIF
c****
c****  Collect all the traces on disk until End-of-Gather or End-of-Job
c****
      CALL wrdisc( lunhdata, buf(1), numhdr)
      CALL wrdisc( lunidata, buf(61), npts)
      ntraces = ntraces + 1
      mindel = MIN( mindel, idelay )
      msamps = MIN( msamps, npts )
      last = MAX( last, idelay+npts )
c**** Nasty problem here.  How do we differentiate between prestack
c**** gathers and post stack?  Both have -1 in word 51.  Ah, but stack
c**** sets word 17 (the cdp fold) - but what if it's a dead stack trace?
      IF( lbuf(51) .NE. -1 .AND. istop .EQ. 0 ) THEN
          nready = 0
          RETURN
      ENDIF
      IF( lbuf(51) .EQ. -1 .AND. ibuf(17) .NE. 0 .AND. istop .EQ.0) THEN
          nready = 0
          RETURN
      ENDIF
c****
c**** Do the whole dataset, make sure everything has the same delay 
c**** and length.  Start by adding the pads.
c****
   90 CONTINUE
      CALL podisc( lunhdata, 0, 0 )
      CALL podisc( lunidata, 0, 0 )
      npts = last - mindel
      IF( npts .GT. MAX_SAMPS ) THEN
          PRINT *,' ***  ERROR  ***  Not enough memory for COFILT.'
          PRINT *,' Decrease the record length.'
          STOP
      ENDIF
      icnt = 1                                                          ! count the number traces, including the pads
      IF( nxpad .GT. 0 ) THEN
          DO i = 1, nxpad
             DO j = 1, npts
                data(j,icnt) = 0.
             ENDDO
             icnt = icnt + 1
          ENDDO
      ENDIF
c**** Another problem here.  What if there aren't N_xwin traces?
      IF( ntraces .LT. N_xwin ) THEN
          PRINT *,' ***  ERROR  ***  Too few traces.  Decrease N_xwin.'
          STOP
      ENDIF
      DO i = 1, N_xwin
         CALL rddisc( lunhdata, scr, numhdr, istat )
         ranges(i) = FLOAT(lscr(ldisptr))
c         nsamps = iscr(isampptr)
         CALL ushort2long( iscr(isampptr), nsamps )
         delay = scr(ldelsptr)
         idelay = NINT(delay / scr(lsisptr))
         index = 1
         IF( idelay .GT. mindel ) THEN
             n = idelay - mindel
             DO j = 1, n
                data(j,icnt) = 0.
             ENDDO
             index = n + 1
         ENDIF
         CALL rddisc( lunidata, scr, nsamps, istat )
         DO j = 1, nsamps
            data(index+j-1,icnt) = scr(j)
         ENDDO
c****    fill/pad the ends
         n = npts - (idelay - mindel + nsamps)
         IF( n .GT. 0 ) THEN
             index = idelay - mindel + nsamps
             DO j = 0, n-1
                data(index+j,icnt) = 0.
             ENDDO
         ENDIF
         icnt = icnt + 1
      ENDDO
      IF( nxpad .GT. 0 ) THEN
          DO i = 1, nxpad
             DO j = 1, npts
                data(j,icnt) = 0.
             ENDDO
             icnt = icnt + 1
          ENDDO
          N_xwin = N_xwin + 1
      ENDIF
c**** wt = modified weights.  iwt1, iwt2, wtfac were for median filter
      IF( nwts .GT. 0 ) CALL get_weights(N_xwin,nwts,wtin,iq_median,
     .                   iwt1,iwt2,wt,wtfac,istat)
      IF( istat .EQ. 1 ) STOP
c****  Do the whole trace - do no honor parameter set
      nT1 = 1
      nT2 = npts
c
c       get amplitude for relative amplitude scaling
c
        amp_0=0.
        vnpts=1./float(nT2-nT1+1)
        do it=nT1,nT2
c          amp_0=amp_0+abs(data(it,Nx2+1))*vnpts
           amp_0=max(amp_0,abs(data(it,Nx2+1)))
        end do
        amp_scl=0.
        if(amp_0.eq.0) THEN
           PRINT *,' Error: Average amplitude of first trace=0.'
           PRINT *, '       Scaling wont work. Try a new N1.'
           STOP
        ENDIF
c
c       initialize some arrays
c
        do iv=1,Nvel
           do it=1,npts
              semb(it,iv)=0.
           end do
        end do
        do i=1,npts
           buf(i)=0.
        end do

        do itrc= 1, ntraces
           if(mod(itrc,50).eq.0) print *, 'Trace', itrc
           if(Vsign .ne. 0)then
              ixsign=ixsign_ch*sign(1.,ranges(itrc))
              if(ixsign.eq.1)then
                 do ix=1,N_xwin
                    do ivel=1,Nvel
                       S1(ivel,ix)=S1pos(ivel,ix)
                       fracX(ivel,ix)=fracpos(ivel,ix)
                    end do
                 end do
              else
                 do ix=1,N_xwin 
                    do ivel=1,Nvel 
                       S1(ivel,ix)=S1neg(ivel,ix)   
                       fracX(ivel,ix)=fracneg(ivel,ix)
                    end do 
                 end do 
              end if
           end if
c
c          For every point on the trace, calculate semblance 
c          for every velocity of the scan
c****     semblance is the square of the sum over the sum of the squares
           do Ipt=nT1,nT2
              Do ivel=1,Nvel
                 sum1=0.
                 sum2=0.
                 Do ix=1,N_xwin
                    i1 = Ipt+S1(ivel,ix)
                    i1 = min(i1,npts)
                    i1 = max(1,i1)
                    val  = data(i1,ix)+fracX(ivel,ix)*
     .                    (data(i1+1,ix)-data(i1,ix))
                    sum1 = sum1+val
                    sum2 = sum2+val*val
                 end do
                 if (sum2 .ne. 0.) then
                    semb(Ipt,ivel)=(sum1*sum1)/(sum2*scale)
                 end if
              end do
           end do
c****
           sembmax=0.
           do ipt=nT1,nT2
c****         Find the best semblance of all the velocities
              sembpt=0.
              do ivel=1,Nvel
                 if(semb(ipt,ivel) .gt. sembpt)then
                    iivl=ivel
                    sembpt=semb(ipt,ivel)
                    sembmax=amax1(sembmax,sembpt)
                 End if
              end do
c****         Get the average semblance of the semblance window.
              IF( i_wt_type .GT. 0 ) THEN
                  zsum=0.
                  do iw=1,nwin
                     jw=ipt-(nwin/2+1)+iw
                     jw=min(jw,nT2)
                     jw=max(jw,nT1)
                     zsum=zsum+semb(jw,iivl)
                  end do
                  z(ipt)=zsum/float(nwin)
              ENDIF
c****         The output is the weighted sum of the data of n_xwin
c****         traces along the best velocity trajectory
              buf(ipt) = 0.
              Do ix=1,N_xwin
                 i1 = ipt+S1(iivl,ix)
                 i1 = min(i1,npts)
                 i1 = max(1,i1)
                 val= data(i1,ix)+fracX(iivl,ix)*
     .               (data(i1+1,ix)-data(i1,ix))
                 IF( wt(ix) .NE. 0 ) THEN
                     buf(ipt)=buf(ipt)+val*wt(ix)
                 ELSE
                     buf(ipt)=buf(ipt)+val
                 ENDIF
              end do
           end do
c****      type 1 weight then multiplies each sample by the average
c****      sample associated with the point, raised to spower
           if(i_wt_type .eq. 1)then
              do ipt=nT1,nT2
                 buf(ipt)=buf(ipt)*(z(ipt)**spower)
              end do
           else if(i_wt_type .eq. 2)then
              do Ipt=nT1,nT2
                 if(sembmax .eq. 0.)sembmax=1.
                 s=z(Ipt)/sembmax
                 wtfun=.67*(1.-cos(pi*s))*sin(pi*s*.5)*sin(pi*s/1.2)
                 if(s .ge. .8) wtfun=1.
                 buf(Ipt)=buf(Ipt)*wtfun
              end do
           end if	

           if(amp_scl .eq. 0.)then
              jmp_1=0.
              do it=nT1,nT2
c                amp_1=amp_1+abs(buf(it))*vnpts
                 amp_1=max(amp_1,abs(buf(it)))
              end do
c****         amp_0 is the average amplitude of the first trace.
              amp_scl=amp_0/amp_1
           end if
           do it=nT1,nT2
              buf(it)=buf(it)*amp_scl
           end do

           CALL podisc( lunhdata, 0, (itrc-1) * numhdr)
           CALL rddisc( lunhdata, scr, numhdr, istat )
c           nsamps = iscr(isampptr)
           CALL ushort2long( iscr(isampptr), nsamps )
c           iscr(isampptr) = npts
           CALL long2ushort( npts, iscr(isampptr) )
           numdat = npts
           delay = FLOAT(mindel) * si_sec
           scr(ldelsptr) = delay
           iscr(idelmptr) = NINT(delay*1000.)                          ! in mils!
           CALL wrdisc( lunodata, scr, numhdr )
           CALL wrdisc( lunodata, buf, npts )
           do ix=1,N_xwin-1
              do ip=1,npts
                 data(ip,ix)=data(ip,ix+1)
              end do
           end do
           CALL rddisc( lunidata, scr, nsamps, istat )
           IF( istat .GT. 0 ) THEN
               DO ip = 1, npts
                  data(ip,N_xwin) = scr(ip)
               ENDDO
           ELSE
             DO j = 1, npts
                data(j,icnt) = 0.
             ENDDO
           ENDIF
        End do	
        nready = ntraces
        CALL podisc( lunodata, 0, 0 )
        RETURN

      ENTRY getcfil( ibuf )
c**** Get the output from disk when requested by contro
      CALL rddisc( lunodata, ibuf, numhdr, istat )
c      nsamps = ibuf(isampptr)
      CALL ushort2long( ibuf(isampptr), nsamps )
      numdat = nsamps
      CALL rddisc( lunodata, ibuf(numhdr+numhdr+1), nsamps, istat )
      last = 0
      ntraces = 0
      msamps = 0
      RETURN
      END
c
c
c
c---------------------------------------------------------------------
c
        subroutine get_weights(N_xwin,nwts,wtin,iq_median,
     .                         iwt1,iwt2,wt,wtfac,istat) 

        real*4   wt(N_xwin),wtin(N_xwin),wtfac

        do i=1,N_xwin
           wt(i)=0.
        end do
 
        if(mod(nwts,2).eq.0)then
           print *,'nwts must be an odd number'
           istat=1 
           goto 199
        end if
 
        if(iq_median.eq.1 .and. nwts.eq.N_xwin)then
           nwts=nwts-2
           do i=1,nwts
              wtin(i)=wtin(i+2)
           end do
        end if
 
        nwts2= nwts/2
        Nxmid= 1+N_xwin/2
        iwt1 = Nxmid-nwts2
        iwt2 = Nxmid+nwts2
c****
c**** Looks like a bug to me if nwts = N_win.  Why not zero all of them?
c**** pch
        do iw=1,iwt1-1
           wt(iw)=0.
        end do
        do iw=iwt2+1,N_xwin
           wt(iw)=0.
        end do
        do iw=iwt1,iwt2
           jw=iw-iwt1+1
           wt(iw)=wtin(jw)
        end do
 
        wtfac=0.
        do iw=1,N_xwin
           wtfac=wtfac+wt(iw)
        end do
        if(wtfac .ne. 0)then
           wtfac=1./wtfac
        else
           print *, 'Error: wts appear to be zero.'
           istat = 1
           goto 199
        end if
 
        do iwx=1,N_xwin
           wt(iwx)=wt(iwx)*wtfac
        end do

 199    return
        end
c
c-----------------------------------------------------------------------
c
        subroutine get_vels(vmin,vmax,vred,Vsign,Nvel,N_xwin,dx,si_sec,
     .                      S1,S1pos,S1neg,fracX,fracpos,fracneg)

        integer*4  S1(Nvel,N_xwin),
     .             S1pos(Nvel,N_xwin),
     .             S1neg(Nvel,N_xwin)
           real*4  fracX(Nvel,N_xwin),
     .             fracpos(Nvel,N_xwin),
     .             fracneg(Nvel,N_xwin),
     .             x,t,umin,umax
         

        if (vred .ne. 0) then
           ured=1./vred
        else
           ured=0.
        end if

        print *, 'Vsign,Nvel,vmin,vmax'
        print *,  Vsign,Nvel,vmin,vmax

        Nxmid= 1+N_xwin/2
        umax = 1./vmin
        umin = 1./vmax

        if(Vsign .eq. 1)then
           uinc = (umax-umin+1)/(float(Nvel))
           do ivel=1,Nvel
              u=umin+float(ivel-1)*uinc
              do ix=1,N_xwin
                 x=float(ix-Nxmid)*dx
                 t=(u-ured)*x
                 if(ix .lt. Nxmid)t=(u+ured)*x
                 S1pos(ivel,ix)=int(t/si_sec)
              fracpos(ivel,ix)=t-float(S1pos(ivel,ix))*si_sec
              end do
           end do
c
c          uinc = -1.*(umax-umin+1)/(float(Nvel))
c          umin = -1.*umin
           do ivel=1,Nvel
              u=-1.*(umin+float(ivel-1)*uinc)
              do ix=1,N_xwin
                 x=float(ix-Nxmid)*dx
                 t=(u-ured)*x
                 if(ix .gt. Nxmid)t=(u+ured)*x
                 S1neg(ivel,ix)=int(t/si_sec)
              fracneg(ivel,ix)=t-float(S1neg(ivel,ix))*si_sec
              end do
           end do
c
        else if(Vsign .eq. 0)then
c
           uinc = (umax-umin+1)/(float(Nvel)*.5)
           do ivel=1,Nvel/2
              u=umin+float(ivel-1)*uinc
              do ix=1,N_xwin
                 x=float(ix-Nxmid)*dx
                 t=u*x
                 S1(ivel,ix)=int(t/si_sec)
              fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
                 print *, ix, ivel, 1./u, t, S1(ivel,ix),fracX(ivel,ix)
              end do
           end do
           do ivel=Nvel/2+1,Nvel
              ivv=ivel-Nvel/2
              u=-1.*(umin+float(ivv-1)*uinc)
              do ix=1,N_xwin
                 x=float(ix-Nxmid+1)*dx  
                 t=u*x
                 S1(ivel,ix)=int(t/si_sec)
              fracX(ivel,ix)=t-float(S1(ivel,ix))*si_sec
              end do
           end do
        end if

        return
        end
