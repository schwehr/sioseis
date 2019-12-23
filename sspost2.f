      SUBROUTINE sspost2( lundata, lunslice, lunvel, lunt, fmin, fmax,
     &      delay, zbig, vbig, ref_type, Nx, dx_km, Npadlt, Npadrt,
     &      nspx_mig, Nt, dt_sec, Nz, dz_km, cbuf, scr, cscr )
c
c      fk, post-stack migration with split-step method of lateral
c      velocity variation correction .

c     Call this for every "panel", with lundata containing the fx data
c  running from fmin to fmax, in complex, (nw*2 real words).
c
c        Positive frequencies between fmin & fmax are downward
c        continued, stored in the complex array rows
c
c  mod 18 Mar 97 - Add some info to error message when it doesn't fit.
c  mod June 2000 - g77 - change type to print
c  Mod Jan 01 - Up iapsiz to 5000000
c
c
      PARAMETER (iapsiz = 5000000, ntb=8192, nxb=8192)
      PARAMETER (isize = 262144 )                                       ! 512 x 512
       parameter (nzb=5000, nrows=800 )

        complex eye, idzw, idz, ph1, ph2, cdat, cbuf(1), cscr(1)
c     .          data(nxb,nrows)

        real*4     r(ntb),   c(ntb),
     .           kx2(nxb),
     .            ux(nxb), uav(nzb),
     .          ctap(nxb),
     .            xz(nxb),
     .              dx_km,  dt_sec, dz_km,
     .               wmin,  wmax,
c     .           Zrow(nxb,nrows*2),
     .           scr(1)

        integer*4 lundata

        integer*4   H4(ntb)
        integer*2   H2(ntb)
        equivalence (H4(1),H2(1))

c        equivalence(data(1,1),Zrow(1,1))

      INTEGER ref_type
c Ref_type:
c  ref_type = 0 -> u0=min slowness (max vel)
c  ref_type = 1 -> u0=avg slowness
c  ref_type = 2 -> u0=max slowness (min vel)
c
c
      REAL    APDATA(0:iapsiz)
      INTEGER IAPDATA(0:iapsiz)
      COMMON /apmem/ apdata
      EQUIVALENCE (APDATA,IAPDATA), (apdata,data)

      COMMON /transp/ t(isize)
      COMPLEX ct(1)
      EQUIVALENCE (t(1),ct(1))

      LOGICAL big
      DATA big/.FALSE./

c      print *,' nx ',nx,' fmin ',fmin,' fmax ',fmax,' delay ',delay
c      print *,' zbig ',zbig,' vbig ',vbig,' ref ',ref_type
c      print *,' Npadlt ',Npadlt,' Npadrt ',Npadrt,' nspx ',nspx_mig
c
c     get powers of two
c
      Nxmig=Nx+Npadlt+Npadrt
      call powtwo(Nxmig,Nkx)
      call powtwo(Nt,Nft)
c      write(*,*) 'Nx=',Nx,'  Nxmig=',Nxmig, '  Nkx=',Nkx, ' dx=',dx_km
c      write(*,*) 'Nt=',Nt,'    Nft=',Nft, '  dt=',dt_sec
      pi=3.141592653589
      twopi=2.*pi
      eye = (0.,1.)
      CALL podiscb( lunvel, 1, 0 )                                      ! rewind the velocity file
      CALL podiscb( lundata, 1, 0 )
      CALL podiscb( lunslice, 1, 0 )
c
c     compute the displacement to the range of frequencies to be used
      dfreq = 1./(dt_sec*float(Nft-1))
      dw    = twopi/(dt_sec*float(Nft-1))
      dkx   = twopi/(dx_km*float(Nkx-1))
      wmin  = twopi*fmin
      wmax  = twopi*fmax
      iwmin = int(wmin/dw)+1                                             !GMK DANL jive with ex phase
      iwmax = int(wmax/dw)+1                                             !GMK DANL jive with ex phase
c      Nw    = (INT( twopi*(fmax-fmin)/dw) + 1)                           !GMK DANL jive with ex phase
      nw = iwmax - iwmin + 1                                            ! pch apr 96
      scalw = 1./float(Nft)
      scalx = 1./float(Nkx)
c
      if(Nw .gt. nrows)then
        PRINT *,'***Input data does not fit; nw=',nw,' nrows=',nrows
        PRINT *,' Reduce the number of frequencies using fmin/fmax'
        PRINT *,' or reduce the data length in the time domain.'
        stop
      end if
      IF( nkx * nw * 2 .GT. isize ) THEN
          big = .TRUE.
          CALL getfil( 1, lunt, scr, istat )
      ENDIF
c
c     read velocity file and output slowness slices, with pads
      CALL getfil( 1, iufile, scr, istat )                              ! get a scratch velocity file
      do jz = 1, nz
c         if (mod(jz,100) .eq. 0) type *,jz
         CALL rddisc( lunvel, r(1), nx, istat )
c         read(ivfile,rec=jz) (r(jx),jx=1,NxvFile)
         do jx=Npadlt+1,Npadlt+Nx
	       lx = jx - Npadlt
            ux(jx)=2./r(lx)
         end do
         do jx=1,Npadlt
            ux(jx)=ux(Npadlt+1)
         end do
         do jx=Npadlt+nx+1,Nkx
            ux(jx)=ux(Npadlt+Nx)
         end do
         if(ref_type .eq. 0)then
            uav(jz) =  999.
            do jx=Npadlt+1, Npadlt+Nx
               if(ux(jx).ne.0)then
                  uav(jz) = min(uav(jz),ux(jx))
               end if
            end do
         else if(ref_type .eq. 1)then
            uav(jz) = 0.
            do jx = Npadlt+1, Npadlt+Nx
               uav(jz) = uav(jz) + ux(jx)
            end do
            uav(jz) = uav(jz)/float(Nx)
         else if(ref_type .eq. 2)then
            uav(jz) = -999.
            do jx=Npadlt+1, Npadlt+Nx
               uav(jz) = max(uav(jz),ux(jx))
            end do
         end if
         CALL wrdisc( iufile, ux, Nkx )
c         write(iufile,rec=jz) (ux(jx),jx=1,Nkx)
      end do
	 print *,' slowness done'
c****
c**** zero out the whole array
c****
c      do i=1,nrows
c         do j=1,Nkx
c            data(j,i) = cmplx(0.,0.)
c         end do
c      end do
c      do it=1,nft
c         r(it)=0.
c         c(it)=0.
c      end do
      CALL podisc( lundata, 1, 0 )
c****
c     Get main panel of data - it's in w already (from iwmin to iwmax)
c****
      istop = 0
      DO ix = Npadlt+1,Nx+Npadlt
         CALL rddisc( lundata, r, nw, istat )
         CALL rddisc( lundata, c, nw, istat )
         if(Delay .gt. 0.)then
            do iw=iwmin,iwmax
               jw=iw-iwmin+1
               w=dw*float(iw-1)
               cdat=cmplx(r(jw),-1.*c(jw))
               ph1=eye*w*Delay
c               data(ix,jw)=cdat*cexp(ph1)
               cbuf(jw) = cdat*cexp(ph1)
            end do
         else
            do iw = 1, nw
c               data(ix,iw) = cmplx(r(iw),-1.*c(iw))
               cbuf(iw) = cmplx(r(iw),-1.*c(iw))
            end do
         end if
         IF( ix .EQ. npadlt+1 .AND. npadlt .GT. 0 ) THEN
             pd_taper=1./float(Npadlt)
             DO i = 1, npadlt
                pd_scale = FLOAT(i) * pd_taper
                do iw = 1, nw
c                   data(ix,iw) = data(i,iw) * pd_scale
                   cscr(iw) = cbuf(iw) * pd_scale
                END DO
                IF( .NOT. big ) THEN
                    CALL ctrans1( cscr, nw, nkx, istop )
                ELSE
                    CALL ctrans2( cscr, nw, nkx, lunt, istop )
                ENDIF
             END DO
          ENDIF
          IF( ix .EQ. nkx ) istop = 1
          IF( .NOT. big ) THEN
              CALL ctrans1( cbuf, nw, nkx, istop )
          ELSE
              CALL ctrans2( cbuf, nw, nkx, lunt, istop )
          ENDIF
      ENDDO
      IF( npadrt .GT. 0 ) THEN                                          ! do the right pad
          pd_taper=1./float(Npadrt)
c          i = npadlt + nx
          do ix = Nx+Npadlt+1,Nxmig
             jx=ix-(Nx+Npadlt+1)+1
             pd_scale=(Npadrt-jx)*pd_taper
             do iw = 1, nw
c                data(ix,iw) = data(i,iw) * pd_scale
                cscr(iw) = cbuf(iw) * pd_scale
             end do
c******   pch changed from istop = 0   to istop = 1 on Apr 15 96
             IF( ix .EQ. nkx ) istop = 1
             IF( .NOT. big ) THEN
                 CALL ctrans1( cscr, nw, nkx, istop )
             ELSE
                 CALL ctrans2( cscr, nw, nkx, lunt, istop )
             ENDIF
          end do
      ENDIF
      IF( nkx .NE. nxmig ) THEN                                        ! Fill out with zeroes
          DO i = 1, nw
             cscr(i) = CMPLX(0.,0.)
          ENDDO
          DO i = nxmig+1, nkx
             IF( i .EQ. nkx ) istop = 1
             IF( .NOT. big ) THEN
                 CALL ctrans1( cscr, nw, nkx, istop )
             ELSE
                 CALL ctrans2( cscr, nw, nkx, lunt, istop )
             ENDIF
          ENDDO
      ENDIF
      CALL podisc( lunt, 1, 0 )
c
c     get taper used in migration
c
      call get_taper(Nkx,nspx_mig,ctap)
      do jf=1,Nkx
         r(jf)=0.
         c(jf)=0.
      end do
c
c     get the dkx**2 vector
      call get_kx2(kx2,Nkx,dkx)
c
      if(Zbig .gt. 0.)then
         Nbigz = NINT(Zbig/dz_km)
         write(*,*) 'Migrating big step ...'
         idz= cmplx(0.,Zbig)
         do ix=1,Nkx
            xz(ix)=0.
         end do
         CALL podisc( lunslice, 1, 0 )
         IF( big ) CALL podisc( lunt, 1, 0 )
         u0 = 2./Vbig
         do iw = iwmin,iwmax
            jw=iw-iwmin+1
c            do ix=1,Nkx
c               r(ix)=     real(data(ix,jw))
c               c(ix)=-1.*aimag(data(ix,jw))
c            end do
            IF( .NOT. big ) THEN
                index = (jw-1) * nkx
                DO ix = 1, nkx
                   r(ix) = REAL(ct(index+ix))
                   c(ix) = -1. * AIMAG(ct(index+ix))
                ENDDO
            ELSE
c*** removed apr 96       CALL podisc( lunt, 1, 0 )
                CALL rddisc( lunt, cscr, nkx*2, istat )
                DO ix = 1, nkx
                   r(ix) = REAL(cscr(ix))
                   c(ix) = -1. * AIMAG(cscr(ix))
                ENDDO
            ENDIF
            call fastf(r,c,Nkx)
            w  = dw*float(iw-1)
            u0w2 = (w*u0)**2.
            do jx = 1,Nkx
               ph1 = cmplx(u0w2-kx2(jx),0.)
               ph1 = csqrt(ph1)*idz
               cdat = cexp(ph1)*cmplx(r(jx),c(jx))
               r(jx) =     real(cdat)
               c(jx) =-1.*aimag(cdat)
            end do
            call fastf(r,c,Nkx)
            IF( .NOT. big ) THEN
                do ix=1,Nkx
c                   data(ix,jw)=cmplx(r(ix),c(ix))*ctap(ix)
                   ct(index+ix) = cmplx(r(ix),c(ix))*ctap(ix)
                end do
            ELSE
                do ix=1,Nkx
                   cscr(ix) = cmplx(r(ix),c(ix))*ctap(ix)
                end do
                CALL podisc( lunt, 2, - nkx * 2 )
                CALL wrdisc( lunt, cscr, nkx * 2 )
            ENDIF
         end do
         do iz=1,Nbigz                                                  ! xz is 0.
            CALL wrdisc( lunslice, xz(1+Npadlt), Nx )
c            write(lunslice,rec=iz) (xz(ix),ix=1+Npadlt,Nx+Npadlt)
         end do
         Nsrcz = Nbigz + 1
      else
         Nsrcz=1
      end if
c****
c     Main loop over all depths
c****
      PRINT *,' Data is now in kx space.'
      idz= cmplx(0.,dz_km)
      iaddr = (nsrcz-1) * Nkx
      CALL podisc( iufile, 1, iaddr )
      CALL podisc( lunslice, 1, nx*(nsrcz-1) )
      do 1001 jz = Nsrcz, nz
c         IF( big ) CALL podisc( lunt, 1, iaddr )
c         IF( big ) CALL podisc( lunt, 1, (jz-1) * Nkx * 2 )
         IF( big ) CALL podisc( lunt, 1, 0 )
         CALL rddisc( iufile, ux(1), Nkx, istat )
c         read(iufile,rec=jz) (ux(jx),jx=1,Nkx)
         u0  = uav(jz)
         write(*,*) 'depth int' , jz,', v0=',2./u0,' uav=',uav(jz)
         do ix=1,Nkx
            xz(ix)=0.
         end do
c        loop over frequencies
         do iw = iwmin,iwmax
            jw = iw-iwmin+1
c           forward fft
c            do ix=1,Nkx
c               r(ix)=     real(data(ix,jw))
c               c(ix)=-1.*aimag(data(ix,jw))
c            end do
            IF( .NOT. big ) THEN
                index = (jw-1) * nkx
                DO ix = 1, nkx
                   r(ix) = REAL(ct(index+ix))
                   c(ix) = -1. * AIMAG(ct(index+ix))
                ENDDO
            ELSE
                CALL rddisc( lunt, cscr, nkx*2, istat )
                DO ix = 1, nkx
                   r(ix) = REAL(cscr(ix))
                   c(ix) = -1. * AIMAG(cscr(ix))
                ENDDO
            ENDIF
            call fastf(r,c,Nkx)
c****       phase shift over all kx
            w = dw*float(iw-1)
            u0w2 = (w*u0)**2.
            do jx = 1,Nkx
               ph1 = cmplx(u0w2-kx2(jx),0.)
               ph1 = csqrt(ph1)*idz
               cdat = cmplx(r(jx),c(jx))*cexp(ph1)
               r(jx) = real(cdat)
               c(jx) =-1.*aimag(cdat)
            end do
            dzw  = w*dz_km
            idzw = cmplx(0.,dzw)
c****       invrs fft, 2nd phase shift, imaging
            call fastf(r,c,Nkx)
            IF( .NOT. big ) THEN
                do ix=1,Nkx
                   ph2= idzw*cmplx((u0-ux(ix)),0.)
c                   data(ix,jw)=cmplx(r(ix),c(ix))*cexp(ph2)*ctap(ix)
                   ct(index+ix) = cmplx(r(ix),c(ix))*cexp(ph2)*ctap(ix)
c                   xz(ix) = xz(ix)+real(data(ix,jw))*scalw
                   xz(ix) = xz(ix) + real(ct(index+ix)) * scalw
                end do
            ELSE
                do ix=1,Nkx
                   ph2= idzw*cmplx((u0-ux(ix)),0.)
                   cscr(ix) = cmplx(r(ix),c(ix))*cexp(ph2)*ctap(ix)
                   xz(ix) = xz(ix) + real(cscr(ix)) * scalw
                end do
                CALL podisc( lunt, 2, - nkx * 2 )
                CALL wrdisc( lunt, cscr, nkx * 2 )
            ENDIF
         end do
c****
c        Integration done, write out image slice
c****
         CALL wrdisc( lunslice, xz(1+Npadlt), Nx )
c         write(lunslice,rec=jz) (xz(ix),ix=1+Npadlt,Nx+Npadlt)
 1001 continue
c****
c       type *, 'Transposing image file, outputting data ...'
c****
c      CALL podisc( lundata, 1, 0 )
c      CALL podisc( lunslice, 1, 0 )
c       do iz=1,Nz,nrows*2
c          jz2=nrows*2
c          if((iz-1+nrows*2).gt.Nz)jz2=Nz-iz+1
c          do jz=1,jz2
c             CALL rddisc( lunslice, scr, Nx, istat )
c             do jx=1, Nx
c                Zrow(jx,jz) = scr(jx)
c             end do
c             read(lunslice,rec=irec) (Zrow(jx,jz),jx=1,Nx)
c          end do
c          do ix=1,Nx
c             do jz=1,jz2
c                irec=iz+jz-1
c                r(irec)=Zrow(ix,jz)
c             end do
c             CALL WRDISC( lundata, r, jz2 )
c          end do
c       end do
c      DO iz = 1, nz
c         CALL rddisc( lunslice, scr, Nx, istat )
c          jz2=nrows*2
c          if((iz-1+nrows*2).gt.Nz)jz2=Nz-iz+1
c          do jz=1,jz2
c             CALL rddisc( lunslice, scr, Nx, istat )
c             do jx=1, Nx
c                Zrow(jx,jz) = scr(jx)
c             end do
cc             read(lunslice,rec=irec) (Zrow(jx,jz),jx=1,Nx)
c          end do
c          do ix=1,Nx
c             do jz=1,jz2
c                irec=iz+jz-1
c                r(irec)=Zrow(ix,jz)
c             end do
c             CALL WRDISC( lundata, r, jz2 )
c          end do
c       end do
c
 999   RETURN
       end
c
c --------------------------------------------------------------------
c
       subroutine powtwo(N,N2)

       ri=0.
       N2=0.
       do while(N2 .eq. 0.)
          ri=ri+1.
          ir1=int(2.**ri)
          ir2=int(2.**(ri+1.))
          if(N .gt. ir1 .and. N .le. ir2) N2 = ir2
       end do
       return
       end
c
c---------------------------------------------------------------------
c
      subroutine get_kx2(kx2,Nkx,dkx)

      real*4 kx2(Nkx),dkx

      n = Nkx/2+1
      do i= 1, n
         kx2(i) = (dkx*float(i-1))**2.
      end do
      do i= n+1, Nkx
         kx2(i) = (dkx*float(Nkx-i+1))**2.
      end do
      return
      end

c
c--------------------------------------------------------------------
c
      subroutine get_taper(Nx,nspx,ctap)

      real*4 ctap(Nx),xscal

      rtap = float(nspx)
      xscal =  1./float(Nx)

      do i=1,Nx
         ctap(i)=xscal
      end do

      IF( nspx .LE. 1 ) RETURN
      do i=1,nspx
         r=float(i)
         arg = -1.*(0.08*(rtap-r))**2.
         ctap(i)      = xscal*exp(arg)
         ctap(Nx+1-i) = xscal*exp(arg)
      end do

c
      return
      end
c
c
c


