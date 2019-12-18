      SUBROUTINE resaex( buf, lbuf, ibuf, iscr, scr )
c
c  ARGUMENTS:
c  buf   - The trace, with SEGY header as TYPE REAL
c  lbuf  - The trace, with SEGY header as TYPE INTEGER*4
c  ibuf  - The trace, with SEGY header as TYPE INTEGER*2
c  scr   - A scratch array.
c
c  COPYRIGHT (C) The Regents of the University of California
c  ALL RIGHTS RESERVED.  
c  Written by Paul Henkart, SIO, 16 October 1991
c  mod 12 June 95 - SEG-Y trace header value for sample rate was wrong!
c  mod 25 Jan. 96 - Do it in the "ap" if that's where the data is!
c  mod 1 Jun 05 - Add warning if decimating by 2 or 4.
c  mod 18 Jul 08 - Use nsamps from numdat rather than segy because of 16 bit
c  mod 19 May 11 - Didn't work when data was in ap
c
      DIMENSION buf(1), lbuf(1), ibuf(1), iscr(1), xa(10), scr(1)
      INTEGER*2 ibuf
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /apmem/ apdata(32767)
      COMMON /transp/ x(32000)                                          ! the transpose array!
      COMPLEX x
      COMMON /resamp/ dtout, lprint, type, order
      INTEGER type, order
      LOGICAL first
      SAVE
      DATA first/.TRUE./
c
c
      nin = ibuf(isampptr)
      dtin = buf(lsisptr)                                               ! sample interval in seconds
      IF( first ) THEN
          first = .FALSE.
          IF( dtout / dtin .EQ. 2 .OR. dtout / dtin .EQ. 4 ) THEN
              PRINT *,' ***   WARNING   ***  DISKIN/DISKOX can be used 
     &with DECIMF ',INT(dtout / dtin)
          ENDIF
          IF( dtin .EQ. dtout ) THEN
              PRINT *,' ***  WARNING  ***  Trace not resampled.'
              PRINT *,' Input sample interval equals output interval',
     &                dtin
              RETURN
          ENDIF
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
         PRINT *,' numdat=',numdat,' dtin=',dtin,' dtout=',dtout
      ENDIF
      IF( type .EQ. 2 ) GOTO 1000
c****
c****   DO OSU   IMSL  frequency domain interpolation here
c****
c first make sure input has an even number of sample
c
c        nin = nin / 2
c        nin = nin * 2
c
c now calculate output parameters
c
c        nout=(nin)*(dtin/dtout)
c        nout=nout/2
c        nout=nout*2
c
c now recalculate dtout to allow for truncation
c
c        dtout=(nin)*dtin/(nout)
c      IF( ibuf(itridptr) .EQ. 2 ) GOTO 1200                             ! forget dead traces
c        fnyquistin=1.e0/(2.e0*dtin)
c        fnyquistout=1.e0/(2.e0*dtout)
c
c give some printout
c        IF( IAND(lprint,2) .NE. 0 ) THEN
c            write(*,*)'Input  nyquist',nin,dtin,fnyquistin
c            write(*,*)'Output nyquist',nout,dtout,fnyquistout
c        ENDIF
c
c do fft with IMSL routine
c        IF( iuseap .NE. 0 .AND. in .NE. 0 ) THEN
c            call FFTRC (apdata(in),nin,X,IWORK,WORK)
c        ELSE
c            call FFTRC (buf(numhdr+1),nin,X,IWORK,WORK)
c        ENDIF
c
c as a check on what your doing check on power at nyquist
c
c        nd2=nout/2
c        power=0
c        do 100 j=1,nd2
c  100    power=power+abs(X(j))**2
c        power=power/nd2
c        powern=abs(X(nd2))*2
c        IF( IAND(lprint,2) .NE. 0 ) THEN
c            write(*,*)' Average power of input series ',power
c            write(*,*)' power at nyquist ',powern
c        ENDIF
c
c now add in extra points if necessary
c
c        if(nout.gt.nin) then
c         ni2=nin/2
c         do 110 j=ni2+1,nd2
c  110      x(j)=(0.0e0,0.0e0)
c        endif
c
c now fill up last half of time series with complex conjugate of first
c
c        do 120 j=2,nd2
c  120     x(nout+2-j)=conjg(x(j))
c
c now do inverse transform by taking conjugate and doing fft
c         do 130 j=1,nout
c  130     x(j)=conjg(x(j))
c         call FFTCC(X,nout,IWORK,WORK)
c         do 140 j=1,nout
c          X(j)=conjg(X(j))/nin
c          buf(numhdr+j)=real(x(j))
c  140    continue 
c         GOTO 1200
c****
c****   Do  time domain interpolation here
c****
 1000 CONTINUE
      nsamps = numdat
      si = buf(lsisptr)
      index = 1
      nout = nsamps * si / dtout - order
      DO 1050 i = 1, nout
         DO 1010 j = index, index+order-1
 1010    xa(j-index+1) = FLOAT(index+(j-index)-1) * si
         xx =  FLOAT(i-1) * dtout
         IF( IAND(lprint,2) .NE. 0 )
     &    PRINT *,' nout=',nout,' si=',si,' xx=',xx,' order =',order
         IF( in .NE. 0 ) THEN
             CALL polint( xa, apdata(in+index-1), order, xx, scr(i), dy)
         ELSE
             CALL polint( xa, buf(numhdr+index), order, xx, scr(i), dy )
         ENDIF
c      print *,' index=',index,' i=',i,FLOAT(index+1)*si,FLOAT(i+1)*dtout
 1020    IF( FLOAT(index)*si .LE. FLOAT(i)*dtout ) THEN
             index = index + 1
             IF( index .GT. nsamps ) GOTO 1050
             GOTO 1020
         ENDIF
 1050 CONTINUE
      IF( in .NE. 0 ) THEN
          DO  i = 1, nout
              apdata(in+i-1) = scr(i)
          ENDDO
      ELSE
          DO  i = 1, nout
             buf(numhdr+i) = scr(i)
          ENDDO
      ENDIF
 1100 CONTINUE
 1200 ibuf(isampptr) = nout
      CALL long2ushort( nout, ibuf(isampptr) )
      numdat = nout
      buf(lsisptr) = dtout
      ibuf(isiptr) = dtout*1000000.
      RETURN
c****
c****
      END
