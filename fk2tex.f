      subroutine FK2TEX(ibuf,buf,lbuf,iscr,lscr, scr, istop1, nready)
c-------------------------------------------------------------------------------
c    FK2TEX is the execution module of SIOSEIS for performing the inverse
c  FK transformation.  i.e. FK2TEX transforms the FK domain back into the tx
c  domain.
c
c     I blast the AP simulator common array after the last trace is input to
c  FK2TEX. The assumption is that any process that may be saving something
c  in the simulated memory will not be called again and any post FK2TX process
c  has not been called yet!  The simulator memory is used for transposing the
c  complex matrix, which might be very very large.  (1000 traces in a seismic
c  line is a short line!  Marine shots are typically 1500 samples long. There-
c  fore,  The seismic line or matrix, contains 1024*2048 (2,095,152) samples
c  (8,388,608 bytes!).  I also ran into a virtual addressing problem with
c  large arrays in SIOSEIS because SIOSEIS is not a small program!.
c
c  Inputs:
c   ibuf/buf/lbuf - Equivalenced array holding
c   iscr/scr      - Scratch Array
c
c   istop  - the stop signal.
c          = STOPNOTR (-1), The last call to fk2tex & buf does not have a trace.
c          = NOSTOP   ( 0), This is not the last call. a trace is in buf.
c          = STOPTR   ( 1), This is the last call & buf has a trace.
c
c  Outputs:
c
c   nready - The number of traces ready for the next processes. Since FK2TX
c            needs all the traces before it can process any, nready is 0
c            until ISTOP<>0, and then nready becomes the number of traces
c            output by FK2TX after 2-D transformation.
c
c  Call From:
c      CONTRO:FK2TEX
c
c  Externals:
c    GETFIL, FREFIL, PODISC, WRDISC, RDDISC : Disk I/O routines
c    EXIT, INAP, RLSEAP
c  Non-AP version:
c    FFTFWD. gnrfft
c    RECTC
c  AP version:
c    APINIT, APRLSE
c    RECT
c    VMOV, CFFT, CVNEG
c
c  Known Limitations:
c     The AP versio has not been tested & doesn't work because of the absence
c   of a Real FFT call
c
c  Modifications
c     10/11/88 ajh.   ( Added DSKSTRT to jstarti/jstarto)
c     12/20/88 ajh    ( Shift POLAR data in AP prior to conversion)
c   3/14/89 ajh Changed external names to 6 characters. Add space as first
c               character of PRINT statements
c               Passed binary Header values through READT
c  Apr 96 - Change txprestk from logical to integer counter
c         - Add unsorting of prestk data
c 27 May 96 - Pre stack with pads didn't work.
c 22 Nov 96 - Prestk didn't work when user gave the header file name
c 21 Dec 99 - Increase character*6 ctemp to character*20 ctemp
c 24 Jul 01 - Change SZAPMEM to 5000000
c 13 Jul 02 - Prevent negative disk address on first time after all traces have been read.
c 5 Feb 03 - ajh - add an additional stage and scratch file during
c            transformation but ensures that all writes to disk are
c             contiguous and sequential.
c 19 May 05 - Allow 32768 samples
c 31 Aug 05 - Add parameter KILL.  Zero dead traces when KILL YES.  Reset the
c             seismic trace identifier (word 15 - the dea trace flag) when KILL NO.
c 5 Jul 07 - Unsort the data only if prestk > 1 (match tx2fk)
c 15 Aug 07 - g95 doesn't do internal write to get ihdrpath
c 2 Oct 07 - That mod was botched.  rddisc needs istat!
c 2 Aug 08 - call getfil for a temp file didn't have space for the return name
c 22 Sep 08 - prestk of 1 no longer worked (Apr 96 change ?)
c           - change SZAPMEM to 10MW
c 26 Sep 08 - Use common numdat rather than segy header for nsamps
c 8 Apr 09 - Above wasn't done in all cases.
c 3 June 09 - Still not right.  Make the output nsamps the same as the original tx.
c-------------------------------------------------------------------------------
c
C INCLUDE FILES
c INCLUDE contro.inc
c.. Include file defining constants of use globally throughout SIOSEIS
c
c Last Modified 11/11/88
c    added DATINAP
c Define the sizes of some Large common blocks

      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 10000000)                                     ! Size of /APMEM/ : AP memory
      parameter (SZTRANP = 262144)                                      ! Size of /TRANP/: Transpose array at 512x512
C parameter definitions for the stop signal istop
      integer STOPNOTR, STOPTR, NOSTOP
      parameter (STOPNOTR  = -1)                                        ! last call but no trace passed
      parameter (STOPTR    =  1)                                        ! last call and a trace passed
      parameter (NOSTOP    =  0)                                        ! normal call to the routine
C
C parameter for AP
      integer CLEARAP, USEAP, NOAP
      integer DATINAP
      parameter (USEAP    = 1)                                          ! There is an AP available
      parameter (NOAP     = 0)                                          ! No AP available
      parameter (CLEARAP  = 0)                                          ! Get data out of AP simulator
      parameter (DATINAP   = 1)                                         ! Data is in the AP
c
c Parameters for coord type
      integer CDRECT, CDPOLAR, CDPOLARU
      parameter ( CDRECT   = 1)
      parameter ( CDPOLAR  = 2)
      parameter ( CDPOLARU = 3)
C
C parameters for Windows
      integer BoxCar
      parameter (BoxCar = 5)
c
      logical txinit, fkinit, txed, fked, tmptxhdr
      integer ltxunt1, ltxunt2, lfkunt1, lfkunt2, txunit, ohdrtxfk,
     &        txprestk
      common /TXFKE/ txinit, fkinit, txed, fked, txunit, ltxunt1,
     &               ltxunt2, lfkunt1, lfkunt2, ohdrtxfk, tmptxhdr,
     &               txprestk, ntx2fk, nfk2tx, range
      integer     CRAYEBC, NORMEBC
      parameter ( CRAYEBC  = 400)
      parameter ( NORMEBC  = 2*CRAYEBC)
c
c BINARY HEADER
      integer     CRAYBIN, NORMBIN
      parameter ( CRAYBIN  =  50)                                       ! length of external disk image
      parameter ( NORMBIN  = 2*CRAYBIN)
c
c  The position of objects in the Binary header
      integer    NTRCPTR,IFMTPTR, IDTYPPTR, INKPTR, ITSIPTR
      integer    ITDELPTR, IVERSPTR
      parameter (NTRCPTR  =  7)                                         ! No of traces / gather
      parameter (IFMTPTR  = 13)                                         ! The SEGY Format of the data
      parameter (IDTYPPTR = 31)                                         ! The Domain/ID of the data e.q. T-X data
      parameter (INKPTR   = 32)                                         ! The No. of wavenumbers in an f-k dataset
      parameter (ITSIPTR  = 33)                                         ! Record of tx sample interval in us
      parameter (ITDELPTR = 34)                                         ! Record of tx time delay in ms.
      parameter (IVERSPTR = 35)                                         ! The SIOSEIS version number
c
c..Constants for SEGY Format (IFMTPTR)
      integer IBMFP, INT16, INT32, HOSTFP
      parameter (IBMFP  = 1)                                            ! IBM floating point
      parameter (INT32  = 2)
      parameter (INT16  = 3)
      parameter (HOSTFP = 5)                                            ! Host FP. In actuality Host FP >= 5
c
c.. Data Domains/Types (IDTYPPTR)
      integer    IDTX, IDFKRCT, IDFKPLR, IDFKPLRU
      parameter (IDTX     = 1)
      parameter (IDFKRCT  = 2)
      parameter (IDFKPLR  = 3)
      parameter (IDFKPLRU = 8)
c
c.. Version Number
      integer  CURVERS
      parameter( CURVERS = 210)
c
c TRACE HEADER
c.. Declare header length on external disk files. Internal length is given by
c Numhdr in common block READT.
      integer  CRAYTHDR, NORMTHDR
      parameter ( CRAYTHDR =  30)                                       ! Length of external disk image
      parameter ( NORMTHDR = 2*CRAYTHDR)
c
c    The positions of elements of the trace header are defined by elements of
c the common block SEGYPTR. This block is initialized in routine SETPTR. The
c position of elements differs on the CRAY since it does not allow mixing of
c Integer*16 & Integer*32 words but only has Integer*64.
c
      integer LLSEQPTR                                                  ! Trace Sequence No. within line
      integer LRSEQPTR                                                  ! Trace Sequence No. within reel
      integer LSHOTPTR                                                  ! Shot number or Stacked Number
      integer LSHTRPTR                                                  ! Trace number within Shot
      integer LRPNPTR                                                   ! RP or CDP number
      integer LRPTRPTR                                                  ! Trace No. within CDP
      integer ITRIDPTR                                                  ! Trace ID Live(1)/Dead(2)
      integer LDISTPTR                                                  ! Source to Receiver Distance
      integer LWBDPTR                                                   ! Water Bottom depth at source
      integer LSXCOPTR                                                  ! Source X co-ordinate
      integer LRXCOPTR                                                  ! Receiver X co-ordingate
      integer IDELMPTR                                                  ! Deep water delay in ms.
      integer ISTMPTR                                                   ! Start Mute time in ms.
      integer IENDMPTR                                                  !   End Mute time in ms
      integer ISAMPPTR                                                  ! Number of data samples in trace
      integer ISIPTR                                                    ! Sample interval of trace in us
      integer IYRPTR                                                    ! Year data was recorded
      integer IDAYPTR                                                   ! Day of year
      integer IHRPTR                                                    ! Hour of day
      integer IMINPTR                                                   ! Minute of hour
      integer ISECPTR                                                   ! Second of Minute
      integer IGMTPTR                                                   ! Time zone of header Local(1)/GMT(2)
      integer LDELSPTR                                                  ! Deep water delay in secs.
      integer LSMUSPTR                                                  ! Start of mute in seconds
      integer LEMUSPTR                                                  ! End of mute in seconds
      integer LSISPTR                                                   ! Sample interval in seconds
      integer LWBTSPTR                                                  ! Water bottom time in seconds
      integer LGATPTR                                                   ! No. of traces in stacked data(>0) or End of Gather(<0)
      integer LSSMSPTR                                                  ! Start of surgical mute in secs.
      integer LESMSPTR                                                  ! End of surgical mute in secs
      integer LSBPTR                                                    ! SeaBeam slant range
c
      common /SEGYPTR/ LLSEQPTR, LRSEQPTR, LSHOTPTR, LSHTRPTR, LRPNPTR,
     *                 LRPTRPTR, ITRIDPTR, LDISTPTR, LWBDPTR,  LSXCOPTR,
     *                 LRXCOPTR, IDELMPTR, ISTMPTR,  IENDMPTR, ISAMPPTR,
     *                 ISIPTR,   IYRPTR,   IDAYPTR,  IHRPTR,   IMINPTR,
     *                 ISECPTR,  IGMTPTR,  LDELSPTR, LSMUSPTR, LEMUSPTR,
     *                 LSISPTR,  LWBTSPTR, LGATPTR,  LSSMSPTR, LESMSPTR,
     *                 LSBPTR
C
c.. Define Constants for Trace Header
c
c.. Constants for Trace ID (ITRIDPTR)
      integer    LIVETR, DEADTR                                         ! Parameters for trace Type
      parameter (LIVETR  = 1)
      parameter (DEADTR  = 2)
C
c.. Define constants for use with the DISKIO subroutines getfil, frefil,
c podisc
c GETFIL constants
      integer    CREATTMP, CREATNEW, OPENOLD
      parameter (CREATTMP   = 1)                                        ! Create a temporary file
      parameter (CREATNEW   = 3)                                        ! Create a new named file
      parameter (OPENOLD    = 4)                                        ! Open an existing file
c FREFIL constants
      integer    RLDLCLS, RLCLS
      parameter (RLCLS   = 2)                                           ! Release and Close the file
      parameter (RLDLCLS = 3)                                           ! Release, delete and close
c PODISC constants
      integer    POSABS, POSREL, DSKSTRT
      parameter (POSABS     = 1)
      parameter (POSREL     = 2)
      parameter (DSKSTRT    = 0)                                        ! The starting position on the disk
C PROGRAM
c
      dimension buf(111),scr(111)
      integer*2 ibuf(111), iscr(111)
      integer   lbuf(111), lscr(111)
      integer   istop, nready
c
      parameter (MAXMEM = SZTRANP)                                      ! The transpose buffer size
      logical    NONORM, INVERSE
      parameter (INVERSE = .false., NONORM = .false.)
      integer    YES, NO
      parameter ( YES = 1, NO = 0)
      real   TXNORM
      parameter (TXNORM = 2.0)                                          ! Additional factor in tx normalization. This is
c                                                                       ! 2.0 for supplied routines
c
      integer parmunit, nwrds
      common /FK2TXC/ parmunit, nwrds                                   ! Pass parameters to execution phase.
c
      common /TRANSP/t(SZTRANP)
      INTEGER lt(1)
      EQUIVALENCE (t(1),lt(1))
      common /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap
      common /APMEM/ ap(SZAPMEM)
c
      integer origntr
      common /READT/ ilun,numhdr,numdat,iunhdr,ireelm,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, origntr
      common /WRITET/ idum(5),ntrcs
c
      character*200 ctemp
      character*200 ihdrpath
      integer*2   txsius, msdelay
      integer     tpow, xpow, pow2
      integer     opad, lprint, kill
      integer     dskhdr1, dskhdr2, nspace1, nspace2
      integer     noutput
      logical     first, byrpnum
c
      save
c
      data first /.true./
      data lfor/1/, itrcno/1/
c
      istop = istop1
      nready = 0
      nfk2tx = nfk2tx + 1
c
c***********************                                ***********************
c                           Initialize the routine
c***********************                                ***********************
c
      if (first) then                                                   ! Initialize the routine
        first = .false.
c
        if ( fked) then
          call podisc(parmunit, POSABS, DSKSTRT)                        ! Read parameters from disk
          CALL rddisc( parmunit, ihdrpath, 25, istat )
          call rddisc(parmunit, lscr(26), nwrds-25, istat)
c          write(ihdrpath, '(25A4)') (lscr(i),i = 1, 25)
          opad   = lscr(26)
          lprint = lscr(27)
          kill = lscr(28)
        else
          ihdrpath = ' '
          opad     = NO
          lprint   = 0
          kill = 0
        endif
c
        if ( IAND(lprint,1).ne.0 ) then
          PRINT '(/A)',' FK2TEX Initialization Info'
          PRINT *, ' File Streams txunit: ', txunit,
     $              ' lfkunt1: ', lfkunt1,' lfkunt2: ',lfkunt2
          PRINT *,' ihdrpath = ',ihdrpath,' opad=',opad
        endif
        fkinit = .true.
c
c..                      Open 1st scratch file to accumulate input traces
        if (txinit) then
          ifunit = txunit                                               ! Use current scratch file if tx2fex was called
        else
          if ( (lfkunt1.ne.0).and.fked) then
            ifunit = lfkunt1
          else
            call getfil(CREATTMP,ifunit,ctemp,istat)                    ! Or get a new file
            if (istat.ne.0) then
              PRINT *,' *** FK2TX ERROR ***',
     $                     ' Could not open 1st Scratch file'
              STOP
            endif
          endif
        endif
c
c..                      Open 2nd scratch file to output traces after 2D FFT
        if ( fked.and.(lfkunt2.ne.0) ) then
          ixunit = lfkunt2
        else
          call getfil(CREATTMP, ixunit, ctemp, istat)                   ! File to hold the output
          if (istat.ne.0) then
            PRINT *,' *** FK2TX ERROR *** ',
     $                     ' Could not open 2nd Scratch file'
            STOP
          endif
        endif
c****
c****  If the user gave the header file name use it.
c****  If the user didn't give it in fk2tx but gave it in tx2fk, use
c****     the one from tx2fk.
c****  If the user didn't give it either place, use the one tx2fk used.
c****
        if (ihdrpath(1:10) .ne. ' ') then                               ! User gave header filename
           if  (ohdrtxfk.ne.0) then
               if (tmptxhdr) then
                  call frefil(RLDLCLS, ohdrtxfk, istat)
               else
                  call frefil(RLCLS, ohdrtxfk, istat)                   ! To avoid conflict
               endif
               ohdrtxfk = 0                                             ! close TX header file
           endif
           call getfil(OPENOLD, ihdrfktx, ihdrpath, istat)
           if (istat .ne. 0) then
              PRINT *,' *** FK2TX *** Could not open header file ',
     $           ihdrpath
              STOP
           endif
           tmptxhdr = .FALSE.
        else if (ohdrtxfk.ne.0) then                                    ! Use header file created by TXFK
           ihdrfktx = ohdrtxfk
        else
           byrpnum  = ( lbuf(LRPTRPTR) .ne. 0 )                          ! Form of trace indexing
           ihdrfktx = 0
        endif
c
c        inlen = ibuf(ISAMPPTR)                                          ! Length of the input trace
c        IF( inlen .LT. 0 ) inlen = lbuf(29)
        inlen = numdat
        nt    = inlen/2                                                 ! Length of the complex time series
        tpow  = pow2(nt)
        dskhdr1 = 0                                                     ! length of headers on 1st scratch
        dskhdr2 = 0                                                     ! length of headers on 2nd scratch
        nspace1 = dskhdr1 + inlen                                       ! Total trace length on disk
        nspace2 = dskhdr2 + nt
c
        if (inlen.ne.2**(tpow+1)) then
          PRINT 2000,' The length of the input FK data must be 2**N + 1'
          PRINT *, ' Input data length is ',inlen
          STOP
        endif
c
        if ((idtype.ne. IDFKRCT).AND.(idtype.ne.IDFKPLR)
     *                          .AND.(idtype.ne.IDFKPLRU) ) then
          PRINT 2000,' The input data must be in the FK domain.'
          PRINT *,   ' ID type of data is ',idtype
          STOP
        endif
      endif                                                             ! of fk2tex initialization
      IF( nfk2tx .EQ. 1 ) THEN
c          call podisc(ifunit, POSABS, DSKSTRT)
c****     the next use of ifunit is relative position -nspace1, which is a negative address!
          call podisc(ifunit, POSABS, nspace1 )
          call podisc(ixunit,POSABS,DSKSTRT)
          call podisc(ihdrfktx, POSABS, DSKSTRT)
      ENDIF
c
      if( istop.eq.STOPNOTR ) go to 500                                 ! Start doing 2-D transformation
c
c***********************                                ***********************
c*            Accumulate Traces & transform them from f to t
c***********************                                ***********************
c
c      nsamps = ibuf(ISAMPPTR)
c      IF( nsamps .EQ. -32768 ) nsamps = 32768
      nsamps = numdat
      if ( inlen.ne.nsamps ) then
          PRINT 2000
          PRINT *, ' Input number of samples for trace ', nfk2tx,' is ',
     *              inlen,' not the expected ',nsamps
          STOP
      endif
      call inap(buf(numhdr+1),nsamps)                                   ! Put everything in the ap
c
c.. If the FK domain is in polar coordinates, convert to rectangular coords
c.. On the forward FFT every other complex sample was negated to get
c.. -nyq -> 0 -> nyq ordering in frequency. Must undo it here
c
      if ( iuseap.eq. NOAP) then
          if( (idtype.eq.IDFKPLR).OR.(idtype.eq.IDFKPLRU) ) then
            do 190 i = 1, nt+nt                                         ! Shift data prior to conversion
  190        ap(nextad+i-1) = ap(in-1+i)
c            PRINT *,'RECTC in, nextad, nt',in, nextad, nt
c            PRINT *, 'ap(in): ', ap(in), 'ap(in+nt):',ap(in+nt)
c            PRINT *,'ap(nextad) (mod):',ap(nextad)
c            PRINT *,'ap(nextad+nt) (phase):',ap(nextad+nt)
            call rectc(nt,ap(in),ap(nextad),ap(nextad+nt))
          endif
c          call fftinv(ap(in),tpow)
          call fftfwd(ap(in),tpow)                                      ! FFT to time
          do 200 i = 1,nsamps,4
              ap(in+i-1) = -ap(in+i-1)
              ap(in+i)   = -ap(in+i)
  200     continue
      else
          if ( (idtype.eq.IDFKPLR).OR.(idtype.eq.IDFKPLRU) ) then
              call vmov(in,1,nextad,2,ncomps)
              call vmov(in+nsamps/2,1,nextad+1,2,ncomps)
              call rect(nextad,2,in,2,ncomps)
          endif
          call cfft(in, ncomps, -1)
          call cvneg(1,4,1,4,nsamps/4)
      endif
c
      iout = 0                                                          ! Force rlseap to get the data out of the ap
      call rlseap(buf(numhdr+1), nsamps)
c
      if(txinit)                                                        ! Write over untransformed trace if it exists
     $  call podisc(ifunit, POSREL, -nspace1 )
      call wrdisc(ifunit, buf(numhdr-dskhdr1+1), nspace1)
      IF( ntx2fk .EQ. 0 .AND. txprestk .GE. 1 ) istop = 1
c****  NOSTOP is 0
      if ( istop .eq. NOSTOP) RETURN
c
c***********************                                ***********************
c*      The data on disk has been transform from f to t, now do k to x.
c***********************                                ***********************
c
c.. There should be 2** nxo2 + 1 traces in the input dataset. spanning
c.. from 0 -> k (nyq). On output there will be nx = 2*nxo2 ranges
c
  500 continue
c
      nxo2 = nfk2tx -1
      xpow = pow2(nxo2)
      nx   = 2 * nxo2
      if (nxo2.ne.2**xpow) then
        PRINT 2000,' The entire set of FK traces must be input.'
        PRINT *,' Expecting ',2**xpow+1,' Traces.',
     *                     ' Instead received ', nxo2+1
        STOP
      endif
c
c.. Must pack the zero and nyquist traces into the zero wavenumber trace
      call podisc(ifunit, POSABS, DSKSTRT)                              ! Read zero wavenumber data
      call rddisc(ifunit, scr, nspace1, istat)
      istart = dskhdr1 + DSKSTRT + nxo2 * nspace1
      call podisc(ifunit, POSABS, istart)                               ! Read in nyquist data
      call rddisc(ifunit, t, nsamps, istat)
      do 510 i = 1, nsamps, 2
  510   scr(dskhdr1+i+1) = t(i)
      call podisc(ifunit, POSABS, DSKSTRT)
      call wrdisc(ifunit, scr, nspace1)


c..                           Create temporary file to hold partial time traces
c      call getfil( CREATTMP, igunit, name, istat)
      call getfil( CREATTMP, igunit, ctemp, istat)
      if (istat.ne.0) then
        PRINT 2000, ' Could not open temp file'
        STOP
      endif

      nsizec   = MAXMEM / nxo2                                           ! Find the no. of k traces that fit in t()
      if ( nsizec .gt. nsamps ) nsizec = nsamps
      ntimes  = nsamps / nsizec                                          ! This is the number of buffers per trace
      if (nsizec*ntimes.ne.nsamps) then
        PRINT 2000, ' Non zero remainder in k->x transform'
        STOP
      endif

      jstarti = dskhdr1 + DSKSTRT
      nsize   = nsizec
      nsizeo  = nsizec / 2

      do 1100 i = 1, ntimes
          istart = jstarti

          do 700 j = 1, nxo2
              call podisc(ifunit,POSABS,istart)                         ! Read part of the trace
              call rddisc(ifunit,scr,nsize,istat)
              indx = j*2 - 1
              do 600 k = 1, nsize, 2
                  t(indx)   = scr(k)
                  t(indx+1) = scr(k+1)
                  indx = indx + nx
  600         continue
              istart = istart + nspace1
  700     continue

          if (iuseap .eq. NOAP ) then
            indx = 1

            do 800 j = 1, nsizeo
              call gnrfft(t(indx), nx, inverse, nonorm)
              indx = indx + nx
  800       continue
          else
            PRINT 2000, ' AP FFT not installed'
            STOP
          endif

c..                             Gather the data back into partial time traces
          do 1000 j = 1, nx
              indx = j
              do 900 k = 1, nsizeo
                  scr(k) = t(indx)
                  indx   = indx + nx
  900         continue
              call wrdisc(igunit,scr,nsizeo)
 1000     continue

          jstarti = jstarti + nsize
 1100 continue


c...                            Assemble the partial trace into full traces
       nsizei  = nsizec/2
       nslice  = nsizei*nx
       jstarti = DSKSTRT - nsizei

       call podisc(ixunit,POSABS,DSKSTRT)

       do 1103 i = 1, nspace2
         scr(i) = 0.
 1103  continue
       do 1102 j = 1, nx
         indx   = dskhdr2 + 1
         nsizei = nsizec/2
         jstarti= jstarti + nsizei

         istart = jstarti

         do 1101 i = 1, ntimes
           call podisc(igunit,POSABS,istart)
           call rddisc(igunit,scr(indx),nsizei,istat)
           indx   = indx   + nsizei
           istart = istart + nslice
 1101    continue

         call wrdisc(ixunit,scr,nspace2)
 1102  continue

       call frefil(RLDLCLS, igunit,istat)                                ! & release the first scratch file


c
c..              Pass back the number of output traces
      if (opad .eq. YES ) then                                          ! Retain the padding traces and times
        nready = nx
      else
        nready = origntr
      endif
      noutput = nready
      nfk2tx   = 0
      call podisc(ixunit,POSABS,DSKSTRT)                                ! Rewind the tx file
c
c.. Recover & Reset variables in SEGY binary header through \READT\ common block
      txsius       = itxsi                                              ! Recover t-x sample interval in us
      msdelay      = itxdel                                             ! & the start time delay in ms
      delay        = msdelay/ 1000.
      si           = txsius/ 1000000.
      norig        = origntr
c
      itxsi        = 0                                                  ! Reset the parameters in /READT/
      itxdel       = 0
      idtype       = IDTX
      nfktrc       = 0
      origntr      = 0
c
c****
c****    If the data was prestack, get the original trace order which
c****  tx2fk appended to the header file.  Resort it and save it in
c****  the t array (transform) and hope that nobody blows it away.
c****

      IF( txprestk .GT. 1 ) THEN
          CALL podisc( ihdrfktx, POSABS, norig*numhdr )
          CALL rddisc( ihdrfktx, lscr, norig, istat )
          IF( istat .NE. norig ) THEN
              PRINT *,' ***  HELP  ***   fk2tx missing prestk sort.',
     &               norig,istat
              STOP
          ENDIF
          DO i = 1, norig
             scr(i) = lscr(i)
          ENDDO
          CALL indexx( norig, scr, lt )
          CALL podisc( ihdrfktx, POSABS, DSKSTRT )
      ENDIF
c
      RETURN
c
c************************                               ************************
c*         Get the complex data from disk and convert to real data.
c************************                               ************************
c
      ENTRY GETNXX(buf,lbuf,ibuf)
      nfk2tx  = nfk2tx + 1
      IF( txprestk .GT. 1 ) THEN
          CALL podisc( ixunit, POSABS, (lt(nfk2tx)-1)*nspace2 )
      ENDIF
      call rddisc(ixunit, buf(numhdr-dskhdr2+1), nspace2, istat)
      in     = 0                                                        ! Force inap to put the data in the "ap"
      scalar = TXNORM / ( nx * nt )
      do 1200 i = 1, nt                                                 ! Normalize the output trace
        buf(numhdr+i) = buf(numhdr+i) * scalar
 1200 continue
c
c..
      if ( ihdrfktx .ne. 0) then
        if (nfk2tx.gt.norig)
     $    call podisc(ihdrfktx, POSREL, -numhdr)
        call rddisc(ihdrfktx, buf, numhdr, istat)                       ! Read in stored trace header
c
      else                                                              ! Manufacture a trace header
         if ( byrpnum) then
           lbuf(LRPNPTR)  = lfor
           lbuf(LRPTRPTR) = itrcno
         else
           lbuf(LSHOTPTR) = lfor
           lbuf(LSHTRPTR) = itrcno
         endif
c
         if ( intrcs.eq.1 ) then                                        ! One trace per "gather"
            lfor   = lfor + 1
         else
            itrcno = itrcno + 1                                         ! Or index by trace no.
         endif
      endif
c
c
c.. No padding adjust times of data
c***  the output tx trace should be the same length as the input tx trace,
c***  so use nsamps from the trace header rather than common (which tx2fk changed)
      if ( (ihdrfktx .ne. 0) .and. (OPAD.eq.NO) ) then
           nshift = nint( buf(LDELSPTR) - delay) / si
c
c           long_nsamps = ibuf(ISAMPPTR)
c           IF( long_nsamps .EQ. -32768 ) long_nsamps = 32768
           CALL ushort2long( ibuf(ISAMPPTR), long_nsamps )
           if( nshift .gt. 0 ) then                                     ! Original start > delay
              ibuf(ISAMPPTR) = min ( nt - nshift, long_nsamps )
              numdat = min ( nt - nshift, long_nsamps )
              do 1250 i = numhdr + 1, numhdr + long_nsamps
                 buf(i) = buf(nshift + i)
 1250         continue
          else                                                          ! Increase start time to delay
              buf(LDELSPTR) = delay
              ibuf(IDELMPTR) = msdelay
              ibuf(ISAMPPTR) = min ( long_nsamps + nshift, nt)
              numdat = min ( long_nsamps + nshift, nt)
          endif
      else
          ibuf(IDELMPTR) = msdelay                                      ! Record start delay in ms
          buf(LDELSPTR) = delay                                         ! & in Secs
          ibuf(  ISIPTR) = txsius                                       ! Record sample interval in us
          buf( LSISPTR) = si                                            ! & in secs
          ibuf(ISAMPPTR) = nt                                           ! length of output in 4 byte words
          numdat = nt
      endif
c
      idtype = IDTX                                                     ! Set the idtype in /READT/
c
c**** dead traces get energy from adjacent traces.  FK filtered data probably
c**** should be rezeroed and the dead traces honored.  FK migrated dead traces
c**** are no longer dead.
c****
      IF( ibuf(ITRIDPTR) .EQ. 2 .AND. kill .EQ. 1 ) THEN
          DO i = 0, numdat-1
             buf(numdat+i) = 0.
          ENDDO
      ELSE
          ibuf(ITRIDPTR) = LIVETR
      ENDIF
      ibuf(ISTMPTR)  = 0                                                ! Zero all mute information
      ibuf(IENDMPTR) = 0
c      buf(LSMUSPTR) = 0.
c      buf(LEMUSPTR) = 0.
c      buf(LSSMSPTR) = 0.
c      buf(LESMSPTR) = 0.
c      if (nfk2tx .eq. noutput) then
c        call frefil(RLDLCLS, ixunit, istat)
c        if (tmptxhdr) then
c          call frefil(RLDLCLS, ihdrfktx, istat)
c        else
c          call frefil(RLCLS, ihdrfktx, istat)
c        endif
c      endif
      if (lprint.ne.0) PRINT *,' Exiting GETNXX nfk2tx =', nfk2tx
      RETURN
 2000 format (' ***  FK2TEX error  ***',(A))
      end
