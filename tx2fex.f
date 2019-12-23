      Subroutine TX2FEX(ibuf,lbuf,buf,scr,iscr,lscr,istop,nready)
C-------------------------------------------------------------------------------
C    TX2FEX is the execution module of sioseis for process tx2fk, which
C transforms a tx domain data set to a fk domain data set.
C
C     The output of this process is a set of traces in the horizontal
C  wavenumber that run from 0 up to the Nyquist wavenumber. Each wavenumber
C  trace has frequencies that go from -Nyquist to 0 -> Nyquist (N.B. this is
C  different from the usual FFT ordering 0 -> Nyq -> 0).
C
C     The data ordering is the most convenient for passing onto the FK migration
C  routines.  The use of only positive wavenumbers relies on the input data
C  being real time series.
C
C      The number of output traces from TX2FEX is NX/2 + 1 where NX is a power
C  of higher than the number of input traces + zero padding. The length of the
C  output traces is NT complex samples, where NT is a power of 2 greater than
C  the length of the input trace + zero padding. Thus there are 2*NT 4 byte
C  words in an output trace.
C
C     The process is divided into 3 parts.
C
C  (i)    The input traces are accumalated onto a scratch disk file. Each trace
C      is either chopped or padded to a common start and end time, and is
C      windowed at each end if desired.
C
C  (ii) The data is transposed into a series of range traces. At this stage the
C      data is windowed in X and zero padded to NX. The length of the padding
C      is >= NXPAD, the user supplied length of padding. The data is transposed
C      back into a series of complex time traces of at the given wavenumbers.
C
C  (iii)  On each subsequent entry the complex time traces are zero padded and
C      t -> f transformed. They are then passed back to the calling routine
C      in BUF.
C
C  SIGN CONVENTION OF 2D FFT.
C           The Forward FFT in time  is exp(+iwt)
C           The Forward FFT in range is exp(-ikx)
C
C  ARGUMENTS:
C
C  BUF    - The trace to be transformed, including the trace header.  The first
C           data sample must be at time delay (which is passed in the trace
C           header).  This is the floating point (real) trace array.
C  LBUF   - The long integer trace array.  This is really the same as buf, but
C           PRIME FORTRAN doesn't allow equivalencing anything to an argument.
C  IBUF   - The short integer trace array.  needed for 16 bit trace header
C           addresses.
C  SCR    - A scratch array for reading the parameters.  therefore, scr must
C           be at least 56 32bit words big.  Scr may be destroyed by the calling
C           routine.
C  ISTOP  - The stop signal.
C         = StopNoTr  (-1), This is the last call to TX2FEX and buf does
C            not have a trace.
C         = NoStop (0), This is not the last call. A trace is in buf.
C         = StopTr (1), This is the last call.  Buf has a trace.
C  NREADY - The count of traces ready for the next processes.  Since TX2FK
C           needs all the traces before it can process any, nready is 0
C           until ISTOP<>0, and then NREADY becomes the number of traces
C           of the output dataset NX/2 + 1.
C
C Call sequence:
C      CONTRO:TX2FEX
C
C Externals:
C    GETFIL, FREFIL, PODISC, WRDISC, RDDISC
C    INAP, RLSEAP
C    WINDOW, POW2
C    fwindw : Windowing Function
C Non AP version:
C    gnrfft, FFTINV
C    POLARC
C AP version :
C    APINIT, APPUT, APWD, APRLSE, VMOV, VMUL, VCLR
C    CVNEG, CFFT, CFFTSC,
C    POLAR
C
C Known Limitations:
C    The AP version has not been tested & currently cannot work because the
C    absence of a Real FFT call.
C
C Modifications
C    9/15/88 (1) use updated include files to use definitions of trace
C                 position in SEISIO
C            (2) record FK sample interval and start in trace headers.
C            (3) Introduced local recording of coord system & ID type.
C   10/18/88 ajh    ( Added Scratch file paths)
C   11/11/88 ajh Corrected errors when data is in AP already
C    3/14/89 ajh Changed all external names to be 6 characters long
C               Passed Changes to binary header through common block READT
c               and didn't write them to disk
c               Add space as 1st character of PRINT statement
C    6/6/89     Added writing of disk header files
C Last Modified
C    6/7/89     Added pch test for ntx2fk = 0
c    10/29/91   Remove freeing of files, so multiple (prestack) calls works
c    Nov 91   - Add reinitialization for prestack stuff
c    12 Oct 93 pch - change the delay and nsamps in the SEGY header
c                    if we change them when stime/etime are given.
c          IT LOOKS TO ME LIKE WINDOWING IS WRONG WHEN STIME < DELAY
c  19 Nov 93 - ah - make sure trace length remains constant
c  11 May 95 - reininitize window EVERY time ntx2fk is 1 because
c             fk2tex blasts the whole ap!
c  5 Apr 96 - change prestk from switch to a counter
c 16 Apr 97 - pch - don't set nextad = 0 because prestk processes still need it
c 22 Apr 97 - pch - set lastnw (last window length) to zero when done
c             to force a new window to be calculated on prestk!
c 21 Dec 99 - pch - Increase character*6 name to character*20 name
c 24 Jul 01 - Change SZAPMEM to 5000000
c 5 Feb 03 - ajh - add an additional stage and scratch file during
c            transformation but ensures that all writes to disk are
c             contiguous and sequential.
c 19 May 05 - pch - at 1100 use the transpose array rather than the
c                   scratch trace array. (buffer overlow!)
c                 - Don't allow an output trace (complex) be larger than
c                   the max trace length (16 bit integer nsamps).
c 8 Nov 06 - pch - Change twinlen from time to samples because the preset of
c                  .1 seconds is way too long on the high frequency data (chirp).
c 5 Jul 07 - pch - Change prestk sorting by range so that the sorting is done
c                  only when prestk > 1.  This allows shots to be done when
c                  the streamer is negative!.  Process gather sorts by abs(),
c                  so rps are sorted fine.
c 14 Aug 07 - g95 requires DO indices to be integers
c 2 Aug 08 - Ah, but fwindo require real arguments, so make new integer
c            variables for end1 and start2 for the indices.
c 22 Sep 08 - Set lfor = lfor + 1  and ltrcno = 1 when EOG detected.
c           - Increase SZAPMEM to 10MW
c 26 Sep 08 - Use common numdat rather than segy header for nsamps
c 8 Apr 09 - Above wasn't done when stime or etime were given.
c
c Define the sizes of some Large common blocks
      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 10000000)      ! Size of /APMEM/ : AP memory
      parameter (SZTRANP = 262144)       ! Size of /TRANP/: Transpose array at 512x512
C parameter definitions for the stop signal istop
      integer STOPNOTR, STOPTR, NOSTOP
      parameter (STOPNOTR  = -1)         ! last call but no trace passed
      parameter (STOPTR    =  1)         ! last call and a trace passed
      parameter (NOSTOP    =  0)         ! normal call to the routine
C
C parameter for AP
      integer CLEARAP, USEAP, NOAP
      integer DATINAP
      parameter (USEAP    = 1)           ! There is an AP available
      parameter (NOAP     = 0)           ! No AP available
      parameter (CLEARAP  = 0)           ! Get data out of AP simulator
      parameter (DATINAP   = 1)          ! Data is in the AP
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
      integer     CRAYEBC, NORMEBC
      parameter ( CRAYEBC  = 400)
      parameter ( NORMEBC  = 2*CRAYEBC)
c
c BINARY HEADER
      integer     CRAYBIN, NORMBIN
      parameter ( CRAYBIN  =  50)        ! length of external disk image
      parameter ( NORMBIN  = 2*CRAYBIN)
c
c  The position of objects in the Binary header
      integer    NTRCPTR,IFMTPTR, IDTYPPTR, INKPTR, ITSIPTR
      integer    ITDELPTR, IVERSPTR
      parameter (NTRCPTR  =  7)          ! No of traces / gather
      parameter (IFMTPTR  = 13)          ! The SEGY Format of the data
      parameter (IDTYPPTR = 31)          ! The Domain/ID of the data e.q. T-X data
      parameter (INKPTR   = 32)          ! The No. of wavenumbers in an f-k dataset
      parameter (ITSIPTR  = 33)          ! Record of tx sample interval in us
      parameter (ITDELPTR = 34)          ! Record of tx time delay in ms.
      parameter (IVERSPTR = 35)          ! The SIOSEIS version number
c
c..Constants for SEGY Format (IFMTPTR)
      integer IBMFP, INT16, INT32, HOSTFP
      parameter (IBMFP  = 1)             ! IBM floating point
      parameter (INT32  = 2)
      parameter (INT16  = 3)
      parameter (HOSTFP = 5)             ! Host FP. In actuality Host FP >= 5
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
      parameter ( CRAYTHDR =  30)        ! Length of external disk image
      parameter ( NORMTHDR = 2*CRAYTHDR)
c
      common /SEGYPTR/ LLSEQPTR, LRSEQPTR, LSHOTPTR, LSHTRPTR, LRPNPTR,
     *                 LRPTRPTR, ITRIDPTR, LDISTPTR, LWBDPTR,  LSXCOPTR,
     *                 LRXCOPTR, IDELMPTR, ISTMPTR,  IENDMPTR, ISAMPPTR,
     *                 ISIPTR,   IYRPTR,   IDAYPTR,  IHRPTR,   IMINPTR,
     *                 ISECPTR,  IGMTPTR,  LDELSPTR, LSMUSPTR, LEMUSPTR,
     *                 LSISPTR,  LWBTSPTR, LGATPTR,  LSSMSPTR, LESMSPTR,
     *                 LSBPTR
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite, maxsamps,nbperw
C
c.. Define Constants for Trace Header
c
c.. Constants for Trace ID (ITRIDPTR)
      integer    LIVETR, DEADTR          ! Parameters for trace Type
      parameter (LIVETR  = 1)
      parameter (DEADTR  = 2)
C
c.. Define constants for use with the DISKIO subroutines getfil, frefil,
c podisc
c GETFIL constants
      integer    CREATTMP, CREATNEW, OPENOLD
      parameter (CREATTMP   = 1)         ! Create a temporary file
      parameter (CREATNEW   = 3)         ! Create a new named file
      parameter (OPENOLD    = 4)         ! Open an existing file
c FREFIL constants
      integer    RLDLCLS, RLCLS
      parameter (RLCLS   = 2)            ! Release and Close the file
      parameter (RLDLCLS = 3)            ! Release, delete and close
c PODISC constants
      integer    POSABS, POSREL, DSKSTRT
      parameter (POSABS     = 1)
      parameter (POSREL     = 2)
      parameter (DSKSTRT    = 0)         ! The starting position on the disk
c
      integer    NSETSP, DBLSETSP
      parameter (NSETSP = 20 )           ! The max. no. of FKFIED input values
      parameter (DBLSETSP  = 2*NSETSP)
c
      integer    SINGPARM                ! The number of single valued params
      parameter( SINGPARM = 6)
c
      integer     FILNOTST, FILTERR, FILTVEL, FILTDIP
      parameter ( FILNOTST = -8)         ! Declare constants for the filter types
      parameter ( FILTERR  = -7)
      parameter ( FILTVEL  =  1)
      parameter ( FILTDIP  =  2)
c
      integer    CUT, PASS, ATINF
      parameter (CUT = 2, PASS = 1)
      parameter ( ATINF = 2)             ! Constant signifying lines near v = inf
c
      Integer PCDIR, CPDIR, PCINF, CPINF
      Parameter (PCDIR =  CUT - PASS)
      Parameter (CPDIR = PASS - CUT)
      Parameter (PCINF = ATINF * PCDIR)
      Parameter (CPINF = ATINF * CPDIR)
c
      Integer INITWDW
      Parameter (INITWDW = 32)
      Parameter (NOINIT  = 0)
c
c..                         Declare constants for the available window types
      integer     NUMWINDT
      parameter (NUMWINDT = 8)           ! The number of available window types
      Integer Hamming, Hanning, Gaussian, Bartlett, Rectang
      Integer Blackman, ExctBlck, BlckHarr
      Parameter (Hamming  = 1)
      Parameter (Hanning  = 2)
      Parameter (Gaussian = 3)
      Parameter (Bartlett = 4)           ! Triangular Window
      Parameter (RectAng  = 5)
      Parameter (BlackMan = 6)
      Parameter (ExctBlck = 7)                                          ! Exact Blackman
      Parameter (BlckHarr = 8)
c
c..                              Parameters for the window options
      Integer    NUMWOPT
      parameter (NUMWOPT = 3)            ! The number of windowing options
      Integer  BYA, BYK, BYW
      Parameter (ByA = 1)
      Parameter (ByK = 2)
      Parameter (ByW = 3)
      logical txinit, fkinit, txed, fked, tmptxhdr
      integer ltxunt1, ltxunt2, lfkunt1, lfkunt2, txunit, ohdrtxfk,
     &        txprestk
      common /TXFKE/ txinit, fkinit, txed, fked, txunit, ltxunt1,
     &               ltxunt2, lfkunt1, lfkunt2, ohdrtxfk, tmptxhdr,
     &               txprestk, ntx2fk, nfk2tx, range
      dimension lbuf(111),buf(111),scr(111),lscr(111)
      integer*2 ibuf(111),iscr(111)
      integer   istop, nready
c
      parameter (MAXWEI = 20)
      parameter (MAXMEM = SZTRANP)                                      ! The size of the transpose array
      logical FORWARD, NONORM
      parameter (FORWARD = .true., NONORM = .false.)
C
      common /TRANSP/ t(SZTRANP)
      common /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap
      common /APMEM/ ap(SZAPMEM)
      INTEGER lap(1)
      EQUIVALENCE( ap(1), lap(1) )
C
      integer origntr
      common /READT/ ilun,numhdr,numdat,iunhdr,ireel,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, origntr
C..                                       Parameter Passing common block
      integer winlenx, winlent
      common /TX2FKC/ ustime,uetime,nxpad,iwindot,winlent,iwindox,
     *                winlenx,lPRINT,nweigs, lcoord, weigs(MAXWEI)
C
      integer*2 txsius
      real      fksikhz, fkdelkhz
      integer   LocId, LocCoord
      integer   pow2, tpow, xpow, dskhdr1, dskhdr2, nspace1, nspace2
      logical   byrpnum, first
      character name*20
C
      save
C
      data lastnw/-1/, lfor/1/, itrcno/1/, nxdone/0/, first/.TRUE./
      DATA iwinap/0/
C
      if(istop .eq. STOPNOTR) go to 500
      nready = 0
      si     =  BUF(LSISPTR)
c      nsamps = IBUF(ISAMPPTR)
      nsamps = numdat
      delay  =  BUF(LDELSPTR)
c     save the range in common because the headers are destroyed
      range = lbuf(LDISTPTR)
      ntx2fk = ntx2fk + 1
c
c***********************                         ***********************
c                           Initialize the routine
c***********************                         ***********************
c
      if( ntx2fk .EQ. 1 ) then
C
          if (IAND(lPRINT,4).ne.0) then
              PRINT *, ' TX2FEX initialization info'
              PRINT *, ' Sample Interval (si): ', si,
     $           ' No. of samples (nsamps): ', nsamps,
     $           'Start Delay (delay): ', delay
              PRINT *, ' Start time (stime): ', ustime,
     $             ' End time (etime): ',  uetime
              PRINT *,' File Stream Numbers: ', ltxunt1, ltxunt2
	      PRINT *,' txprestk=',txprestk,' range=', range
	      PRINT *,' ntx2fk=',ntx2fk,' nfk2tx=',nfk2tx
          endif
C..                       Create scratch file to hold input traces
          IF( .NOT. first ) GOTO 10
          first = .FALSE.
          if (ltxunt1.ne.0) then
              itunit = ltxunt1                                          ! User specified file at Edit stage
          else
              call getfil( CREATTMP, itunit, name, istat)
              if (istat.ne.0) then
                  PRINT *,' *** TX2FX ERROR ***',
     $              ' Could not open 1st Scratch file'
                  STOP
              endif
          endif
c
C  Create a 2nd scratch file to hold the transposed data
          if (ltxunt2.ne.0) then
              ixunit = ltxunt2
          elseif (fked.and.(lfkunt1.ne.0) ) then                        ! FK2TX has opened the file stream
              ixunit = lfkunt1
          else
              call GETFIL (CREATTMP, ixunit, name, istat)
              if (istat.ne.0) then
          PRINT *,' *** TX2FX ERROR *** Could not open 2nd Scratch file'
                   STOP
              endif
          endif
          txunit = ixunit
          txinit = .true.                                               ! Let other FK procs know we have been called
c
c..  Create file to hold input trace headers
          if (ohdrtxfk.eq.0) then                                       ! Create temporary file
              call getfil( CREATTMP, ohdrtxfk, name, istat)
              if (istat.ne.0) then
                  PRINT *,' *** TX2FX ERROR ***',
     $              ' Could not open file for trace headers'
                  STOP
              endif
              tmptxhdr = .TRUE.                                         ! Indicate temporary file via /TXFKC/
          endif
c
   10     CONTINUE
          call podisc(itunit, POSABS, DSKSTRT)                          ! Ensure correct positioning
          call podisc(ixunit, POSABS, DSKSTRT)
          call podisc(ohdrtxfk, POSABS, DSKSTRT)                        ! Ensure correct positioning
          stime = ustime
          if (ustime.eq.-1.) stime = delay                               ! Preset stime if user didnt give it
          etime = uetime
          if (uetime.eq.-1.) etime = delay + FLOAT(nsamps-1)*si          ! same with etime
          nlen     = nint((etime - stime)/si) + 1                       ! No. of time samples per trace
          tpow     = pow2(nlen)
          nt       = 2 ** tpow
          n        = nt + nt                                            ! length of complex time trace in 4 byte words
          txsius   = ibuf(ISIPTR)                                       ! Save t-x sample interval for binary header
          byrpnum  = ( lbuf(LRPTRPTR) .ne. 0 )                          ! Form of trace indexing
          dskhdr1  = 0
          nspace1  = dskhdr1 + nlen                                     ! Total length of trace on 1st scratch disk
c
C..     Convert user defined outout data type to Coord system & Data ID
          if ( lcoord.eq.CDRECT ) then
               LocCoord = CDRECT
               LocId    = IDFKRCT
          else if ( lcoord.eq.CDPOLAR) then
               LocCoord = CDPOLAR
               LocId    = IDFKPLR
          else if ( lcoord.eq.CDPOLARU) then
               LocCoord = CDPOLAR
               LocId    = IDFKPLRU
          endif
C
C..            Assign memory allocations in the AP for windowing
          if(in.eq.DATINAP) then                                        ! Previous process left data in AP
             iout = CLEARAP                                             ! Force Data out
             call RLSEAP(buf(numhdr+1),nsamps)
          endif
c****     allocate the ap for nt complex
          call INAP( buf(numhdr+1), n )                                 ! Assign and allocate the AP buffers
          IF( iwinap .EQ. 0 ) THEN                                      ! Only assign ap addresses once (prestk)
              iwinap = nextad                                           ! The time domain window function will be put here
c             nextad = nextad + nlen*2
              nextad = nextad + nlen
          ENDIF
          in     = CLEARAP                                              !So that can transfer starting at correct address
      endif                                                             ! of routine initialization
c
c***********************                                ***********************
c     Accumlate incoming traces on disk. Padding and windowing as necessary
c***********************                                ***********************
c
      IF( ibuf(ITRIDPTR).eq.DEADTR) then
          do 105 i = 1, nlen                                              ! zero the trace and before writing to disk
  105     buf(numhdr+i) = 0.
      ELSE                                                              ! The trace is live
c          nwin   = winlent / si                                           ! Window length in samples
          nwin = winlent
          istart = 1
c
          IF( stime .gt. delay ) then
              IF( in.eq.DATINAP) then
                  iout = CLEARAP                                              ! Force Data out
                  call RLSEAP(buf(numhdr+1),nsamps)
              ENDIF
              nremov = (stime - delay) / si
              istart = 1      + nremov
              nsamps = nsamps - nremov
              delay = stime                                                 ! we just changed the time of the first sample
              BUF(LDELSPTR) = stime
              IBUF(ISAMPPTR) = nsamps
              numdat = nsamps
          ENDIF
c****   make sure the data length is the same as the last
c****   ah 19 nov 93
          IF( nsamps.gt.nlen) nsamps = nlen
c
          call INAP( buf(numhdr+istart), nsamps)                          ! put the trace in the ap
          istart = 1                                                      ! Assumes nothing in the ap
C
c  Assume that the window can stay in the ap from call to call
          IF( iwindot .ne. BOXCAR ) then
              IF( lastnw .ne. nwin .OR. ntx2fk .LE. 1 ) then              ! Create a new window
                  IF( nwin .eq. 0 ) nwin = nsamps
                  lastnw = nwin
                  IF( nwin .ne. nsamps .AND. iwindot .GT. 0 )
     &                iwindot = -iwindot                                 ! a partial window
                  call window( iwindot, scr, nwin, 0.25)                  ! create the window
                  IF( iuseap.eq.NOAP ) then
                      do 110 i = 1, nwin                                  ! save window in ap simulator
  110                 ap(iwinap+i-1) = scr(i)
                  else
                      call apput(scr,iwinap,nwin,2)
                      call apwd
                  ENDIF
              ENDIF                                                         ! of creating new window
c..                                                                     ! now apply the window
              IF( iuseap .eq. NOAP ) then                                   ! do it in memory
                  do 120 i = 1, nwin
  120             ap(i) = ap(i) * ap(iwinap+i-1)
                  IF( iwindot .lt. 0 ) then                                 ! A partial window
                      do 130 i = 1, nwin                                    ! do the end of the trace
                         indx     = istart + nsamps - i
                         ap(indx) = ap(indx) * ap(iwinap+i-1)
  130                 continue
                  endif
              else                                                          ! Use the array processor
                  call vmul(istart,1,iwinap,1,istart,1,nwin)
                  if( iwindot .lt. 0 ) then
                      indx = istart + nsamps - 1
                      call vmul(indx,-1,iwinap,1,indx,-1,nwin)
                  ENDIF
              ENDIF
          ENDIF                                                           ! of non boxCar window
C
          IF( stime .lt. delay ) then                                     ! Should we pad the front of the trace?
              nzeros = NINT((delay-stime) / si)                             ! The start time was before the delay
              ntop   = nsamps + nzeros
              IF( nzeros .GE. nlen ) THEN
                PRINT *, ' ***  WARNING  ***  The deep water delay of ',
     &          delay,' is after the etime of ',etime
                  nzeros = nlen
              ENDIF
              if( iuseap.eq.NOAP) then
                  IF( ntop .GT. nsamps ) THEN
                      do 200 i = 0, nsamps-1                            ! Move the trace down
  200                ap(ntop-i) = ap(nsamps -i)
                  ENDIF
                  do 210 i = 1, nzeros
  210                ap(i) = 0.
              else
                  call VMOV(istart+nsamps-1,-1,istart+ntop-1,-1,nsamps)
                  call VCLR(istart,1,nzeros)
              endif
              nsamps = nsamps + nzeros
              delay = stime                                             ! we just changed the time of the first sample
              buf(LDELSPTR) = stime
              ibuf(ISAMPPTR) = nsamps
              numdat = nsamps
          ENDIF
C
          IF( nsamps .lt. nlen ) then                                   ! Zero fill the back of the trace ?
              nzeros = nlen - nsamps
c           assuming data starts in ap at beginning (ap(1))
              IF( iuseap .eq. NOAP ) then
                  DO 250 i = 1, nzeros
  250             ap(nsamps+i) = 0.
              ELSE
                  CALL VCLR(1+nsamps,1,nzeros)
              ENDIF
          ENDIF
c
          iout = CLEARAP                                                ! Force the trace out of the ap
          CALL RLSEAP(buf(numhdr+1),nlen)
      ENDIF                                                             ! of if trace is live
c
      itemp = numhdr - dskhdr1+1
      CALL wrdisc (itunit, buf(itemp), nspace1 )
      CALL wrdisc (ohdrtxfk, buf, numhdr)                               ! write original header
      nsamps = nlen
      ibuf(ISAMPPTR) = nsamps                                           ! Update length in trace header
      numdat = nsamps
      in = CLEARAP
      IF( istop .eq. NOSTOP) RETURN                                     ! Are there more traces to come?
C
  500 CONTINUE
c
c***********************                                ***********************
c                        2D FFT The Accumalated Traces
c***********************                                ***********************
c
C... Start working on 2D fft by doing range to wavenumber transform
C... To do this we must transpose the stored time traces into a series of
C..  Range traces.
C
c
      if ( ntx2fk .LE. 0 ) then
          PRINT *,' ***  ERROR  ***  No traces were input to tx2fk!'
          STOP
      endif
c****
c****   reorder (sort) the data by range (distance) if prestack.
c****   Do it before windowing the data.  Do it by creating another
c****   scratch disk file and rewriting the data! pch apr 96
c****   BUT, only sort the data if there's more than 1 record (rp) -
c****   this is because "super-gathers" are several rps combined.
c****
      IF( txprestk .GT. 1 ) THEN
          CALL podisc( ohdrtxfk, POSABS, DSKSTRT)
          DO i = 1, ntx2fk
             CALL rddisc( ohdrtxfk, buf, numhdr, istat )
             scr(i) = lbuf(LDISTPTR)
             lap(i) = i
          ENDDO
          CALL indexx( ntx2fk, scr, lap )
          CALL wrdisc( ohdrtxfk, lap, ntx2fk )                           ! save the order for fk2tx
          CALL getfil( CREATTMP, itemp, name, istat)
          DO i = 1, ntx2fk
             CALL podisc( itunit, POSABS, (lap(i) - 1 ) * nlen )
             CALL rddisc( itunit, scr, nlen, istat )
             CALL wrdisc( itemp, scr, nlen )
          ENDDO
          CALL podisc( itemp, POSABS, DSKSTRT)
          CALL podisc( itunit, POSABS, DSKSTRT)
          DO i = 1, ntx2fk
             CALL rddisc( itemp, scr, nlen, istat )
             CALL wrdisc( itunit, scr, nlen )
          ENDDO
          CALL frefil( RLDLCLS, itemp,istat)    ! release this scratch file
      ENDIF
c
      if ( iwindox.ne.RECTANG) then    ! Need to window in range
        mid = ( ntx2fk + 1) / 2
        if (winlenx.eq.0) then  ! No taper length given
          end1   = mid          ! Window over entire range of data
          start2 = mid + 1
        else
          end1   = min(winlenx, mid)   ! End trace of 1st taper
          start2 = max( ntx2fk+1-winlenx, mid + 1 )   ! Start trace of 2nd taper
        endif
c
        call podisc( itunit, POSABS, DSKSTRT)
        xfac = fwindw(1., 1., end1, iwindox, CPDIR, INITWDW)
c
        iend1 = NINT(end1)
        do 510 i = 1, iend1
          call rddisc( itunit, scr, nspace1, istat)
          xfac = fwindw(float(i), 1., end1, iwindox, CPDIR, NOINIT)
          do 515 j = dskhdr1+1, nspace1
            scr(j) = xfac * scr(j)
  515     continue
          call podisc( itunit, POSREL, -nspace1)
          call wrdisc( itunit, scr, nspace1)
  510  continue
c
       istart = start2 * nspace1 + DSKSTRT
        call podisc( itunit, POSABS, istart)
        rtrac = float(ntx2fk)
        xfac = fwindw(start2, start2, rtrac,iwindox, PCDIR, INITWDW)
c
        istart2 = NINT(start2)
        do 525 i = istart2, ntx2fk
          call rddisc( itunit, scr, nspace1, istat)
          xfac = fwindw(float(i),start2, rtrac, iwindox, PCDIR, NOINIT)
          do 520 j = dskhdr1+1, nspace1
            scr(j) = xfac * scr(j)
  520     continue
          call podisc( itunit, POSREL, -nspace1)
          call wrdisc( itunit, scr, nspace1)
  525   continue
      endif
c
      xpow    = pow2(ntx2fk+nxpad)
      nx      = 2**xpow
      nxo2    = nx/2
      dskhdr2 = 0                                                       ! Do not store header on scratch file 2
c     n = nt * 2, dskhdr2 = 0 from line above
      nspace2 = dskhdr2 + n
      nsampo  = 2 * nlen
      IF( nsampo .GT. maxsamps) THEN
          PRINT *,' The FK output data length is too long.'
          PRINT *,' (Max is 32K complex frequencies).'
          PRINT *,' nsampo=',nsampo,' maxsamps=',maxsamps
          STOP
      ENDIF
c
c.. We are going to transpose data into a series of range traces of length nx
c.. with a constant time sample.
c
      nsizec  = MAXMEM / nx / 2                                          ! No. of range traces in memory at one time
      if(nsizec.gt.nlen) nsizec = nlen                                  ! Cant have more than data length
      ntimes =  nlen / nsizec                                            ! Number of passes through data
      nrem   =  mod (nlen, nsizec)
      if (nrem .eq. 0) then                                             ! Add one if remaining time samples
        nrem = nsizec
      else
        ntimes = ntimes + 1
      endif
      nsizeoc  = 2*nsizec                                                 ! Double on output because complex

c
c..                    Create a temporary file to hold partial transformed traces
      call getfil( CREATTMP, igunit, name, istat)
      if (istat.ne.0) then
        PRINT *,' *** TX2FX ERROR ***',
     $              ' Could not open temp file'
        STOP
      endif


      jstarti = dskhdr1 + DSKSTRT                                       ! disk address of 1st  input time sample
      nsize   = nsizec
      nsizeo  = nsizeoc

      do 1100 i = 1, ntimes                                             ! Outer loop over sections of time traces
         if (i.eq.ntimes) then
            nsize  = nrem
            nsizeo = 2 * nsize
         endif
C..                              Read in the trace data and scatter into array T
         istart = jstarti
         do 1060 j = 1, ntx2fk
            call podisc( itunit, POSABS, istart)
            call rddisc( itunit, scr, nsize, istat)
            indx = j
            do 1050 k = 1, nsize
              t(indx)   = scr(k)
              indx      = indx + nx
 1050       continue
            istart = istart + nspace1                                   ! position of next time trace
 1060    continue
C..                                Now FFT the nsize range traces held in memory
         if (iuseap .eq. NOAP) then
            indx = 1
            do 1070 j = 1, nsize
              IF ( ntx2fk .LT. nx ) THEN
                do 1065 k = indx + ntx2fk, indx-1+nx                    ! Fill end of trace with zeros
                  t(k) = 0.
 1065           continue
              ENDIF
c..                             Do FFT in place as real time series of length NX
              call gnrfft ( t(indx), nx, FORWARD,.false.)
              indx = indx + nx
 1070       continue
         else
            stop 'AP fft routine not installed'
         endif

c..            Transpose (GATHER) the data back into partial complex time traces
          do 1090 j = 1, nxo2
              indx = j * 2 - 1

              do 1080 k = 1, nsizeo, 2
                scr(k)   = t(indx)
                scr(k+1) = t(indx+1)
                indx     = indx + nx
 1080         continue

              call wrdisc(igunit, scr, nsizeoc)
 1090     continue

        jstarti = jstarti + nsize
 1100 continue                                                          ! End of Outer loop

c
c..                 Assemble the complex traces sections into complete traces
       nsizei  =  2*nsizec
       nslice  =  nsizec*nx
       jstarti =  DSKSTRT - nsizei

       call podisc(ixunit,POSABS,DSKSTRT)
c****  AH used array scr which is the scratch trace array rather than
c****  the transpose array t, but the indexing looks like it should be
c****  the transpose array. - pch May 2005
       do 1103 i = 1, nspace2
c         scr(i) = 0.
         t(i) = 0.
 1103  continue
       do 1102 j = 1, nxo2
         indx   = dskhdr2 + 1
c         itemp = indx + nsizei*ntimes
c         IF( itemp .GT. 33000 ) THEN
c             PRINT *,' Buffer overflow expected.'
c         ENDIF
         nsizei = 2*nsizec
         jstarti= jstarti + nsizei
         istart = jstarti
         do 1101 i = 1, ntimes
           if (i.eq.ntimes) nsizei = 2 * nrem
           call podisc(igunit,POSABS,istart)
c           call rddisc(igunit,scr(indx),nsizei,istat)
           call rddisc(igunit,t(indx),nsizei,istat)
           indx   = indx   + nsizei
           istart = istart + nslice
 1101    continue
c         call wrdisc(ixunit,scr,nspace2)
         call wrdisc(ixunit,t,nspace2)
 1102  continue
c..                                 Add a dummy Nyquist trace
       do 1104 i = 1, nspace2
c         scr(i) = 0.
         t(i) = 0.
 1104  continue
c       call wrdisc(ixunit,scr,nspace2)
       call wrdisc(ixunit,t,nspace2)

       call frefil(RLDLCLS, igunit,istat)                                ! release temp file

c..                                           Assembly completed

C..              Now unpack 1st trace into zero wavenumber and Nyquist traces
      istart = dskhdr2 + DSKSTRT
      call podisc(ixunit,POSABS, istart)
      call rddisc(ixunit, scr, nsampo, istat)
      do 1110 i = 1, nsampo, 2
        t(i)          = scr(i)
        t(i+1)        = 0.
        t(nsampo+i)   = scr(i+1)
        t(nsampo+i+1) = 0.
 1110 continue

      call podisc(ixunit, POSABS, istart)                               ! write 0 wavenumber part
      call wrdisc(ixunit, t, nsampo)

      istart = istart + nxo2 * nspace2
      call podisc(ixunit, POSABS,istart)                                ! write Nyquist wavenumber part
      call wrdisc(ixunit,t(nsampo+1),nsampo)

      call podisc(ixunit, POSABS, DSKSTRT)                              ! Rewind the TK disc file &


      nsamps = n
      nready = nxo2 + 1                                                 ! Pass the number of traces back to CONTRO
c
c.. Pass the saved t-x values through common \READT\. These will be saved in
c   The binary header
      nfktrc   = nready
      itxsi    = txsius                                                 ! t-x sample interval in us
      itxdel   = nint (stime * 1000.)                                   ! & the new start time delay
      fksikhz  = 10000./ txsius / float(nt)                             ! fk sample interval in hHz
      fkdelkhz = -10000./ 2. / txsius                                   ! fk start time in hhz
      if ( lcoord.eq.CDPOLARU) fkdelkhz = 0.
      origntr  = ntx2fk                                                  ! The number of traces prior to pad
c
      ntx2fk  = 0
c     removed clearing of nextad because prestk processes still may use!
c      nextad = 0                                                        ! Update AP status
      in     = CLEARAP
      lastnw = 0
      RETURN
c
c***********************                                ***********************
C*         Get a trace from disk and transform it into frequency (w)
c***********************                                ***********************
      ENTRY GETNFK(buf,lbuf,ibuf)
      ntx2fk  = ntx2fk  + 1
      call RDDISC(ixunit,buf(numhdr-dskhdr2+1), nspace2, istat)
      DO i = nsampo+1, nspace2
         buf(numhdr-dskhdr2+i) = 0.
      ENDDO
c
c.. Set the trace header
      do 1450 i = 1, numhdr
        lbuf(i) = 0
 1450 continue
      if (byrpnum) then                                                 ! Index using RP/CDP no.
          lbuf(LRPNPTR)  = lfor
          lbuf(LRPTRPTR) = itrcno
      else                                                              ! Index using shots
          lbuf(LSHOTPTR) = lfor
          lbuf(LSHTRPTR) = itrcno
      endif
      if( intrcs.eq.1 ) then                                            ! One trace per "gather"
          lfor   = lfor + 1
      else
          itrcno = itrcno + 1                                           ! Or index by trace no.
      endif
      ibuf(ITRIDPTR) = LIVETR
       buf(LDELSPTR) = fkdelkhz                                         ! record FK start (hHz) as the delay
      ibuf(IDELMPTR) = nint(fkdelkhz*1000.)                             ! convert to dHz
       buf( LSISPTR) = fksikhz                                          ! record FK interval in hHz
      ibuf(  ISIPTR) = nint(fksikhz*1.E6)                               ! & in 10** -4 Hz
      ibuf(ISAMPPTR) = n                                                ! length of output in 4 byte words
      numdat = n
      IF( ntx2fk .EQ. nxo2+1 ) THEN
          lbuf(lgatptr) = -1
          lfor = lfor + 1
          itrcno = 1
      ENDIF
c
c.. Negate every other complex sample prior to the FFT in time. This ensures
c   that on output the frequencies will run from -Nyq -> 0 -> Nyq
c
      in = CLEARAP                                                      ! put the data in the ap
      call INAP(buf(numhdr+1), nsamps)
      if( iuseap .eq. NOAP) then
          do 1500 i = 1, nsampo, 4
               ap(i)   = -ap(i)
               ap(i+1) = -ap(i+1)
 1500     continue
c          call fftfwd(ap, tpow)
          call fftinv(ap, tpow)
          if( LocCoord .eq. CDPOLAR) then
              call POLARC(nt, ap, buf(numhdr+1), buf(numhdr+nt+1))
              in     = CLEARAP                                          ! data is no longer in the ap
              iasgnd = 0                                                ! do not assign the ap
          endif
      else
          call cvneg(1,4,1,4,nsamps/4)
          call cfft(1,nt,1)
          call cfftsc(1,nt)
          if (lcoord .eq. CDPOLAR) then
               call polar(1,2,nextad,2,nt)
               call vmov(nextad,2,1,1,nt)
               call vmov(nextad+1,2,nt+1,1,nt)
          endif
      endif
c
      idtype = LocId                                                    ! Set Data id in common /READT/
      numdat = n                                                        ! & Pass length of output data
c

      if (IAND(lPRINT,1).ne.0) then
        PRINT '(/A)',' GETNFK(tx2fex) debug info'
        PRINT *, ' Output trace ', ntx2fk, ' Data length ',n
        PRINT *, ' idtype ', idtype
      endif
c
c      if ( (ntx2fk.eq.nxo2+1).AND.( .not.fkinit) )
c     $   call frefil (RLDLCLS, ixunit,istat)
c      RETURN
      end
c
c
      subroutine txfkinit
C-------------------------------------------------------------------------------
C This routine initializes the global common blocks for tx2fk/fk2tx and is
C called as the beginning of the respective edit stages
C-------------------------------------------------------------------------------
C INCLUDE FILES
c INCLUDE txfkgbl.inc
c.. This is the global common block for the FK routines. At present it is used
c.. by TX2FEX, FK2TEX & their associated edit routines to indicate whether they
c.. have been called & to pass file unit numbers.
c
c  Last Modified 10/18/88
c
      logical txinit, fkinit, txed, fked, tmptxhdr
      integer ltxunt1, ltxunt2, lfkunt1, lfkunt2, txunit, ohdrtxfk,
     &        txprestk
      common /TXFKE/ txinit, fkinit, txed, fked, txunit,
     $               ltxunt1, ltxunt2, lfkunt1, lfkunt2,
     $               ohdrtxfk, tmptxhdr, txprestk, ntx2fk, nfk2tx, range
C PROGRAM
      logical first
      SAVE
c
      data first /.TRUE./
c
      if (first) then
        first  = .FALSE.
        txinit = .FALSE.
        fkinit = .FALSE.
        fked   = .FALSE.
        ohdrtxfk = 0
        tmptxhdr = .FALSE.
        ntx2fk = 0
      endif
C
      return
      end
c
c===============================================================================
c
      integer function Pow2(size)
C-------------------------------------------------------------------------------
C  This function calculates the nearest power of 2 above the input variable size
C Returns -1 if size <= 0
C-------------------------------------------------------------------------------
      integer    DEBUG
      parameter (DEBUG = 1)
      integer    MAXTR
c      parameter (MAXTR = 32769)
      parameter (MAXTR = 65536)
c
      integer size, length, tempow
c
      if (size.le.0) then
        pow2   = -1
        if (DEBUG.gt.0) then
          PRINT *,' *** POW2 error *** size < 0'
          STOP
        endif
      else if (size.ge.MAXTR) then
        if (DEBUG.gt.0) then
          PRINT *,' *** POW2 error *** size > MAXTR'
          STOP
        endif
      else
        tempow = 0
        length = 1
    5   if(length.lt.size) then
          length = length + length
          tempow = tempow + 1
          go to 5
        endif
        pow2 = tempow
      endif
      return
      end
