      subroutine fkfiex( buf, cbuf, ibuf, scr, lscr, iscr )
c-------------------------------------------------------------------------------
c
c  FKFIEX is the execution phase of the SIOSEIS seismic process FKFILT.
c  FKFILT performs filtering in the FK domain. See subroutine FKFIED for the
c  user description.
c
c     The input traces are supplied with wavenumbers varying from k = 0 to +Nyq
c  Each trace contains frequencies from -Nyq to + Nyq. This arrangement is the
c  one supplied by the 2D FFT and is convenient for the F-K migration.
c  However it differs from the conceptual model of the user which has
c  wavenumbers running from -Nyq -> 0 -> Nyq and only positive frequencies.
c
c    This setup is convenient for velocities filtering since lines are sorted
c  by FKFIED into increasing order, but care must be taken to add appropriate
c  velocity lines near v = 0 if the user wants different velocity filters in
c  0+ & 0-. Care is also need to ensure proper continuity of filter across
c  v (inf).
c
c     Conversely filters defined in terms of dips are consistent with the
c  conceptual model, since they vary smoothly from -inf -> 0 -> +inf, but are a
c  odds with the way the traces are given to FKFIEX. Thus the input dip lines
c  are converted to velocities as part of the input process.
c
c Inputs:
c    buf/cbuf/ibuf  : Equivalenced set of arrays holding the trace (header+data)
c    scr/lscr/iscr) : Equivalence set of scratch arrays.
c
c Outputs:
c    buf/cbuf/ibuf  : Filtered version of trace
c
c Call Chain:
c    CONTRO:FKFIEX
c
c Externals:
c   ClcWpt, FrmWnd                                                      ! Local fkfiex subroutines
c   INAP, RLSEAP EXIT
c   PODISC, RDDISC                                                      ! disk i/o
c
c Mdofications
c      10/12/88 ajh.  ( SZAPMEM )
c Last Modified
c      12/20/88 ajh   Altered Logic of line addition at v = 0
c                     Changed definition of vzerop
c                     Added more Debug info
c      3/16/89 ajh  Added read of binary header info through common /READT/
c     7 Nov 91 pch  Add reinitialization stuff for prestack.
c 8 Apr 09 - Use common numdat rather than segy header word ISAMPPTR
c 27 Sept 10 - Don't cal FrmWnd if cut/pass goes across 0


      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
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
c This include file include all (?) of the parameters needed to define SIOSEIS
c I/O routines. In particular it defines positions of objects in the SEGY
c headers & constants for these values
c
c EBCDIC HEADER
c Declare Header lengths. These are the length of the EXTERNAL disk image in
c host words.
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
      integer    ITDELPTR
      parameter (NTRCPTR  =  7)                                         ! No of traces / gather
      parameter (IFMTPTR  = 13)                                         ! The SEGY Format of the data
      parameter (IDTYPPTR = 31)                                         ! The Domain/ID of the data e.q. T-X data
      parameter (INKPTR   = 32)                                         ! The No. of wavenumbers in an f-k dataset
      parameter (ITSIPTR  = 33)                                         ! Record of tx sample interval in us
      parameter (ITDELPTR = 34)                                         ! Record of tx time delay in ms.
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
c TRACE HEADER
c.. Declare header length on external disk files. Internal length is given by
c Numhdr in common block READT.
      integer  CRAYTHDR, NORMTHDR
      parameter ( CRAYTHDR =  30)                                       ! Length of external disk image
      parameter ( NORMTHDR = 2*CRAYTHDR)
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
      integer    CREATTMP, CREATNEW
      parameter (CREATTMP   = 1)                                        ! Create a temporary file
      parameter (CREATNEW   = 3)                                        ! Create a new named file
c FREFIL constants
      integer    RLDLCLS
      parameter (RLDLCLS = 3)
c PODISC constants
      integer    POSABS, POSREL, DSKSTRT
      parameter (POSABS     = 1)
      parameter (POSREL     = 2)
      parameter (DSKSTRT    = 0)                                        ! The starting position on the disk
c fkfilt.inc
c Include file for the FK filter routines FKFIED & FKFIEX
c
      integer    NSETSP, DBLSETSP
      parameter (NSETSP = 20 )                                          ! The max. no. of FKFIED input values
      parameter (DBLSETSP  = 2*NSETSP)
c
      integer    SINGPARM                                               ! The number of single valued params
      parameter( SINGPARM = 6)
c
      integer     FILNOTST , FILTERR, FILTVEL, FILTDIP
      parameter ( FILNOTST  = -8)                                       ! Declare constants for the filter types
      parameter ( FILTERR   = -7)
      parameter ( FILTVEL   =  1)
      parameter ( FILTDIP   =  2)
c
      integer    CUT, PASS, ATINF
      parameter (CUT = 2, PASS = 1)
      parameter ( ATINF = 2)                                            ! Constant signifying lines near v = inf
c
      Integer PCdir, CPdir, PCinf, CPinf
      Parameter (PCdir =  Cut - Pass)
      Parameter (CPdir = Pass - Cut)
      Parameter (PCinf = AtInf * PCdir)
      Parameter (CPinf = AtInf * CPdir)
c
      Integer INITWDW
      Parameter (INITWDW = 32)
      Parameter (NOINIT  = 0)
c
c..                         Declare constants for the available window types
      integer     NUMWINDT
      parameter (NUMWINDT = 8)                                          ! The number of available window types
      Integer Hamming, Hanning, Gaussian, Bartlett, Rectang
      Integer Blackman, ExctBlck, BlckHarr
      Parameter (Hamming  = 1)
      Parameter (Hanning  = 2)
      Parameter (Gaussian = 3)
      Parameter (Bartlett = 4)                                          ! Triangular Window
      Parameter (RectAng  = 5)
      Parameter (BlackMan = 6)
      Parameter (ExctBlck = 7)                                          ! Exact Blackman
      Parameter (BlckHarr = 8)
c
c..                              Parameters for the window options
      Integer    NUMWOPT
      parameter (NUMWOPT = 3)                                           ! The number of windowing options
      Integer  BYA, BYK, BYW
      Parameter (ByA = 1)
      Parameter (ByK = 2)
      Parameter (ByW = 3)
C PROGRAM
c
      dimension buf(111),scr(111),lscr(111)
      integer*2 ibuf(111), iscr(111)
      complex   cbuf(111)
c
      common /FKFILT/ munit, nlists, kTrace
      common /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap
      common /APMEM/ ap(SZAPMEM)
      common /READT/lun,numhdr,numdat,iunhdr,ireeln,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc
c
      real    vLine(DBLSETSP + 2)
      integer vType(DBLSETSP + 2), wPt(DBLSETSP + 2)
      integer FiltTyp, wBase, Offs
      integer BasAddr0, BasAddr1
      integer Taper
      logical first, Found
c
      save
c
      data first/.TRUE./, kTrace/-1/
c
      if (first) then
          first = .FALSE.
          if ( (idtype.ne.IDFKRCT).AND.(idtype.ne.IDFKPLR).AND.
     *        (idtype.ne.IDFKPLRU) ) then
             print *,' ***  FKFILT ERROR  ***  The input must be ',
     *               ' in the FK domain.'
              STOP
          endif
c
          call podisc( munit, POSABS, DSKSTRT)                          ! Get single parameters
          call rddisc( munit, scr, SINGPARM, istat )
c
          dx      = scr(1)                                              ! trace spacing
          iwindo  = lscr(2)                                             ! type of window function
          lprint  = lscr(3)                                             ! debug print option
          iwopt   = lscr(4)                                             ! manner in which to apply window
          FiltTyp = lscr(5)                                             ! Type of filter
          nLine   = lscr(6)                                             ! Number of filter lines
c
c         call podisc( iunhdr, POSABS, NORMEBC+DSKSTRT )
c         call rddisc( iunhdr, scr, NORMBIN, istat )                    ! read Binary Header
          nk     = 2*nfktrc - 1                                         ! The no. of wavenumbers
          dt     = itxsi / 1.0E6                                        ! Sample interval in secs
          if (IAND(lprint,2).ne.0) then
            print *,'FKFIEX Initialization info'
            print *,'dx: ', dx,' nk: ', nk,' dt: ', dt
            print *,'No. of Filter Lines (nLine): ', nLine
          endif
c
c..                                   Calculate the constants
c          nsamps = ibuf(ISAMPPTR)                                       ! The no. of 4 byte samples
          nsamps = numdat
c          nw     = ibuf(ISAMPPTR) / 2                                   ! The no. of frequencies
          nw = numdat / 2
          wBase  = nw / 2 + 1                                           ! The 0 frequency position
          If (FiltTyp.eq.FILTDIP) dx = 0.001                            ! Includes allowance for mS.
          vnorm  = float(nk) * dx / dt / float(nw)                      ! Non dimensioning velocity
          pnorm  = 1. / vnorm
          vinfp  = float(nw)                                            ! Velocity  ~ Inf
          vzerop = 1./ float(nk)                                        ! Velocity  = 0+
c
          BasAddr0  = DBLSETSP + 1                                      ! Set base address in scr for type info
          BasAddr1  = BasAddr0 + 1
c
          call rddisc( munit, scr, DBLSETSP, istat )                    ! Read in filter lines
          call rddisc( munit, lscr(BasAddr1), DBLSETSP, istat )         ! & cut/pass
c
          found   = .False.                                             ! Look for filter lines stradling zero
          i       = 1
  5       continue
            i     = i + 1
            if ((scr(i) * scr(i-1) ) .le. 0.) then
              found = .true.
              izm   = i - 1                                             ! Index of  last line <  0
              izp   = i                                                 ! Index of first line >= 0
            endif
          if ( (.not.Found).AND.(i.lt.nLine) ) go to 5
c
          if (.not.Found) then                                          ! Filter Line do not straddle zero
            if(scr(1).lt.0.) then
              izm = nLine                                               ! -ve Lines only
              izp = nLine + 1
              lscr(BasAddr0+izp) = lscr(BasAddr0+1)
            else                                                        ! +ve Lines only
              izm = 0
              izp = 1
              lscr(BasAddr0+izm) = lscr(BasAddr0+nline)
            endif
          endif
c...            *** Input lines were defined as dips : Convert to velocities ***
          If (FiltTyp.eq.FILTDIP) then
            j          = izp
            do 10 i = 1, izm                                            ! Convert -ve Dips
              j        = j - 1                                          ! Reverse index to preserve ordering
              vLine(j) = pnorm / scr(i)
              vType(j) = lscr(BasAddr0 + i)
  10        continue
c
c..                 Filter lines differ across v = 0 so add lines at v = 0+ & 0-
            if ( lscr(BasAddr0+1).ne.lscr(BasAddr0+nLine) ) then
              vLine(izm+1) = -vzerop
              vLine(izm+2) =  vzerop
              vType(izm+1) = lscr(BasAddr0 + 1)
              vType(izm+2) = lscr(BasAddr0 + nLine)
              ntop         = nLine + 2
              izm          = izm   + 2
            else
              ntop         = nLine
            endif
c
            if ( Found .AND. (scr(izp).eq.0.) ) then                    ! A line at zero dip ?
              vLine(izm+1) = vInfp                                      ! Convert to inf. velocity
            else
              vLine(izm+1) = pnorm/scr(nLine)
            endif
            vType(izm+1) = lscr(BasAddr0 + nLine)
c
            j = nTop + 1                                                ! Convert +ve dips to velocity
            do 20 i = izp, nLine-1
              j        = j - 1
              vLine(j) = pnorm / scr(i)
              vType(j) = lscr(BasAddr0 + i)
  20        continue
c
            nLine        = nTop
c....                                ***  Filter lines are given as velocity ***
          else if (FiltTyp.eq.FILTVEL) then
c
            do 25 i = 1, izm                                            ! Normalize -ve velocities
              vLine(i) = pnorm * scr(i)
              vType(i) = lscr(BasAddr0 + i)
  25        continue
c..
            Offs = 0
            izpp = izp + 1
            if ( Found.and.(scr(izp).eq.0.) ) then                      ! Line at v = 0
              vLine(izp)  = -vzerop                                     ! Split into 0+ & 0- lines
              vLine(izpp) =  vzerop
              vType(izp)  = lscr(BasAddr0 + izp)
              vType(izpp) = lscr(BasAddr0 + izp)
              izp         = izpp                                        ! Adjust to first v > 0
              Offs        = 1                                           ! Added a line
c
c...                                            Line types differ across v = 0.
            else if ( lscr(BasAddr0+izm).ne.lscr(BasAddr0+izp) ) then
              vLine(izp)  = -vzerop
              vLine(izpp) =  vzerop                                     ! Add lines at v = 0+ & 0-
              vType(izp)  = lscr(BasAddr0 + izm)
              vType(izpp) = lscr(BasAddr0 + izp)
              Offs        = 2
            endif
c
            do 30 i = izp, nLine                                        ! Normalize +ve velocities
              vLine(i + Offs) = pnorm * scr(i)
              vType(i + Offs) = lscr(BasAddr0 + i)
 30         continue
c
            nLine = nLine + Offs                                        ! Allow for added lines
          endif                                                         ! of if FiltTyp =
          if (IAND(lprint,2).ne.0) then
            print '(/A)','FKFIEX : Normalized Velocities'
            print *,'Adjusted No. of Filter Lines (nLine): ', nLine
            do 40 i = 1, nLine
              if (vType(i).eq.CUT) then
                print *,' Cut Velocity : ', Vline(i)
              else if (vType(i).eq.PASS) then
                print *,'Pass Velocity : ', Vline(i)
              else
                print *,'     Velocity : ', Vline(i),' Type : ',vType(i)
              endif
  40        continue
          endif
      Endif                                                             ! of if First
c
c*************************                             *************************
c*                      Start of the Main Filtering loop
c*************************                             *************************
c
      call inap( buf(numhdr+1), nsamps )                                ! Put the data in the AP
      BasAddr0            = NextAd - 1                                  ! Base address in the AP for window
      BasAddr1            = NextAd
      ap(BasAddr0 + wbase) = 0.                                         ! Zero frequency
      kTrace              = kTrace + 1                                  ! Current wavenumber (k)
c
c***                                *** Generate a Window function for trace ***
c
      If (kTrace.eq.0) then                                             ! This is the Zero wavenumber Trace
        wPt(1)     = wbase - 1
        wPt(nLine) = wbase
      else                                                              ! Wavenumber <> 0
        call ClcWpt(0, wPt, kTrace, vLine, nLine, nw)
c..                                     Position of Line Intercepts on the trace
        iprev = wPt(1)
        i     = 1
c
  120   If ( (iprev.lt.nw).AND.(i.lt.nLine) ) then
          i = i + 1
          if (wPt(i).gt.iprev) then                                     ! Gaps between lines
            if (vType(i).eq.vType(i-1))then                             ! Simple pass/cut sector
              If (vType(i).eq.CUT ) then
                 val = 0.0
              else                                                      ! if pass then
                 val = 1.0
              endif
              do 130 jj = iprev + 1, wPt(i)
  130           ap(BasAddr0 + jj) = val
            else                                                        ! Taper at ends of pass/cut
              Taper = vType(i) - vType(i-1)                             ! Pass -> Cut or Cut -> Pass ?
              IF( vLine(i)*vLine(i-1) .GE. 0 ) call FrmWnd
     *         ( iwindo, iwopt, vLine(i-1), vLine(i), kTrace,
     *         iprev+1-wbase, wPt(i)-wbase, ap(BasAddr1 + iprev), Taper)
c
            endif                                                       ! of vType(i) <> vType(i-1)
          endif                                                         ! of Gaps between lines
          iprev = wpt(i)
          go to 120
        endif                                                           ! of iprev < nw & i < nLine
      endif                                                             ! of Wavenumber <> 0
c
c...                                Check the form of the sector across v = inf
      if (vType(1).eq.vType(nLine)) then                                ! Simple pass/cut sector
          if (vType(1).eq.CUT ) then
            val = 0.0
          else                                                          ! if pass
            val = 1.0
          endif
          do 150 jj = 1, wPt(1)
 150        ap(BasAddr0 + jj) = val
          do 160 jj = wPt(nLine)+1, nw
 160        ap(BasAddr0 + jj) = val
      else                                                              ! Else tapering at v(inf)
        Taper = ATINF * (vType(1) - vType(nLine))
c..                                                  Taper large -ve velocities
        call FrmWnd(iwindo, iwopt, vLine(1), vLine(nLine), kTrace,
     *                1 - wbase, wPt(1) - wbase, ap(BasAddr1), Taper)
c..                                                  Taper large +ve velocities
        call FrmWnd(iwindo, iwopt, vLine(1), vLine(nLine), kTrace,
     *                  wPt(nLine)+1-wbase, nw-wbase,
     *                  ap(BasAddr1 + wPt(nline)), Taper)
      endif
c
c*************************                             *************************
c                       Multiply Trace by its Window Fn
c*************************                             *************************
c
      If ( (idtype.eq.IDFKPLR).OR.idtype.eq.IDFKPLRU ) then
        do 200 i = 1, nw
 200    ap(i) = ap(i) * ap(BasAddr0 + i)
      else if (idtype.eq.IDFKRCT) then
        j = -1
        do 210 i = BasAddr0 + 1, BasAddr0 + nw
          j       = j + 2
          ap(j)   = ap(j)   * ap(i)
          ap(j+1) = ap(j+1) * ap(i)
 210    continue
      endif
c
      iout = CLEARAP                                                    ! Get the modified trace out of the Ap
      call rlseap( buf(NumHdr+1), nsamps)
      RETURN
      end
c++
c++
      Subroutine ClcWpt(type, wPt, k, v, nLine, nw)
c------------------------------------------------------------------------------
c    ClcWpt calculates the positions at which the filtering lines cross the
c current trace. These positions are returned as indices of the input trace in
c the integer array wPt. The indices are chosen to be the nearest index below
c the actual crossing line.
c
c Inputs:
c   type      This is an index denoting the form of the filtering lines.
c             Currently ignored as all lines are currently are radial lines
c             from the origin.
c   k         The input wavenumber trace k E [0,nk/2]
c   v(nLine)  The array of normalized velocities for the filter lines
c   nLine     The number of filter lines.
c   nw        The length of the input trace. Frequencies are assumed to run
c             -Nyq -> 0 -> +Nyq - dw
c Outputs:
c    wpt(nLine)  The array of returned indices
c
c Call Chain:
c    CONTRO:FKFIEX:ClcWpt
c
c Externals:
c    IBLW
c
c Last Modified:
c    10/6/88 ajh
c
c------------------------------------------------------------------------------
c
      integer type
      integer k, nw
      integer nLine
      integer wpt(nLine)
      real      v(nLine)
c
      integer wbase, iblw
c
      wbase = nw/2 + 1                                                  ! Position of the zero frequency sample
      do 10 i = 1, nLine
        wpt(i)   = iBlw(v(i) * k) + wbase                               ! Calculate crossing position
        if(wPt(i).lt.0) then                                            ! Make sure wpt in [0,nw]
          wPt(i) = 0
        else if (wPt(i).gt.nw) then
          wPt(i) = nw
        endif
  10  continue
      RETURN
      end
C**
c**
      Integer Function Iblw(x)
c-------------------------------------------------------------------------------
c Iblw returns the nearest integer below a real value x. Not the integer part.
c-------------------------------------------------------------------------------
c
      real x
c
      if(x.ge.0.) then
        Iblw = Int(x)
      else
        Iblw = Int(x) - 1
      endif
      RETURN
      end
c++
c++
      Subroutine FrmWnd(iwindo,iwopt,vst,vend,k,stPt,endPt,buf,dir)
c-------------------------------------------------------------------------------
c   FrmWnd calculates the half window function between vst & vend for the
c points stPt to endPt. vst & vend are real so the window does not necessarily
c start & finish on integer values.
c
c Inputs:
c   iwindo Type of window function
c   iwopt  Variable for windowing
c          = BYA - Window as a function of angle
c          = BYW - Window as a function of frequency
c          = BYK - Window as a function of wavenumber
c
c   vst    Start velocity of filter
c   vend   Final velocity of filter
c    stPt  First point in window
c   EndPt  Last point in window
c
c    dir  Direction of the windowing function
c         = PCDIR - The window is from pass at vst to cut vend
c         = CPDIR - The window is from cut at vst to pass vend
c         = PCINF - The velocity lines straddle v infinity +ve v is pass
c         = CPINF - "       "       "       "       "      +ve v is cut
c
c Outputs:
c   buf(stPt:EndPt)                                                     ! buffer in which to place window fn
c
c Call Chain:
c    CONTRO:FKFIEX:FrmWnd
c
c Externals:
c    Fwindw
c
c Last Modified:
c    10/6/88 ajh
c-------------------------------------------------------------------------------
c
C INCLUDE FILES
c fkfilt.inc
c Include file for the FK filter routines FKFIED & FKFIEX
c
      integer    NSETSP, DBLSETSP
      parameter (NSETSP = 20 )                                          ! The max. no. of FKFIED input values
      parameter (DBLSETSP  = 2*NSETSP)
c
      integer    SINGPARM                                               ! The number of single valued params
      parameter( SINGPARM = 6)
c
      integer     FILNOTST , FILTERR, FILTVEL, FILTDIP
      parameter ( FILNOTST  = -8)                                       ! Declare constants for the filter types
      parameter ( FILTERR   = -7)
      parameter ( FILTVEL   =  1)
      parameter ( FILTDIP   =  2)
c
      integer    CUT, PASS, ATINF
      parameter (CUT = 2, PASS = 1)
      parameter ( ATINF = 2)                                            ! Constant signifying lines near v = inf
c
      Integer PCdir, CPdir, PCinf, CPinf
      Parameter (PCdir =  Cut - Pass)
      Parameter (CPdir = Pass - Cut)
      Parameter (PCinf = AtInf * PCdir)
      Parameter (CPinf = AtInf * CPdir)
c
      Integer INITWDW
      Parameter (INITWDW = 32)
      Parameter (NOINIT  = 0)
c
c..                         Declare constants for the available window types
      integer     NUMWINDT
      parameter (NUMWINDT = 8)                                          ! The number of available window types
      Integer Hamming, Hanning, Gaussian, Bartlett, Rectang
      Integer Blackman, ExctBlck, BlckHarr
      Parameter (Hamming  = 1)
      Parameter (Hanning  = 2)
      Parameter (Gaussian = 3)
      Parameter (Bartlett = 4)                                          ! Triangular Window
      Parameter (RectAng  = 5)
      Parameter (BlackMan = 6)
      Parameter (ExctBlck = 7)                                          ! Exact Blackman
      Parameter (BlckHarr = 8)
c
c..                              Parameters for the window options
      Integer    NUMWOPT
      parameter (NUMWOPT = 3)                                           ! The number of windowing options
      Integer  BYA, BYK, BYW
      Parameter (ByA = 1)
      Parameter (ByK = 2)
      Parameter (ByW = 3)
C PROGRAM
c
      integer iwindo, iwopt
      real    vst, vend
      integer stPt, EndPt
      real    buf(stPt:EndPt)
      integer dir
c
      parameter (DUMV = 0.)
c
      integer DirLcl
      real    Fwindw
c
      if (endPt.lt.StPt) then
        print *, '*** FKFIEX (FrmWnd) ERROR *** ',
     *           ' FrmWnd stPt > Endpt', StPt,endPt
        STOP
      endif
c
      rk = float(k)                                                     ! Convert wavenumber to real
c
      if ((dir.eq.PCDIR).OR.(dir.eq.CPDIR)) then                        !.... Simple window
        if (iwopt.eq.BYA) then
            buf(StPt) = Fwindw(ATan(stPt/rk), ATan(vst), ATan(vend),
     *                          iwindo, dir, INITWDW )
c
        do 10 i = StPt, EndPt
  10      buf(i) = Fwindw(ATan(i/rk), Dumv, Dumv, iwindo,dir, NOINIT)
        else if (iwopt.eq.BYW) then
          buf(StPt) = Fwindw(float(StPt), rk*vst, rk*vend, iwindo,
     *                                                  dir, INITWDW )
          do 20 i = StPt, EndPt
  20        buf(i) = Fwindw(float(i), Dumv, Dumv, iwindo, dir, NOINIT)
        else if (iwopt.eq.BYK) then
          do 30 i = StPt, EndPt
  30        buf(i) = Fwindw( rk, i/vst, i/vend, iwindo, dir, INITWDW )
        endif
c
      else                                                              !........ Filter lines across vinf
        if(dir.eq.PCINF) then
          dirLcl = PCDIR
        else
          dirLcl = CPDIR
        endif
c
        if (iwopt.eq.BYW) then
           print *,'**** FKFILT(FrmWnd) Warning *** ',
     *                'Cannot window as a fn. of w across v = inf'
           print *, 'Using Angle (BYA) instead'
        endif
c
        if (iwopt.eq.BYA .OR. iwopt.eq.BYW) then
           buf(StPt) = Fwindw(ATan(rk/stPt), ATan(1./vst),
     *                         ATan(1./vend), iwindo, dirLcl,INITWDW)
             do 110 i = StPt, EndPt
 110         buf(i) = Fwindw(ATan(rk/i), DumV, DumV, iwindo,
     *                                                dirLcl, NOINIT)
        else if (iwopt.eq.BYK) then
          do 120 i = StPt, EndPt
 120       buf(i) = Fwindw(rk, i/vst, i/vend, iwindo, dirLcl,INITWDW)
        endif
      endif                                                             ! of (dir == PCDIR) | (dir == CPDIR)
      RETURN
      end
c++
c++
      Real Function Fwindw(theta, start, end, iwindo, dir, init)
c------------------------------------------------------------------------------
c   Fwindw returns the value of a window function . The windows supported
c are the standard SIOSEIS ones.
c
c Inputs:
c   theta  Value within the window. Within Fwindw it is normalized to [0,1] wrt
c          the total range of the window |end - start|
c          = 0  => complete pass
c          = 1  => cut off end of window
c
c   start  Start value of window. Used only on Initialization
c   end    End   value of window.  "    "    "    "    "    "
c   iwindo The type of window to be applied
c   dir    Direction of windowing
c          = PCDIR - The window is from pass to cut
c          = CPDIR - The window is from cut to pass
c
c Last Modified:
c   10/6/88 ajh.
c------------------------------------------------------------------------------
c
      real    theta
      real    start, end
      integer iwindo
      integer dir
c
      Parameter ( PI    = 3.1415927)
      Parameter ( TWOPI = 2. * PI)
      Parameter ( BETA  = 0.5)                                          ! Width constant for Gaussian window
c
      integer    DirLcl
c
      save thetaSt, thetaEnd, Range, b2
c
C INCLUDE FILES
c fkfilt.inc
c Include file for the FK filter routines FKFIED & FKFIEX
c
      integer    NSETSP, DBLSETSP
      parameter (NSETSP = 20 )                                          ! The max. no. of FKFIED input values
      parameter (DBLSETSP  = 2*NSETSP)
c
      integer    SINGPARM                                               ! The number of single valued params
      parameter( SINGPARM = 6)
c
      integer     FILNOTST , FILTERR, FILTVEL, FILTDIP
      parameter ( FILNOTST  = -8)                                       ! Declare constants for the filter types
      parameter ( FILTERR   = -7)
      parameter ( FILTVEL   =  1)
      parameter ( FILTDIP   =  2)
c
      integer    CUT, PASS, ATINF
      parameter (CUT = 2, PASS = 1)
      parameter ( ATINF = 2)                                            ! Constant signifying lines near v = inf
c
      Integer PCdir, CPdir, PCinf, CPinf
      Parameter (PCdir =  Cut - Pass)
      Parameter (CPdir = Pass - Cut)
      Parameter (PCinf = AtInf * PCdir)
      Parameter (CPinf = AtInf * CPdir)
c
      Integer INITWDW
      Parameter (INITWDW = 32)
      Parameter (NOINIT  = 0)
c
c..                         Declare constants for the available window types
      integer     NUMWINDT
      parameter (NUMWINDT = 8)                                          ! The number of available window types
      Integer Hamming, Hanning, Gaussian, Bartlett, Rectang
      Integer Blackman, ExctBlck, BlckHarr
      Parameter (Hamming  = 1)
      Parameter (Hanning  = 2)
      Parameter (Gaussian = 3)
      Parameter (Bartlett = 4)                                          ! Triangular Window
      Parameter (RectAng  = 5)
      Parameter (BlackMan = 6)
      Parameter (ExctBlck = 7)                                          ! Exact Blackman
      Parameter (BlckHarr = 8)
c
c..                              Parameters for the window options
      Integer    NUMWOPT
      parameter (NUMWOPT = 3)                                           ! The number of windowing options
      Integer  BYA, BYK, BYW
      Parameter (ByA = 1)
      Parameter (ByK = 2)
      Parameter (ByW = 3)
C PROGRAM
c
c...                                Initialize the window variables
      DirLcl = dir
c
      if ( init.eq.INITWDW ) then                                       ! Test for INIT bit
c****  pcdir =1, cpdir = -1
        if (DirLcl.eq.PCDIR) then
          thetaSt  = start
          thetaEnd = end
        else if (DirLcl.eq.CPDIR) then
          thetaSt  = end
          thetaEnd = start
        else
          stop '*** Error *** Fwindw incorrect direction specified'
        endif
c
        range = thetaEnd - thetaSt
        if (iwindo.eq.GAUSSIAN)
     *         b2 = 2 * BETA * BETA
      endif
c
      thetaN = (theta - thetaSt) / range                                ! Normalize theta to [0,1]
c
      if ( thetaN .lt.0.) then
        print '(A,G12.3)',
     *        '*** Fwindw Warning *** Normalized theta < 0 ', ThetaN
        thetaN = 0.
      else if (thetaN.gt.1.) then
        print '(A,G12.3)',
     *        '*** Fwindw Warning  *** Normalized theta > 1 ', ThetaN
        thetaN = 1.
      endif
c
      if (iwindo.eq.HAMMING) then
         Fwindw = 0.54 + 0.46 * Cos(PI * thetaN)
      else if (iwindo.eq.HANNING) then
         Fwindw = 0.5 + 0.5 * Cos(PI * thetaN)
      else if (iwindo.eq.GAUSSIAN) then
         Fwindw = Exp( -thetaN*b2)
      else if (iwindo.eq.BARTLETT) then
         Fwindw = 1. - ThetaN
      else if (iwindo.eq.RECTANG) then
         Fwindw = 1.
      else if (iwindo.eq.BLACKMAN) then
         Temp = PI * ThetaN
         Fwindw = 0.42 + 0.5 * Cos(temp) + 0.08 * Cos( 2 * temp)
      else if (iwindo.eq.EXCTBLCK) then
         Temp = PI * ThetaN
         Fwindw = 0.42659071 + 0.49656062 * Cos(temp) +
     *             0.07684867 * Cos( 2 * temp)
      else if (iwindo.eq.BLCKHARR) then
         Temp = PI * ThetaN
         Fwindw = 0.35875 + 0.48829 * Cos(temp) +
     *             0.14128 * Cos( 2 * temp) + 0.01168 * Cos(3 * temp)
      endif
c
      RETURN
      end
