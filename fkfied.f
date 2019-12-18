      subroutine fkfied ( scr, lscr )
c-------------------------------------------------------------------------------
c
c                            PROCESS FKFILT
c                            ------- ------
c
c  Document Date:
c
c       FKFILT calculates and applies a filter in the frequency-wavenumber (FK)
c  domain.
c
c       Currently the only type of filter that is implemented is a fan filter or
c  pie slice filter. This type of filter is useful for removing or retaining
c  signals travelling across the seismic line at certain phase velocities. The
c  filter is defined in terms of a series of lines from the origin which
c  delimit pass and cut slices of the filter. In between a cut and pass region
c  the filter response is tapered according to a chosen window function.
c
c    To define a fan filter, the filter lines may be given either in terms of
c  velocity or in terms of dip. The cut and pass lines may be input in
c  any order and will be sorted and checked for consistency.
c  For velocity the filter region runs
c                v :  0- -> -inf / +inf -> 0+
c  While for dips it runs
c              dip :  -inf -> 0 -> +inf.
c  ( Remember horizontal events have 0 dip & infinite velocity. While steeply
c    dipping events have small velocity. )
c
c    It is a mistake to define a filter that, when sorted, consists of 3 or more
c  lines of the same type within the body of the filter or 2 or lines of the
c  same type at either end
c
c       Refer to two articles in the January 1983 "First Break" for more details
c  on both the FK domain and FK filtering.
c
c  The Parameter Dictionary:
c  --- --------- ----------
c
c  DipCut - The dip of the lines defining the FK region(s) to be removed. Dip
c           is measured in mS per trace
c
c  DipPas - The dip of the lines defining the FK region(s) to be retained.
c           e.g  DipPas -1 1 DipCut -2 2 will retain only events with small dip
c
c  VelCut - The velocity of the lines defining the FK region(s) to be removed.
c           The units for velocity must be consistent with those used for
c           Deltax.
c
c  VelPas - The velocity of the lines defining the FK region(s) to be removed.
c
c           e.g.  VelCut -100 -900 900 100
c                 VelPas -250 -500 500 250 will retain only arrivals with
c           apparent velocities between +/- 900 & 500.
c
c  Deltax - If the filter is defined using VelPas/VelCut the deltax must be
c           given.
c
c  Window - The type of window to use when tapering.
c         = HAMM, Hamming
c         = HANN, Hanning
c         = BART, Bartlett (triangular)
c         = RECT, Rectangular (box car - no window).
c         = BLAC, Blackman
c         = EBLA, Exact Blackman
c         = BLHA, Blackman-Harris
c           PRESET = HANN,  e.g. window rect
c
c  WinOpt - The windowing may be done as a function of Angle, Wavenumber or
c           frequency. However a window that span infinite v cannot be tapered
c           as a function of frequency
c         = ByA - As a function of angle
c         = ByK - As a function of wavenumber
c         = ByW - As a function of frequency
c           PRESET = ByA
c
c  END    - Terminates each parameter list.
c
c
c  Written and COPYRIGHTED (C) by:
c  ALL RIGHTS RESERVED.
c
c Externals:
c           Shindx : Sorts an array using a Shell sort
c-------------------------------------------------------------------------------
c
C INCLUDE FILES
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
      parameter ( NPARS   =  9)                                         ! the number of user parameters
      parameter (INDX1 =  2*NSETSP + 1)                                 ! Offset into the scratch array
      parameter (INDX2 =  4*NSETSP + 1)
      parameter (INDX3 =  6*NSETSP + 1)
      parameter (INDX4 =  8*NSETSP + 1)
      parameter (INDX5 = 10*NSETSP + 1)
c
      dimension scr(*), lscr(*)
c
      character*80 token
      character*4  window, lwinds(NUMWINDT)
      character*4  winopt, lopt(NUMWOPT)

      character*6  names (NPARS)
      character*1  types (NPARS)
      integer      length(NPARS)
      dimension    vals (NPARS), lvals (NPARS)
      equivalence ( vals(1), lvals(1))
c
      common /FKFILT/ munit, nlists, ktrace
      common /EDITS/ ierror, iwarn, irun, now, icompt
      common /READT/ ilun,numhdr,numdat,iunhdr,ireeln,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype
c
      real    DPass(NSETSP), VPass(NSETSP),
     *        DCut(NSETSP),  VCut(NSETSP)
      integer nDPass, nDCut, nVpass, nVcut                              ! The number of each value supplied
      logical found
      integer FiltTyp
c
c
      equivalence ( deltax,  vals(1) ),
     2            ( lprint, lvals(2) ),
c     3            ( window,  vals(3) ),
     4            ( dippas,  vals(4) ),
     5            ( dipcut,  vals(5) ),
     6            ( velpas,  vals(7) ),
     7            ( velcut,  vals(8) )
c     8            ( winopt,  vals(9) )
C
      data names/'DELTAX','LPRINT','WINDOW','      ',
     *           'DIPPAS','DIPCUT','VELPAS','VELCUT',
     *           'WINOPT'/
      data types/'F','L','A',5*'F','A'/
      data length/NPARS*6/
c
      data lwinds/'HAMM','HANN','GAUS','BART','RECT','BLAC','EBLA',
     *            'BLHA'/
      data lopt/'BYA ','BYK ','BYW '/
      data vpass/NSETSP*0./, dpass/NSETSP*0./, dcut/NSETSP*0./,
     *     vcut/NSETSP*0./
      data nDPass,nDCut,nVPass,nVcut /4*0/
C
C..                                      Set The Presets
      deltax  = 0.
      lprint  = 0
      window  = 'HANN'
      winopt  = 'BYA '
      nlists  = 0
      ns      = 0
      FiltTyp = FILNOTST 
c
      call getfil( CREATTMP, munit, token, istat )                      ! File to hold parameters
      call podisc (munit, POSABS, DSKSTRT)
c
c.. Get a list or set of parameters from the user. The current command line in
c.. the SIOSEIS buffer (rdline) may have the parameters, if not, get another
c.. line.
c
      ntokes = 1
  100 CONTINUE
      Call GeToke ( token, nchars )
      Call upcase ( token, nchars )                                     ! Convert to uppercase
      if( nchars .eq. 0 ) then                                          ! Zero length token?
        if( now .eq. 1 ) print *,' <  Enter Parameters  >'
        call rdline                                                     ! Get another line
        ntokes = 0
        GO TO 100                                                       ! & start again
      endif
c
      ntokes = ntokes + 1
      do 190 i = 1, npars                                               ! See if it is a parameter name
        len    = length(i)                                              ! Get the legal parameter name length
        iparam = i                                                      ! Save the index
        if( ( token(1:nchars).eq.names(i)(1:len)) .AND.
     *        (nchars .EQ. len) ) GO TO 200
  190 continue                                                          ! still looking for the name
c
      if ( (token(1:nchars).eq.'END').AND.(nchars.eq.3) )
     *    GO TO 1000                                                    ! end of list?
c
      if ( ns.ne.0 ) GO TO 230                                          ! The last parameter name multivalued?
c
      print *,' ***  ERROR  ***  FKFILT does not have a parameter',
     *    ' named ', token(1:nchars)
      ierror = ierror + 1
      GO TO 100
c
c..                        *** Found The Parameter Name, Now Find The Value ***
  200 CONTINUE
      ns     = 0                                                        ! Reset the multi-valued parameter count
      nparam = iparam                                                   ! save the parameter number
c
  210 CONTINUE
      call getoke ( token, nchars )                                     ! get a token
      call upcase ( token, nchars )                                     ! convert to uppercase
      ntokes = ntokes + 1
      if( nchars.le.0 ) then                                            ! end of line?
          IF( now.eq.1 ) print *,' <  Enter Parameters >'
          call rdline                                                   ! get another line
          ntokes = 0
          GO TO 210
      endif
c
  230 CONTINUE                                                          ! JUMP here if multivalued parameter
      if( types(nparam).eq.'A' ) then                                   ! alpha ?
        if ( names(nparam) .eq.'WINDOW' ) then
           window = token(1:4)
        else if ( names(nparam).eq.'WINOPT') then
           winopt = ' '
           winopt = token(1:nchars)
        endif
C..                                      Numeric Argument
      else
        call dcode( token, nchars, areal, istat )                       ! Convert to binary
        if ( istat .ne. 2 ) then                                        ! => Not a numeric value
          ierror = ierror + 1                                           ! dcode wrote the error message
          GO TO 100
        endif
c
        if ( types(nparam).eq.'F' ) then
          ns           = ns + 1                                         ! Inc. multivalued parameter count
          vals(nparam) = areal                                          ! Assign to the variable
          if( names(nparam) .eq. 'DIPPAS' ) then
            dpass(ns) = areal
            ndpass    = ndpass + 1
          else if( names(nparam) .eq. 'DIPCUT' ) then
            dcut(ns)  = areal
            ndcut     = ndcut + 1
          else if( names(nparam) .eq. 'VELPAS' ) then
            vpass(ns) = areal
            nvpass    = nvpass + 1
          else if( names(nparam) .eq. 'VELCUT' ) then
            vcut(ns)  = areal
            nvcut     = nvcut + 1
          endif
        else
          lvals(nparam) = areal                                         ! must be integer
        endif
      endif
      GO TO 100                                                         ! Get another parameter
c
c*************************                             *************************
c*           Finished A List, Now Do The Error And Validity Checks
c*************************                             *************************
c
 1000 CONTINUE
c
      do 1010 i=1, NUMWINDT                                             ! Convert window type to integer
        iwindo = i
        if ( window .eq. lwinds(i) ) GO TO 1020                         ! is this the one?
 1010 continue
      print *,' ***  FKFILT ERROR  ***',
     *        'Illegal window type of ', window
      ierror = ierror + 1
 1020 continue
c                                                                       ! Convert window option to integer
      found = .false.
      i     =  1
 1025 if (winopt .eq. lopt(i)) then
        iwopt = i
        found = .true.
      endif
        i = i + 1
      if (.not.found .and. i.le.NUMWOPT) go to 1025
c
      if (.not.found) then
        print *,' ***  FKFILT ERROR  *** ',
     *          ' Illegal window option of ', winopt
        ierror = ierror + 1
      endif
c
c..                    Check that either velocities or dips have been specified.
      nvchck = nvcut + nvpass
      ndchck = ndcut + ndpass
      if ( nvchck + ndchck .eq. 0 ) THEN
        print *,' ***  FKFILT ERROR  *** ',
     *      ' Either Velocity (VELPAS & VELCUT) or',
     *      ' Dip (DIPPAS & DIPCUT) filter parameters must be given.'
          ierror  = ierror + 1
          FiltTyp = FILTERR
      endif
c
c*************************                             *************************
c                             Velocity filtering
c*************************                             *************************
c
      if ( nvchck .ne. 0 ) then                                         ! User gave velocity filter parameters
c
        If (FiltTyp.eq.FILNOTST ) then
          FiltTyp  = FILTVEL
        else
          FiltTyp  = FILTERR                                            ! Another type of filter has been given
        endif
c
        if ( deltax .eq. 0. ) then
          print *,' ***  FKFILT ERROR  *** ',
     *            ' DELTAX is required for velocity filtering.'
          print *,' i.e when VELPAS & VELCUT are given.'
          ierror = ierror + 1
        endif
c
        if ( (nvpass.eq.0).OR.(nvcut.eq.0) ) then
          print *,' ***  FKFILT ERROR  *** ',
     *            ' Both VELPAS and VELCUT values must be given.'
          ierror = ierror + 1
        else
c
c...      Sort the Cut & Pass Lines into ascending order. The sorted lines are
c         returned starting at scr(INDX1), and the corresponding types starting
c         at lscr(INDX2)
c
          call srtln ( vPass,vCut,nVPass,nVCut,NSETSP,scr(INDX3),
     *                lscr(INDX4),lscr(INDX5),scr(INDX1),lscr(INDX2))
c
c..                  Check there are no inconsistencies in the definitions
          call ChkOrd (FILTVEL,'VelPas','VelCut',scr(INDX1),
     *                    lscr(INDX2), nvchck)
c
          nLines = nvchck                                               ! Save the Total number of lines specified
        endif
      endif                                                             ! of if nvchck <> 0
c
c*************************                             *************************
c                              Dip filtering
c*************************                             *************************
c
      if ( ndchck .ne. 0 ) then                                         ! User gave velocity filter parameters
        If (FiltTyp.eq.FILNOTST ) then
          FiltTyp  = FILTDIP
        else
          FiltTyp  = FILTERR                                            ! Another type of filter has been given
        endif
c
        if ( (ndpass.eq.0).OR.(ndcut.eq.0) ) then
          print *,' ***  ERROR  ***  Both DIPPAS and DIPCUT ',
     *        'values must be given.'
          ierror = ierror + 1
        else
c
c...      Sort the Cut & Pass Lines into ascending order. The sorted lines are
c         returned starting at scr(INDX1), and the corresponding types starting
c         at lscr(INDX2)
c
          call srtln(dPass,dCut,ndPass,ndCut,NSETSP,scr(INDX3),
     *                lscr(INDX4),lscr(INDX5),scr(INDX1),lscr(INDX2))
c
c..             Check there are no inconsistencies in the definitions
c
          call ChkOrd(FILTDIP,'DipPas','DipCut',scr(Indx1),
     *                    lscr(Indx2), ndchck)
          nlines = ndchck
        endif
      endif                                                             ! of ndchck <> 0
c
      if ( FiltTyp.eq.FILTERR) then                                     ! More than one type of filter given
        print *,' ***  ERROR  ***  Only one type of FK filter ',
     *           ' may be specified.'
        ierror  = ierror + 1
      endif
c
c*************************                             *************************
c*                         Output the parameters
c*************************                             *************************
c
       scr(1) = deltax
      lscr(2) = iwindo
      lscr(3) = lprint
      lscr(4) = iwopt
      lscr(5) = FiltTyp
      lscr(6) = nLines
c
      if ( IAND(lprint,1) .eq. 1 ) then                                  ! Print debug information
          print *,' '
          print *,' FKFILT parameters:',scr(1),(lscr(i),i=2,6)
          print *,' DipPas ',(dPass(i),i= 1, ndPass)
          print *,' DipCut ',(dCut(i), i= 1, ndCut)
          print *,' velpas ',(vpass(i),i= 1, nvPass)
          print *,' VelCut ',(vcut(i), i= 1, nvCut)
c
          If (FiltTyp.eq.FiltErr) then
             print *,' *** Error *** specifying filter lines'
          else
            print '(/A)',' Sorted Filter Lines'
            do 1100 i = 0, nlines-1
              if (lscr(Indx2+i).eq.Cut) then
                print '(3X,A,F10.3)', ' Cut line at ', scr(Indx1+i)
              else
                print '(3X,A,F10.3)', 'Pass line at ', scr(Indx1+i)
              endif
 1100       continue
          endif
      endif
c
c..  Write this set of parameters to disk
      call wrdisc( munit, scr, SINGPARM )                               ! scr + lscr
c
      if (FiltTyp.ne.FILTERR) then
        call wrdisc( munit, scr(INDX1), 2*NSETSP )                      ! Line Values
        call wrdisc( munit, lscr(INDX2), 2*NSETSP )                     ! Line Types Pass/Cut
      endif
      nlists = nlists + 1
      ns     = 0
C
C                                                                       ! Found an END for parameter list
 2020 call getoke( token, nchars )                                      ! get the next token
      call upcase( token, nchars )
      ntokes = ntokes + 1
      if ( nchars.eq.0 ) then                                           ! was it the end of a line?
          if ( now .eq.1 ) print *,' <  Enter Parameters  >'
          call rdline                                                   ! get another line
          ntokes = 0
          GO TO 2020
      endif
c
      if ( (token(1:nchars).ne.'END').OR.( nchars.ne.3) ) then
        print *,' ***  ERROR  ***  FKFILT permits only one list of',
     *    ' parameters to be given.'
        ierror = ierror + 1
      endif
      RETURN
      end
c**
c**
      Subroutine srtln(xpass,xcut,npass,ncut,nmax,xmix,nmix,indx,
     *                     xsort,nsort)
c-------------------------------------------------------------------------------
c    srtln mixes together & sorts the input cut/pass lines of FKFIED into
c ascending order
c
c Inputs:
c    xpass(nmax)      Array holding the Pass lines
c    xcut(nmax)       Array holding the cut  lines
c    npass            The number of pass lines
c    ncut             The number of cut  lines
c    nmax             The maximum/possible number of pass/cut lines
c    xmix(2*nmax)     Scratch array to hold the combined Pass/Cut lines
c    nmix(2*nmax)     Scratch array to hold line type of combined data
c    indx(2*nmax)     Scratch array used as Index array of the sort
c
c Outputs:
c    xsort(2*nmax)     Array of sorted Pass/Cut lines
c    nsort(2*nmax)     Array of corresponding sorted line types
c
c Call Chain
c   CONTRO:FKFIED:SRTLN
c
c Last Modified
c    10/6/88 ajh.
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
      real     xpass(nmax), xcut(nmax)
      integer  npass, ncut
      real     xmix(2*nmax)
      integer  nmix(2*nmax)
      integer  indx(2*nmax)
      real     xsort(2*nmax)
      integer  nsort(2*nmax)
c
      integer    ASCEND
      parameter (ASCEND = 1)
c
      nLine = npass + ncut                                              ! Put the separate lines into the works arrays
      do 10 i = 1, npass
        xmix(i) = xPass(i)
        nmix(i) =  PASS                                                 ! Keep track of the line type
 10   continue
c
      IndxOff = nPass
      do 20 i = 1, nCut
        xmix(i + IndxOff) = xCut(i)
        nmix(i + IndxOff) = CUT
 20   continue
c
c.. Shindx indexes the array xmix in ascending order. The index is returned
c   in indx. So the indx of the i th smallest value of xmix is given indx(i)
c
      call Shindx(nLine, xmix, indx, ASCEND)
c
      do 30 i = 1, nLine                                                ! Arrange values in sorted order for output
               j = indx(i)
        xsort(i) = xmix(j)
        nsort(i) = nmix(j)
  30  continue
      RETURN
      end
c**
c**
      Subroutine ChkOrd(FiltTyp, cType1, cType2, xLine, xType, nLine)
c
c-------------------------------------------------------------------------------
c    This routine checks the ordering of the sorted pass & cut lines to ensure
c that there has been no error in the input specification. Essentially this
c requires that there aren't multiple (2 or more) lines of the same type pass
c or cut defined at the end of the fan, or that there is 3 or more lines of the
c same kind are not defined within the fan.
c
c    Conceptually for filters defined in terms of velocity the lines of the fan
c run from
c           v = 0- -> -inf / +inf -> 0+.
c
c while for filters defined in terms of dips the fan runs from
c           dip = -inf -> 0 -> +inf
c
c   Since the input lines are always sorted from -inf to +inf this order is fine
c as is for dips but the ends of the fan 0- & 0+ are in the center of the sorted
c velocity lines.
c
c Inputs:
c    FiltTyp          The filter type Velocity/Dip
c    cType1, cType2   The names of the filter lines as used in the input file.
c                     Used by the error output
c    xLine(0:nLine-1) The values of the input lines
c    xType(0:nLine-1) The types of the input lines
c    nLine            The number of input lines
c
c Call Chain:
c   CONTRO:FKFIED:CHKORD
c
c Date Last Modified:
c    10/6/88 ajh.
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
      parameter (TINY = 1.E-6)
c
      Integer       FiltTyp
      Character*(*) cType1, cType2
      Real          xLine(0:nLine-1)
      Integer       xType(0:nLine-1)
c
      common /EDITS/ ierror, iwarn, irun, now, icompt                   ! need access to ierror
c
      integer PreTyp
      logical found
      common /CHECKB/ istart, nTop                                      ! Used by ChkErr
c
       nLineM = nLine - 1
       nTop   = nLineM
c
c.. If a velocity filter look for the pair of lines that straddle v = 0 and
c.. define the ends of the fan.
       istart = 0
       if ( FiltTyp.eq.FILTVEL ) then
          found = .false.
          i = 0
  1       i = i + 1
            if (xLine(i)*xLine(i-1).le.0) found = .true.
          if ( (.not.found) .AND.(i.lt.nLineM) ) go to 1
c
          if (found) then                                               ! Found a 2 lines straddling v = 0
            istart = i                                                  ! Start the error check at v > 0 & loop round v(inf)
            print *, xLine(i)
            if(xLine(i).eq.0.0) nTop = nLine                            ! If a line at v = 0 count it at
c                                                                       ! both ends of the filter.
          endif
        endif                                                           ! of FiltTyp == FILTVEL
c
c                                          Check the start of the fan
      PreTyp = xType(istart)
      i = 0
    5 continue
        i  = i + 1
        ii = mod(i+istart,nLine)
      if ( (xType(ii).eq.PreTyp).AND.(i.lt.nTop) ) go to 5
      icount = i
c
      if (icount.ge.2) then                                             ! >=2 lines the same at start of fan
        print 110
        call ChkErr(PreTyp,xLine,nLine,0,icount-1,cType1,cType2)
        ierror = ierror + 1
      endif
c                                          Check the body of the fan
      icount  = 1
      ibeg    = i + 1
      PreTyp  = xType(ii)
c
      do 10 i = ibeg, nTop
        ii = Mod(i+istart,nLine)
        if (xType(ii).eq.PreTyp) then
          icount = icount + 1
        else                                                            ! A change in the line type
          if (icount.ge.3) then                                         ! 3 or more of the same type in a row ?
            print 100
            call ChkErr(PreTyp,xLine,nLine,i-icount,i-1,cType1,cType2)
            ierror = ierror + 1
          endif
          icount = 1
          PreTyp = xType(ii)
        endif
  10  continue
c
      if (icount.ge.2) then                                             ! Check the end of the fan
        print 110
        call ChkErr(PreTyp,xLine,nLine,nTop-icount+1,nTop,
     *                cType1,cType2)
        ierror = ierror + 1
      endif
      RETURN
c
 100  format (' *** ERROR *** 3 or more lines of the ',
     *        'same type (Pass/Cut) within the fan' )
 110  format (' *** ERROR *** 2 or more lines of the same type',
     *        ' (Pass/Cut) at the end of the fan')
      end
C**
C**
      Subroutine ChkErr(PreTyp,xLine,nLine,j1,j2,cType1,cType2)
c-------------------------------------------------------------------------------
c   ChkErr is called by routine ChkOrd to print diagnostic messages
c locating the position of the error in the input specfications. It prints the
c input type & values that caused the error.
c
c Inputs:
c    PreTyp           The type of input line for which an error is printed Pass
c                      /Cut
c    xLine(0:nLine-1) The values of the input lines
c    nLine            The number of input lines
c    j1,j2            The range of lines for the error message
c    cType1, cType2   The names of the filter lines as used in the input file.
c
c Call Chain:
c
c Date Last Modified:
c    14/3/89 ajh.
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
      integer       PreTyp
      real          xLine(0:nLine-1)
      character*(*) cType1, cType2
c
      common /CHECKB/ istart,nTop
c
      if (PreTyp.eq.PASS) then
        print '(3A,(5F10.3))', 'Error In ',cType1,' ',
     *                           (xLine(Mod(j+istart,nLine)),j = j1, j2)
      else
        print '(3A,(5F10.3))', 'Error In ',cType2,' ',
     *                           (xLine(Mod(j+istart,nLine)),j = j1, j2)
      endif
      RETURN
      end
