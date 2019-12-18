      Subroutine TX2FED
C
C-------------------------------------------------------------------------------
C                              Process TX2FK
C                              ------- -----
C
C  Document date: 28th October 1991    version 2.1    a.j. harding
C
C     Process TX2FK transforms data from the TX (time-space) domain into the
C  FK (frequency-wavenumber) domain.  The input is a set of normal (t-x)
C  seismic traces and the output is a transformed set of FK traces. The
C  transformed output traces are in rectangular form unless polar form
C  (amplitude and phase) are requested.
C
C     The frequencies within each output trace are ordered from -Nyq to 0 to
C  Nyq.  Each output trace contains a power of 2 number of samples. The traces
C  are ordered in wavenumber from 0 to k (Nyquist). Data in polar
C  form are ordered with the modulus followed by the argument.
C
C     Any SIOSEIS process may follow TX2FK, but care should be taken that it
C  makes sense!  The imaginary part of data in rectangular coordinates may be
C  omitted from plot by decimating by a factor of 2.
C
C     The steps used in the FK transformation are:
C       1.)  Each trace is windowed temporally in order to minimize edge
C            effects along the time trace.
C       2.)  The data is transposed so that all the data of constant times are
C            adjacent. This sorts the data by x rather than t (or f).
C       3.)  The 'range' traces are zero padded. Effectively adding dummy traces
C            to the end of the dataset. The data must be padded to a power of 2.
C            but additional padding may be desirable for migration etc. A user
C            minimum number of dead traces is specified by nxpad. The
C            data is also windowed in x if desired.
C       4.)  The complex fft is performed, transforming x to k, or from
C            space to wavenumber.
C       5.)  The data is transposed back to ordering by time.
C       6.)  The forward fft is performed, converting time to frequency.
C       7.)  The data is converted to polar coordinates if requested.
C       Data in the fk domain may be processed by any other seismic process in
C  sioseis.
C
C     The run time of TX2FK is governed by the number of traces, including pads,
C  and the trace length, including pads.  Both dimensions are a power of 2, so
C  transforming 1500 points takes as long as 2000 points (2048 being the
C  closest larger power of 2).
C
C    The frequency-wavenumber domain is discussed in a paper "a review of the
C  two-dimensional transform and its use in seismic processing" by D.W. March
C  and A.D.Bailey in the "First Break", January 1983.
C
C     Each parameter list must be terminated with the word end.  The entire set
C  of TX2FK parameters must be terminated by the word end.
C
C  The Parameter Dictionary
C  --- --------- ----------
C
C  STIME  - The start time of the data for the entire data set.  Any trace
C           that has an intial time (delay) greater than stime will be zero
C           padded so that the data starts at stime.  Any trace that has a
C           delay greater than stime will be truncated.
C           preset = the delay of the first trace.
C
C  ETIME  - The end time of the data for the entire data set. Data in excess
C           of etime will be omitted from the transformation.
C           preset = the last time of the first trace.
C
C  NXPAD  - the number of dummy traces to insert at both ends of the seismic
C           line.  Process fkmigr needs dummy traces in order to prevent
C           "wrap around".
C           preset = 10
C
C  TWINDOW - The type of window to apply before computing the temporal fft.
C          = HAMM, hamming
C          = HANN, hanning
C          = GAUS, gaussian
C          = BART, bartlett (triangular)
C          = RECT, rectangular (box car - no window)
C          = BLAC, blackman
C          = EBLA, exact blackman
C          = BLHA, blackman harris
C            preset=HANN  e.g. window RECT
C
C  TWINLEN - The window length, in seconds.  a window length of zero causes
C           the entire time domain trace to be windowed.  A non zero length
C           indicates that winlen data will be modified at both ends of each
C           trace.
C           preset=.1  e.g. winlen .2
C
c  XWINDOW - Same as Twindow only it windows the data by range.
C            preset = RECT ( No windowing of ranges )
C
C  XWINLEN - The window length in number of traces. A window length of zero
C            causes the entire set of ranges to be windowed. A non zero length
C            causes that number of traces to be windowed at each end.
C            preset = 10
C
C  COORDS - The coordinates of the output trace.
C
C         =RECT, Rectangular coordinates.  The output trace will be complex.
C                The trace values will consist of real and imaginary pairs.
C                The frequency dimension runs from 0 to nyquist (pi) then the
C                negative frequencies back to 0, as output from the fft.  The
C                output frequency trace is a poiwer of two.  The k dimension
C                is also ordered as it comes directly out of the fft, 0 to
C                nyquist followed by the most negative k.
C
C         =POLAR,Polar cordinates.  The first half of the output trace will be
C                the amplitude spectrum and the second half of the trace will
C                be the phase spectrum, each ordered 0 to nyquist back to 0.
C                The k dimension is reordered so that the first trace is the
C                negative nyquist, then increasing to 0, then increasing to
C                the positive nyquist.  The total number of traces output
C                is a power of two plus 1.
C
C        = POLARU, "user friendly" polar coordinates. This produces output data
C                as a series of wavenumber traces running from -Nyq -> +Nyq
C                and only positive frequencies in polar form. This format is
C                useful if it is desired to look at the FK data using for example
C                a contouring program. The data is not used internally in this
C                form but is converted on input/output from SIOSEIS.
C
C           preset = RECT   e.g.  coords POLAR
C
C  OHDRPATH - If processes TX2FK and FK2TX are called in the same job then the
C             original TX headers will be used as the trace headers of the
C             the processed TX data. This will preserve all RP/Shot numbering
C             as well as GMT information. However, if these two processes are
C             done in separate jobs then the user may specify a permanent disk
C             file to hold the original TX headers. This filename can then
C             be given to FK2TX when the inverse transform is done.
C
C  PATH1 & PATH2 - Before TX2FK transforms the input traces it accumulates them
C                them in a scratch datafile. The size of this file is the number
C                of input traces prior to padding in range * The sample length
C                of each trace (etime - stime) prior to padding in time.
C
C                  The size of the data after transformation is equal to the
C                number of traces * trace length after padding of each to the
C                nearest power of 2. This is stored in a second scratch dataset
C                after which the first scratch file is deleted. If there is a
C                FK2TX in the procs list then it will use this second scratch
C                file as its 1st scratch file.
C
C                   The default location of these files is implementation
C                dependent and may be of insufficient size. The PATH parameters
C                allow the user to specify the location of the two scratch files
C
C  PATH1    The location of the 1st scratch file.
C
C  PATH2    The location of the 2nd scratch file. Since this is the same as the
C            first scratch file used by FK2TX (if present) it should be
C            given only here or in FK2TX. If contradictory definitions are
C            given the first on processed will be used.
c  PRESTK - When set nonzero, the FK transformation is done whenever the
c           PRESTK number of "gathers" have been collected.  A "gather" is
c           whenever the "end-of-sort" flag (SEGY header word 51) is -1.
c           Processes SORT and GATHER set the "end-of-sort" flag.  Even
c           shot data must have gone through SORT in order to have word
c           51 set.
C
C  END    - terminates each parameter list.
C
C Last Modified
C    10/20/88 ajh. ( Corrected window errors)
c    10/28/91 pch  add prestk
c    4/5/96 pch.  change prestk from switch to a counter.
c    8 Nov 06 pch - Change time window length from time to samples.
c    21 Apr 09 - Make prestk > 1 an error
C-------------------------------------------------------------------------------
C INCLUDE FILES
c INCLUDE seisio.inc
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
c INCLUDE txfk.inc
C Include file TXFK.INC
C include file for the sioseis 2D FFT and fk migration routines
C
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
c INCLUDE txfkgbl.inc
c.. This is the global common block for the FK routines. At present it is used
c.. by TX2FEX, FK2TEX & their associated edit routines to indicate whether they
c.. have been called & to pass file unit numbers.
c
c  Last Modified 10/18/88
c
      logical txinit, fkinit                                            ! Execution initialization ?
      logical txed, fked                                                ! Edit initialization ?
      integer ltxunt1, ltxunt2                                          ! File Stream ID for tx2fex scratch file
      integer lfkunt1, lfkunt2                                          ! File Stream ID for fk2tex scratch file
      integer txunit                                                    ! Used Stream ID for tx2fex scratch file
      integer ohdrtxfk                                                  ! File unit holding tx headers
      logical tmptxhdr                                                  ! Indicates if tx header file is temporary
      integer prestk                                                    ! Indicates if data is prestack or not
      common /TXFKE/ txinit, fkinit, txed, fked, txunit,
     $               ltxunt1, ltxunt2, lfkunt1, lfkunt2,
     $               ohdrtxfk, tmptxhdr, prestk, ntx2fk, nfk2tx, range
C PROGRAM
      parameter (NPARS  = 15)                                           ! the number of user parameters
      PARAMETER (multiv = 15)
      parameter (MAXWEI = 20)                                           ! the maximum number of weights that may be given
C
C..                Parameters for TX2FK are transferred via common block TX2FKC
      integer twindo, xwindo, xwinle, Twinlen, twinle
      common /TX2FKC/ stim,etim,nxpa,twindo,twinle,xwindo,xwinle,lprin,
     *     nweigs, lcoord, weigs(maxwei)
      common /EDITS/ ierror,iwarn,irun,now,icompt
c
      character*4 lwinds(8)
      character*4 Twindow, Xwindow
      integer     Xwinlen
      character*8 names(NPARS)
      character*1 types(NPARS)
      integer     length(NPARS)
      character*80 token
      dimension vals(NPARS),lvals(NPARS)
      logical flag
      integer status
c
      save /TX2FKC/
C
      equivalence (stime,    vals(1)),
     2            (etime,    vals(2)),
     3            (nxpad,   lvals(3)),
     4            (lprint,  lvals(4)),
     5            (Twinlen,  lvals(5)),
     7            (coords,  lvals(7)),
     8            (Xwinlen, lvals(8)),
     4            (nprestk, lvals(13)),
     *            (weight,   vals(15))
c
      data names/'STIME ','ETIME ','NXPAD ','LPRINT', 'TWINLEN',
     *           'TWINDOW','COORDS','XWINLEN','XWINDOW', 'PATH1',
     *           'PATH2','PRESTK','NPRESTK','OHDRPATH',
     &           'WEIGHT'/
      data types/2*'F', 2*'L','F', 2*'A','L', 4*'A','L','A','L'/
      data length/5,5,5,6,7,7,6,7,7,2*5,6,7,8,6/
      data lwinds/'HAMM','HANN','GAUS','BART','RECT','BLAC','EBLA',
     *            'BLHA'/
C****
C****      set the presets
C****
      stime   = -1.
      etime   = -1.
      nxpad   = 10
      Twinlen = 25
      Twindow = 'HANN'
      Xwinlen = 10
      Xwindow = 'RECT'
      lcoord  = CDRECT
      weight  = -1.
      lprint  = 0
      ns      = 0
      nweigs  = 0
c..                     Set values in FK global common
      call txfkinit
      txed     = .TRUE.
      ltxunt1  = 0
      ltxunt2  = 0
      ohdrtxfk = 0
      nprestk = 0
C****
C****   the current command line in the system buffer may have the parameters.
C****   get a parameter list from the user.
C****
  100 continue
      call getoke(token,nchars)                                         ! get a token from the user parameter line
      call upcase(token,nchars)                                         ! convert the token to uppercase
C
      if(nchars.le.0) then                                              ! get another user parameter line
        if(now.eq.1) print 140
  140   format(' <  Enter Parameters  >')
        call rdline
        go to 100
      endif
C
      do 190 i = 1, npars                                               ! see if it is a parameter name
        len    = length(i)                                              ! get the legal parameter name length
        iparam = i                                                      ! save the index
        if(token(1:nchars).eq.names(i)(1:len).and.nchars.eq.len)
     *  go to 200
  190   continue
C                                                                       ! still looking for the name
      if(token(1:nchars).eq.'END'.and.nchars.eq.3) go to 1000           ! end of list?
      if (ns.ne.0) go to 230                                            ! Multivalued parameter
      print 191, token(1:nchars)
  191 format(' ***  Error  *** TX2FK does not have a parameter ',
     *  'named ',A10)
      ierror = ierror + 1
      go to 100
C****
C****    found the parameter name, now find the value
C****
  200 continue
      ns      = 0
      nparam  = iparam
  210   continue                                                        !  now find the value
        call getoke(token,nchars)
        if(nchars.le.0) then                                            ! end of line?
          if(now.eq.1) print 140                                        ! Allow parameters on a different line
          call rdline                                                   ! get another line
          go to 210
        endif
C
  230 continue
      if( types(nparam).eq.'A') then                                    ! An alpha Parameter
c
        if ( names(nparam).eq.'PATH1') then                             ! Open first Scratch file
          call getfil(CREATNEW,ltxunt1,token,status)
          if (status.ne.0) then
            print *,'***ERROR*** Error opening PATH1: ',token
            ierror = ierror + 1
          endif
c
        else if ( names(nparam).eq.'PATH2') then
          if ( fked.and.(lfkunt1.ne.0) ) then
            ltxunt2 = lfkunt1
            print *,'*** Warning TX2FK **** Second scratch file already'
     $             ,' specified by FK2TX. Using that file'
          else
            call getfil(CREATNEW,ltxunt2,token,status)
            if (status.ne.0) then
              print *,'***ERROR*** Error opening PATH2: ',token
              ierror = ierror + 1
            endif
          endif
c
        else if (names(nparam).eq.'OHDRPATH') then
          call getfil(CREATNEW, ohdrtxfk, token, status)
          if (status.ne.0) then
              print *,'***ERROR*** Error opening OHDRPATH: ',token
              ierror = ierror + 1
          endif
c
        else if( names(nparam) .eq. 'COORDS' ) then
          call upcase(token,nchars)
          if( token(1:nchars) .eq. 'RECT' ) then
             lcoord = CDRECT
          else if( token(1:nchars) .eq. 'POLAR' ) then
             lcoord = CDPOLAR
          else if( token(1:nchars) .eq. 'POLARU' ) then
             lcoord = CDPOLARU
          else
             print *,'*** ERROR *** Illegal COORDS Option'
             ierror = ierror + 1
          endif
        else if ( names(nparam) .eq. 'TWINDOW' ) then 
          call upcase(token,nchars)
          Twindow = token(1:4)
        else if ( names(nparam) .eq. 'XWINDOW' ) then
          call upcase(token,nchars)
          Xwindow = token(1:4)
        else if ( names(nparam) .eq. 'PRESTK' ) then
          call upcase(token,nchars)
          if( token(1:1) .eq. 'Y' ) nprestk = 1
          if( token(1:1) .eq. '1' ) nprestk = 1
          IF( token(1:1) .NE. 'Y' .AND. token(1:1) .NE. 'N' .AND.
     &        token(1:1) .NE. '1' .AND. token(1:1) .NE. '0' ) THEN
              PRINT *,' ***  ERROR  ***  Illegal PRESTK.'
              PRINT *,'                  Did you mean NPRESTK?'
              ierror = ierror + 1
          ENDIF
        endif
      else
        call dcode(token,nchars,areal,istat)                            ! try and decode it
        if(istat.ne.2) then                                             ! =2 means it is a numeric
          ierror = ierror + 1                                           ! dcode printed an error
          go to 100
        endif
        if(types(nparam).eq.'F') then                                   ! floating point values
          if(nparam.eq.multiv) then                                     ! Multivalued parameter
            ns        = ns + 1
            nweigs    = ns
            if(ns.le.maxwei) weigs(ns) = areal
          else
            vals(nparam) = areal
          endif
        else                                                            ! 32 bit integer values
          lvals(nparam) = areal
        endif
      endif
      go to 100
C****
C****   finished a list, now do the error and validity checks
C****
 1000 continue
      flag = .true.                                                     ! Convert Time window to integer code
      i    = 0
 1010 if (flag.and.(i.lt.8)) then
        i = i + 1
        if(Twindow.eq.lwinds(i)) flag = .false.
        go to 1010
      endif
      if (flag) then
        print 1020, Twindow
        ierror = ierror + 1
 1020   format(' ***  ERROR  *** Illegal Time window type : ',A4)
      else
        Twindo = i
      endif
C
      flag = .true.                                                     ! Convert Range window to integer code
      i    = 0
 1015 if (flag.and.(i.lt.8)) then
        i = i + 1
        if(Xwindow.eq.lwinds(i)) flag = .false.
        go to 1015
      endif
      if (flag) then
        print 1025, Xwindow
        ierror = ierror + 1
 1025   format(' ***  ERROR  *** Illegal Range window type : ',A4)
      else
        Xwindo = i
      endif
C
      if(nweigs.gt.maxwei) then                                         ! Number of input weights exceeded
        itemp = maxwei
        print 1040, itemp
 1040   format(' ***  ERROR  ***  Too many taper weights. ',I2,
     *       ' is the maximum.')
        ierror = ierror + 1
      endif
C                                                                       ! Transfer values into common block variables
      stim   = stime
      etim   = etime
      nxpa   = nxpad
      Twinle = Twinlen
      Xwinle = Xwinlen
      lprin  = lprint
      prestk = nprestk
      if(nweigs.eq.0) then
        do 1110 i = 1, 9
 1110   weigs(i) = float(i)/10.
        nweigs   = 9
      endif
C
      if(IAND(lprint,1).eq.1) print *, stim,etim,nxpa,twindo,twinle,
     *   xwindo,xwinle, nweigs,lcoord,prestk,(weigs(i),i=1,nweigs)
C
C                                                                       ! Look for the end of the parameter list
 2020 call getoke(token,nchars)
      call upcase(token,nchars)
      if(nchars.eq.0) then                                              ! End of a line?. Then get another
        if (now.eq.1) print 140
        call rdline
        go to 2020
      endif
      if(token(1:nchars).ne.'END'.or.nchars.ne.3) then
        print *,' ***  ERROR  ***  TX2FK only permits one list to be ',
     *    ' given.'
        ierror = ierror + 1
      endif
      return
      end
