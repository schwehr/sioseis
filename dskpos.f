      subroutine DskPos(entry, TraceNo, MrgParam)
c------------------------------------------------------------------------------
c  This subroutine positions the file pointer at a given place in a disk file
c  It works on the assumption that all traces have the same length of data
c  e.g. FK transformed data.
c
c   It correctly works out alignment on the CRAY (??) where the start of data/
c headers might not lie on word boundaries.
c
c    Actual positioning is achieved with a call to PODISC
c
c Inputs :
c    entry : The entry to DskPos
c          =  DPINIT    : Initialization
c          =  FIRSTHDR  : Position at start of first trace header.
c          =  TRCHDR    : Position at start of Header for TraceNo
c          =  TRCDATA   : Position at start of Data   for TraceNo
c
c   TraceNo : The trace number within the dataset
c
c   MrgParam  : The elements of MrgParam used by ReadHdr are.
c at COMPOFS  : The computer type
c at FMTOFS   : The input format of the data
c at LPRNTOFS : The value of the debug print option
c at DUNITOFS : The file stream of the input data.
c at DDATLOFS : The length of the data trace
c at OHDROFS  : The length of the binary Header. This is a bug fix so as to read
c               old style 100 word headers.
c   MrgParam
c Outputs:
c    Mrgparam : The following MrgParam elements are set by DskPos
c at IDARNOFS : The misalignment parameter. This indicates how many 16 bit words
c                away we are from a 64 bit word alignment on the CRAY
c at CPOSOFS  : The current position within the file
c
c Call Chain:
c   CONTRO:DIEX:MRGFK:DskPos
c   CONTRO:DOEX:spltfk:DskPos
c
c Externals:
c   PODISC
c
c Last Modified
c   12/21/88 ajh.  : Altered Logic so that reinitialization occurred when the
c                    File stream number changed between successive calls
c------------------------------------------------------------------------------
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
c.. Include file for SplitFK, MrgFK & related processes
c
c
c... Computer Type
      integer PRIME, VAXUNIX, APOLLO, VAXVMS, CRAY
      integer CONVEX, IEEE
      parameter (PRIME   = 1)
      parameter (VAXUNIX = 2)
      parameter (APOLLO  = 3)
      parameter (VAXVMS  = 4)
      parameter (CRAY    = 5)
      parameter (CONVEX  = 6)
      parameter (IEEE    = 7)
c
      parameter ( NOOUTP  = -997)                                       ! No Output process yet seen
c
c.. Parameters for DiskPos Subroutine
      integer   DPINIT, FIRSTHDR, TRCHDR, TRCDATA
      parameter ( DPINIT   = 1)
      parameter ( FIRSTHDR = 2)
      parameter ( TRCHDR   = 3)
      parameter ( TRCDATA  = 4)
c
c.. Entries for ReadTrce
      integer RTINIT, READDATA
      parameter(RTINIT   = 1)
      parameter(READDATA = 2)
c
      parameter ( KSPLTLN = 15)                                         ! Number of parameters in the block
c
      integer   COMPOFS, FMTOFS, DUNITOFS, IDARNOFS, DDATLOFS, CPOSOFS
      integer   LPRNTOFS, IDISKOFS, TRCCTOFS, NXOFS
      integer   SINGOFS, MULTOFS
c..                                   Define offsets to the parameters
      parameter ( COMPOFS = 1)                                          !   Computer type
      parameter (  FMTOFS = 2)                                          !   Input data format
      parameter (DUNITOFS = 3)                                          !   Input data file unit
      parameter (IDARNOFS = 4)                                          !   idarn : misalignment on the Cray
      parameter (DDATLOFS = 5)                                          !   No. of samples per output trace
      parameter ( CPOSOFS = 6)                                          !   Current Position of file pointer
      parameter (LPRNTOFS = 7)                                          !   Lprint debug value
      parameter ( NEWSOFS = 8)                                          !   A new start frequency
      parameter ( NEWEOFS = 9)                                          !   A new finish frequency
      parameter (IDISKOFS = 10)                                         !   The output process number
      parameter (TRCCTOFS = 11)                                         !   The number of transferred trace
      parameter (   NXOFS = 12)                                         !   The total number of output traces
      parameter ( SINGOFS = 13)                                         !   Pointer to header comp that is set to 1
      parameter ( MULTOFS = 14)                                         !   Pointer to header comp that records trace No
c
      integer  OHDROFS
      parameter ( OHDROFS = 15)                                         !  Temporary bugfix to read old header types

C PROGRAM
c
      integer TraceNo
      integer entry
      integer MrgParam(*)
c
      logical first                                                     ! Initialized ?
      integer EBChdrL, BinHdrL, TrcHdrL
      integer position                                                  ! Position in file
      integer TrcDatL                                                   ! Length of trace data
      integer TrRem                                                     ! The remainder of the trace modulo 8 byte CRAY words
c                                                                       ! It is either 16 bit or 32 bit words as appropriate
      integer TrLen
c
      save first
      save icompt, ifmt, lprint, idunitL
      save EBChdrL, BinHdrL, TrcHdrL
      save TrcDatL, TrRem, TrLen
      save Ldiv
c
      data first / .TRUE. /
      data idunitL / -999 /                                             ! The file stream No. on the last entry
c
      idunit  = MrgParam(DUNITOFS)
      lprint  = MrgParam(LPRNTOFS)
c
      if( IAND(lprint,2).ne.0) then
        print '(/A)', 'DskPos debug info'
        print *, 'Trace No: ', TraceNo, ' Entry: ',entry
        print *, 'File Stream No: ', idunit
      endif
c
      if (MrgParam(OHDROFS).ne.0) BinHdrL = MrgParam(OHDROFS)
c
      if (first) then
        first = .FALSE.
c
        icompt = MrgParam(COMPOFS)
c
c Set up the lengths for headers
        if (icompt.ne.CRAY) then
          EBChdrL = NORMEBC                                             ! Length in 4 byte words
          BinHdrL = NORMBIN
          TrcHdrL = NORMTHDR
        else
          EBChdrL = CRAYEBC                                             ! Length in 8 byte CRAY words
          BinHdrL = CRAYBIN
          TrcHdrL = CRAYTHDR
        endif
      endif                                                             ! of if first
c
      if ((entry.eq.DPINIT).or.(idunit.ne.idunitL) ) then
        idunitL = idunit
        ifmt    = MrgParam(FMTOFS)
        TrcDatL = MrgParam(DDATLOFS)
c
        if (ifmt.eq.INT16) then
           Ldiv = 4
        else
           Ldiv = 2
        endif
c
        if (icompt.eq.CRAY) then
          if (ifmt.ne.HOSTFP) then
            TrRem = MOD(TrcDatL,2)                                      ! Remainder
            TrLen = TrcDatL / Ldiv                                      ! Length in 8 byte words
          endif
        else
          TrRem = 0
          if (ifmt.eq.INT16) then
            TrLen = TrcDatL / 2
          else
            TrLen = TrcDatL
          endif
        endif
        if (entry.eq.DPINIT) RETURN
      endif
c
      if (entry.eq.FIRSTHDR) then                                       ! Position to read first header
        position = EBChdrL + BinHdrL + DSKSTRT
c
      else if ( (entry.eq.TRCHDR).OR.(entry.eq.TRCDATA) ) then
c
        position = (traceNo - 1) * (TrLen + TrcHdrL)
        if(entry.eq.TRCDATA)
     $     position = position + TrcHdrL
        if (icompt.eq.CRAY.AND.ifmt.ne.HOSTFP) then
          itemp    = (traceNo - 1) * TrRem
          position = position + itemp / Ldiv
          MrgParam(IDARNOFS)= MOD(itemp,Ldiv)
        endif
        position = position + EBChdrL + BinHdrL + DSKSTRT
      endif
c
      call podisc(idunit,POSABS,position)
      MrgParam(CPOSOFS) = position
c
      if( IAND(lprint,2).ne.0) then
        print *, 'Computer: ', icompt, ' Format: ', ifmt
        print *, 'Trace Length: ',  TrcDatL, ' Header Length: ', TrcHdrL
        print *, 'File Position: ', position
      endif
c
      RETURN
      end
