      Subroutine FKMIEX(ibuf,buf,lbuf,scr,scr2,scr3)
C-------------------------------------------------------------------------------
C      FKMIEX is the execution phase of the sioseis process fkmigr. FKMIEX
C performs the Stolt FK migration.  The input data to FKMIEX must be in the FK
C domain (i.e. must have been through process TX2FK).
C
C      FKMIEX is called once for each input k domain complex frequency trace.
C The wavenumbers of the input traces are from 0 to +k Nyquist (The negative
C wavenumbers are not needed since they can be inferred from the positive
C ones). The frequencies of the input trace are from -Nyq -> 0 -> +Nyq.
C
C     FKMIEX basically does some housekeeping for the input traces. The
C main work is done by STOLT for the AP version, and by routine HALE for the
C non AP version.
C
C Known Limitations (10/3/88):
C      The AP version has not been tested.
C
C Inputs:
C    BUF - The complex frequency trace to be migrated
C   IBUF - The integer*2 equivalent of buf. Used to pass trace header info
C   LBUF - The integer*4 equivalent of buf. Used to pass trace header info
C   SCR/ISCR/LSCR/ - Scratch array and its integer equivalents.
C
C Outputs:
C    BUF - contains the migrated trace.
C
C Called from : CONTRO
C
C Externals :
C  PODISC, RDDISC    : Disk i/o routines
C  EXIT
C  INAP
C Non-AP version
C  RECTC, POLARC     : Coordinate conversion routines
C  HALE              : Migration routine
C AP version :
C  VMOV, APPUT, APWD
C  RECT, POLAR       : Coordinate conversion routines
C  STOLT1, STOLT2    : Migration routine
C
C Modifications:
C   10/5/88 Altered the Scratch indices on the call to HALE
C   10/12/88 Corrected Idtype check
c   11/11/88  added DATINAP
C   3/14/89 Changed format so as to read binary data through /READT/
C           and changed txsius to integer
c   24 Jul 01 - Print a message if too much data.  Hale needs 3 complex scratch buffers
c   18 July 07 - Get nwrds from common numdat rather than segy header
c              - Use scr2 and scr3 input trace buffers as scratch buffers.
c   Sep 08 - Use numdat rather than segy header
C-------------------------------------------------------------------------------
C
C INCLUDE FILES
c.. Include file defining constants of use globally throughout SIOSEIS
c
c Define the sizes of some Large common blocks

      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 5000000)                                     ! Size of /APMEM/ : AP memory
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
C PROGRAM
      Parameter (NSCLRS = 7)
      Parameter (PI = 3.141592654)
C
      common /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap,
     *               idecim
      common /READT/ilun,numhdr,numdat,iunhdr,ireelm,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc
      common /FKMIGR/ v,nfint,lprint,dx,dt, realk
      common /APMEM/ ap(SZAPMEM)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite, maxsamps,nbperw
C
      dimension lbuf(111),buf(111), scr(111), scr2(111), scr3(111)
      integer*2 ibuf(111)
      integer   txsius
      integer   LocCoord
      real      sclrs(NSCLRS)
      logical   first
      save
C
      data first /.true./, nxdone/0/
C
      if (first) then                                                   ! FKMIGR Initialization
        first = .false.
c
c..                       Read the time delay/sample interval from READT common
        txsius = itxsi                                                  ! t-x sample interval in us
        delay  = itxdel/1000.                                           ! & time delay in secs.
c
        if (idtype.eq.IDFKRCT) then
           LocCoord = CDRECT
        else if ((idtype.eq.IDFKPLR).OR.(idtype.eq.IDFKPLRU)) then
           LocCoord = CDPOLAR
        else
          print *, ' *** FKMIGR Error *** ',
     *           ' The input data is not in the FK domain.'
          STOP
        endif
        nv    = 1                                                       ! Only a single velocity allowed
        nx    = 2* (nfktrc-1)                                           ! Calcalate no. of output traces
        if(dt.le.0.)
     *    dt  = FLOAT(txsius)/1.0E6                                     ! Delta t
        dk    = 2.*pi/(float(nx)*dx)                                    ! Delta k (distance between wavenumbers)
c        nwrds = ibuf(ISAMPPTR)                                          ! No. of 32 bit words in the complex trace
c        IF( IAND(ibuf(ISAMPPTR),32768) .NE. 0 ) nwrds = lbuf(29)
        nwrds = numdat
        IF( nwrds .GT. 32768 ) THEN
            PRINT *,' ***  ERROR  ***   Too many samples for FKMIGR.'
            PRINT *,' The maximum number of samples allowed is 16384.'
            STOP
        ENDIF
        nw    = nwrds / 2                                               ! The number of frequencies
        IF( nw .LE. 0 ) THEN
            PRINT *,' ***  ERROR  ***  Bad nw in fkmigr. ',nw
            STOP
        ENDIF
        dw    = 2.*pi/(float(nw)*dt)                                    ! Delta w (distance between frequencies)
        to    = delay * dw                                              ! The delay normalized wrt sampling rate
        vnorm = v*v / (4.0*dw*dw)                                       ! (v/2 )** 2  normalized wrt sampling rate
        nt    = nwrds / 2                                               ! No. of time samples
c        IF( nwrds*3 .GT. maxsamps .OR. nwrds .LE. 0 ) THEN
c            PRINT *,' ***  ERROR  ***  Bad number of frequencies of ',
c     &        nwrds,' in FKMIGR.'
c            PRINT *,' Max number of frequencies is ',maxsamps/6,
cc****       remember the data are now complex and HALE needs 3 scratch buffers.
c     &        ' but have ', nw
c            PRINT *,' (Keep the number of times samples < 8192.)'
cc   If maxsamps is 64K, which is 32K complexes,
c            STOP
c        ENDIF
        realk = 0.                                                      ! Wavenumber of the initial input trace
C
        if( iuseap .eq. USEAP) then                                     ! AP specific initialization
          sclrs(1) =  0.00001
          sclrs(2) =  0.99999
          sclrs(3) =  nfint/2-1
          sclrs(4) = -nw/2
          sclrs(5) = -nw-2
          sclrs(6) =  nw
          sclrs(7) =  to
C
C..                                 Set up the AP addresses - use all of the AP
          iastrt   = 0
          iavsks   = iastrt
          iap      = iavsks + nv
          iaq      = iap + nwrds
          iasclr   = iaq + nv*nwrds                                     ! Put scalars after q, which is nwrds long
          iaspac   = iasclr + NSCLRS                                    ! Scratch area after scalars (nsclrs long)
          iaend    = iaspac + nwrds + nw*13 + 4 + NSCLRS-1              ! Last AP address
C
C..                                                                     ! Put the trace in the ap and make sure it fits
          call inap(buf(numhdr+1),nwrds)
          if(nextad-in.ne.nwrds/idecim*2) then                          ! Assume inap allocates 2 traces
            print *,' ***  FKMIGR WARNING  *** ',
     *        'Some previous AP process is going to be overwriten',
     *        ' Eliminate the pre FKMIGR AP process.'
          endif
          if(iaend.ge.lapsiz) then
            print 120,iaend,lapsiz
  120       format(' ***  FKMIGR ERROR  ***',
     *             ' Too much AP is requested.',I6,
     *              'requested, Max allowed is',I6)
          endif
        endif                                                           ! of AP specific initialization
      endif                                                             ! of FKMIEX initialization
C
C***********************                                ***********************
C*                          Start of trace migration
C***********************                                ***********************
C
      call inap(buf(numhdr+1),nwrds)                                    ! Assume inap puts it at ap location 1
      vskso4 = vnorm * realk * realk                                    ! ( (v * k)/(2 * dw) ) ** 2
C
C..                                           AP version of migration
      if( iuseap .eq. USEAP ) then
        if ( LocCoord.eq.CDPOLAR ) then
           call vmov(in,1,nextad,2,nw)                                  ! Move the polar coords in pairs
           call vmov(in+nw,1,nextad+1,2,nw)                             ! Move the phase spectrum
           call rect(nextad,2,in,2,nw)                                  ! Convert to rectangular coords
        endif
        call apput(sclrs,iasclr,NSCLRS,2)                               ! Put the scalars in the AP each time
        call apput(vskso4,iavsks,nv,2)
        if( IAND(lprint,2).eq.2 ) then
          print '(/A)', 'Debug Info FKMIGR: AP version'
          print 530, vskso4, nv, nfint, nt, nw
  530     format ( '(v*k)/(2*dw)**2: ',f10.4,'No. velocities (nv)'
     *            ,I3,' nfint ', I4,' Time points (nt)', I6,
     *            ' No. of wavelegths (nw): ',I5)
C           call dumpap(iasclr,7)
        endif
c
        call apwd
c        call stolt1(iavsks,nv,iap,iaq,nt,nw,nfint,iasclr,iaspac)
c        call stolt2(iavsks,nv,iap,iaq,nt,nw,nfint,iasclr,iaspac)
c
        if ( LocCoord.eq.CDRECT) then
          call vmov(iaq,1,in,1,nwrds)                                   ! Move data to where SIOSEIS expects it
        else
          call polar(iaq,2,iasclr,2,nw)
          call vmov(iasclr,2,in,1,nw)
          call vmov(iasclr+1,2,in+nw,1,nw)
        endif
      endif                                                             ! of AP version of migration
C
C..                                  Host memory (Non-AP) version of migration
      if ( iuseap.eq.NOAP ) then
        if ( LocCoord.eq.CDPOLAR ) then
          call rectc( nw, scr, ap(in), ap(in+nw) )                      ! Convert to rect coords
          do 1010 j = 1, nwrds, 2
            ap(in+j-1) = scr(j)
            ap(in+j)   = scr(j+1)
 1010     continue
        endif
c
        if ( IAND(lprint,2).eq.2) then
          print '(/A)','Debug Info FKMIGR: Host memory version '
          print *,'Trace No: ', nxdone,' Wavenumber: ', realk
          print *,'No of w (nw): ', nw,' No of ranges (nx): ', nx,
     *            ' (v*k)/(2*dw)**2 (vkso4): ',vskso4
          print *,'AP address In: ', in, ' Nextad: ', nextad
          print *,'Delay: ', delay,' nwrds=',nwrds
        endif
C
c****   scr(nwrds+nwrds+5) is the start.  nwrds+nwrds+nwrds is needed!
        call HALE(vskso4, ap(in), ap(nextad), nw, nfint, scr, scr,
c     *          scr(nwrds+3), scr(nwrds+nwrds+5), to)
     *          scr2, scr3, to)
C
        if ( LocCoord.eq.CDRECT) then
          do 1040 i  = 1, nwrds                                         ! Shift address of migrated trace
 1040       ap(in+i-1) = ap(nextad+i-1)
        else if ( LocCoord.eq.CDPOLAR ) then
          call polarc(nw,ap(nextad),scr(1),scr(nw+1))
          do 1050 i=1,nw
            ap(in+i-1)    = scr(i)
            ap(in+nw+i-1) = scr(nw+i)
 1050     continue
        endif
      endif                                                             ! of Non-AP version of migration
C
      realk  = realk  + dk                                              ! Increment the wavenumber
      nxdone = nxdone + 1                                               ! & the number of traces completed
      RETURN
      end
