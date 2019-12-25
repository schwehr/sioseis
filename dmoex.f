      SUBROUTINE dmoex(buf,lbuf,ibuf,scr,lscr,iscr)
c-------------------------------------------------------------------------------
c       DMOEX is the execution phase of the sioseis process DMO. DMOEX is a single
c  trace process that applies the phase shift to produce the DMO ellipse. Input data
c  must be in the FK domain (i.e. must have been through process TX2FK).
c
c       DMOEX  is called once for each input k domain complex frequency trace.
c  The wavenumbers of the input traces are from 0 to +k Nyquist (The negative
c  wavenumbers are not needed since they can be inferred from the positive
c  ones). The frequencies (OMEGA) of the input trace are from -Nyq -> 0 -> +Nyq.
C
C      DMOEX calculates the frequency (OMEGA) and wavenumber phase shift for each trace
C  by applying the phase factor developed by Liner (SEE GEOPHYSICS, 1990). PROCESS uses
c  only rect coordinates for f-k.

c
c Inputs:
c    BUF - The complex frequency trace to be DMOed
c   IBUF - The integer*2 equivalent of buf. Used to pass trace header info
c   LBUF - The integer*4 equivalent of buf. Used to pass trace header info
c   SCR/ISCR/LSCR/ - Scratch array and its integer equivalents.
c
c Outputs:
c    BUF - contains the migrated trace.
c
c Called from : CONTRO
c
c Externals :
c  PODISC, RDDISC    : Disk i/o routines
c                               INCLUDE FILES

c-------------------------------------------------------------------------------

c*GMK THESE ARE THE INCLUDE FILE WHICH HAVE BEEN PASTED INTO THE EXECUTABLE CODE
c*    MANY OF THESE VARIABLES ARE NOT USED FOR DMO BUT I WON'T GO CRAZY AND CHOP
c     OUT THE UNNECESSARY ONES - JUST IN CASE I NEED THEM LATER!
c INCLUDE FILES
c.. Include file defining constants of use globally throughout SIOSEIS
c
c Modified 11/11/88     added DATINAP
c 8 Apr 09 - Use common numdat rather than segy header word ISAMPPTR
c Define the sizes of some Large common blocks

      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 65537)                                       ! Size of /APMEM/ : AP memory
      parameter (SZTRANP = 262144)                                      ! Size of /TRANP/: Transpose array at 512x512
c parameter definitions for the stop signal istop
      integer STOPNOTR, STOPTR, NOSTOP
      parameter (STOPNOTR  = -1)                                        ! last call but no trace passed
      parameter (STOPTR    =  1)                                        ! last call and a trace passed
      parameter (NOSTOP    =  0)                                        ! normal call to the routine
c
c parameter for AP
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
c
c.. Define Constants for Trace Header
c
c.. Constants for Trace ID (ITRIDPTR)
      integer    LIVETR, DEADTR                                         ! Parameters for trace Type
      parameter (LIVETR  = 1)
      parameter (DEADTR  = 2)
c
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
c Include file TXFK.INC
c include file for the sioseis 2D FFT and fk migration routines
c
c
c Parameters for coord type
      integer CDRECT, CDPOLAR, CDPOLARU
      parameter ( CDRECT   = 1)
      parameter ( CDPOLAR  = 2)
      parameter ( CDPOLARU = 3)
c
c parameters for Windows
      integer BoxCar
      parameter (BoxCar = 5)
c PROGRAM
      Parameter (NSCLRS = 7)
      Parameter (PI = 3.141592654)

C
      common /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap,
     *               idecim
      common /READT/ilun,numhdr,numdat,iunhdr,ireelm,intrcs,ifmt,nskip,
     *               secs,lrenum,isrcf,idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc
      COMMON /dmocom/ idmounit, ndmolists, ndmowrds
      common /TXFKE/ txinit, fkinit, txed, fked, txunit,
     $               ltxunt1, ltxunt2, lfkunt1, lfkunt2,
     $               ohdrtxfk, tmptxhdr, txprestk, ntx2fk, nfk2tx, range
      common /APMEM/ a(SZAPMEM)
C
      dimension lbuf(1),buf(1), scr(1),lscr(1)
      integer*2 ibuf(1),iscr(1)
      integer   txsius
      integer   LocCoord
      logical   first
      save
C
      dimension params1(5), lparams1(5)
      equivalence (params1(1),lparams1(1))

      data first /.true./, hold /-1.0/, zerotol /0.00001/

c**************************************************************************************
c                                  PROCESS DMOEX
c**************************************************************************************
C
c*GMK Hopefully resets wavenumk when new offset panel is processed

      if (first) then                                                   ! DMO Initialization
        first = .false.

c*GMK Checks logstretch and reads appropriate disk file
        CALL podisc( idmounit, 1, 0 )
        CALL rddisc( idmounit, params1, ndmowrds, istat )
          ifiltype  = lparams1(1)
          dx        = params1(2)
          lprint    = lparams1(3)
          rdtl      = params1(4)
          roffset   = params1(5)
c       write(*,*) 'ifiltype;', ifiltype,'dx:', dx,'lprint:', lprint,
c     &'rtdl:', rdtl, 'ndmowrds:', ndmowrds, 'idmounit:', idmounit,
c     &'offset: ', roffset
c
c*GMK Checks to see if coordinates are rect. polar or non-FK
        if (idtype.eq.IDFKRCT) then
           LocCoord = CDRECT
        else if ((idtype.eq.IDFKPLR).OR.(idtype.eq.IDFKPLRU)) then
           LocCoord = CDPOLAR
               print *, ' *** DMO Error *** ',
     *           ' PROCESS DMO dose not accept data
     *in  POLAR COORDINATES.'
               STOP
             else
               print *, ' *** DMO Error *** ',
     *           ' The input data is not in the FK domain.'
               STOP
        endif

c..                       Read the time delay/sample interval from READT common
c*GMK   Imtialize various f-k parametrs
        if (roffset .ne. 999999.0) then
           hosr = roffset/2.0
        else
           hosr = range / 2.0
        endif
        txsius = itxsi                                                  ! t-x sample interval in us
        delay  = itxdel/1000.                                           ! & time delay in secs.
        nx     = 2* (nfktrc-1)                                          ! Calcalate no. of output traces
        dt     = FLOAT(txsius)/1.0E6                                    ! Delta t
        dk     = 2.*pi/(float(nx)*dx)                                   ! Delta k (distance between wavenumbers)
c        nw     = ibuf(ISAMPPTR)/2                                       ! The number of frequencies
        nw = numdat / 2
        dw     = 2.*pi/(float(nw)*dt)                                   ! Delta w (distance between frequencies)
c        nwrds  = ibuf(ISAMPPTR)                                         ! No. of 32 bit words in the complex trace
        nwrds = numdat
        nt     = nwrds / 2                                              ! No. of time samples
        realk  = 0.                                                     ! Wavenumber of the initial input trace

        wnyqneg = (-2.0*pi) * ( 1.0/(2.0*dt) )
        wavenumk = realk
        numleft = nfktrc

        if ( IAND(lprint,1).ne.0) then
          print *,'EXECUTE DMO'
          print *,'Number of fk traces:', nfktrc, 'Output traces:', nx
          print *,'Delta T:', dt,' delta k:', dk,'Delta w:', dw
          print *,'No of w (nw):', nw,' No of time samps (nt):', nt
          print *,'Number wrds:', nwrds, 'tx samp int:', txsius
          print *,'half-offset:', hosr, 'Delay: ', delay
          print *,'in ap', in
        endif

      endif


c*GMK What type of filter do you want ?
       if (ifiltype .eq. 0 .or. ifiltype .eq. 999) then
          goto 30
       else
           print*, 'ifiltype: ', ifiltype, ' not supported yet'
          goto 40
       endif


c***********************                                ***********************
*                     Loop over Wavenumer for each FK Trace
c***********************                                ***********************
c*GMK Start at k=0, and w = -nyquist to +nyquist-dw (padded to power of 2**n (even) whereas -nqy to nyq (odd)
c*    thus +nyquist sample is omitted
c
30     continue

C*GMK If half-offset equal to zero, skip phase shift...already at zero-offset
       if (hosr .eq. 0.0) then
          if (first) print*, 'Zero-offset data, no phase shift applied'
          goto 40
       endif

c*GMK  loop over complex trace samples
          Do 20 j = 1, nwrds,  2
             wfreq = wnyqneg + float( (j+1)/2 - 1)*dw
c*GMK  Break phase shift into real and imaginary parts (preal pimag)

c*     Stationary point ys --> 0 as ky --> 0 expand (1+e)**.5 for small e
             if (wavenumk .eq. 0.) then
               ys = 0.
             else
               if (abs(wfreq).lt.zerotol) then
                  ys = -1.0*hosr
               else
                 subys = wfreq/(2.0*wavenumk)
                 ys =  subys * (1.0  - (1.0 + (hosr**2/subys**2) )**0.5)
               endif
             endif
             betas = ys/hosr

c*GMK  If wfreq is equal to zero, don't take log (0), expand later
             if ( abs(wfreq).gt.zerotol) then
                 deltas = 0.5*log(1.0-betas**2)
             endif

             denom = (1.0+betas**2)**0.5

c*GMK  If wfreq is equal to zero, the phase term goes to zero, although it asymptotes
c      to +/- wavenumk*ys in the positive or negative limit (THANKS AJH)

             if (abs(wfreq).gt.zerotol) then
                aphase = wfreq*deltas-wavenumk*ys
             else
                aphase = 0.0
             endif

c*GMK Take the real and imaginary parts of the phase term (complex conjugate
c* due to fk sign convention e(-iwt) --> forward e(+iwt) --> inverse in sioseis
             preal = cos(aphase)/denom
             pimag = -1.0*sin(aphase)/denom

c*GMK     Is data in AP
c*        Either way, multiply the complex phase term * complex trace and not screw
c*        up like I did (GMK)
          if (in.eq.0) then
            tempbj = buf(j+numhdr)*preal -  pimag*buf(j+numhdr+1)
            tempbj1 = buf(j+1+numhdr)*preal + buf(j+numhdr)*pimag
            buf(j+numhdr) = tempbj
            buf(j+numhdr+1) = tempbj1
          else
            tempaj  = a(j)*preal-pimag*a(j+1)
            tempaj1 = a(j+1)*preal+a(j)*pimag
            a(j) = tempaj
            a(j+1) = tempaj1
          endif
20        continue

c*GMK     SAVE saves wavenumk and it is advanced to next wavenumber trace if it exists
          wavenumk  = wavenumk + dk
40    continue

c*GMK Count number of traces for a given offset panel until done
c*    Reset first to .true. which allows new offset to be read correctly
      numleft = numleft - 1
      if (numleft.eq.0) then
         first = .true.
      endif

      return
      end
