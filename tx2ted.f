      SUBROUTINE tx2ted
c                                                                      
c                       PROCESS TX2TP
c                       ------- -----
c
c  Document date: 25 August 1989
c
c          Process TX2TP transform data in the time-space domain to
c  the tau-p space using the "HOP" method for slant stacking refraction
c  data" by Henry,Orcutt, and Parker (GRL, Dec 1980).  The problem of
c  slant-stacking seismic records at a number of ranges to synthesize a
c  tau-p curve is posed as a linear inverse problem for fixed frequency.
c  Using an inner product weighted by (k^2 + b^2)*k (where k is
c  wavenumber and b some real positive number), then the representers
c  are bessel functions of k*range, scaled by 1/(k^2 + b^2), and the 
c  model is U(k,w) (vert comp only).  The inverse of the Gram matrix 
c  can be found analytically as sums and differences of products of 
c  modified bessel functions of b*range.
c         Since the tx to tau-p transformation changes the meaning of 
c  shots and rps, TX2TP will output the data using the same SEGY
c  headers as input to it so that shots remain shots and rps remain
c  rps, with the same numbers.  If there are more tau-p traces as
c  input t-x traces, the last t-x trace header is used on the extra
c  tau-p traces.
c  The SEGY trace headers will contain the following information:
c  record ("shot") number   (SEGY word 3)
c  trace number with the record  (SEGY word 4)
c  trace id flag (live trace flag)  (SEGY word 15)
c  p value*1000. (in the tx domain position for range)  (SEGY word 10)
c  tau*1000 of the first output sample (delay in ms) (SEGY word 55)
c  tau of the first output sample (delay in seconds) (SEGY word 46)
c  number of data samples (SEGY word 58)
c  sample interval in microseconds (normal SEGY) (SEGY word 59)
c  
c      The input data must be order so that the ranges are monotonically
c  increasing.
c
c  Limitations :  300 input time traces
c                1024 samples within a time trace
c                 300 output p traces
c                 400 taus within a p trace
c                                               
c                                               
c  PARAMETER DICTIONARY
c  --------- ----------
c  SET    - The start and end times of the data to transformation. 
c           Data prior to the start time or after the end time will be 
c           omitted.
c           Preset = the times of the first input trace.  e.g.  set 3 7
c  SEP    - Start and end p values to calculate.  The sep range should
c           only span the dip components of interest (Yilmaz pg 437)
c           Preset = .2 .7  (1/5 1/1.4)   e.g.  sep .05 .55
c  SETAU  - The start and end taus to calculate.  The first output
c           sample will be at the start tau.
c
c  NP     - The number of p values to calculate.  NP is the increment
c           between the start and end p values.
c           Preset = the number of input time traces
c  OHPATH - Output header pathname.  TX2TP saves all of the SEGY trace
c           headers in a disk file so that the output tau-p traces have
c           the save headers as the input.  The disk file is temporary
c           unless OHPATH is given.  This is particularly important if
c           the data is converted back to tx in a separate SIOSEIS run.
c           Preset = temporary file.    e.g.   ohpath txheaders.line1-2
c  FC     - Cutoff frequency for calculating models.
c           Preset = Nyquist frequency      e.g. fc 125
c  PCNTI  - Percent taper applied to the input data before the forward
c           FFT.
c           Preset = 0.
c  PCNTO  - Percent Taper applied to the models before the inverse FFT.
c           Preset = 0.
c  SHIFT  - The time shift to apply to successive traces.  The shift
c           is accumulative, thus each trace is shifted by SHIFT 
c           relative to the previous trace.  The shift represents a
c           constant reduction velocity, i.e. start time for each trace
c           must be delayed by same amount relative to previous trace
c           Required.              e.g.  shift .05
c  PRESTK - When set nonzero, the transformation is done whenever the
c           PRESTK number of "gathers" have been collected.  A "gather" is
c           whenever the "end-of-sort" flag (SEGY header word 51) is -1.
c           Processes SORT and GATHER set the "end-of-sort" flag.
c           Other schemes using DISKIN and HEADER can be used also.
c 
c  END    - Terminates each parameter list.
c  
c
c
c  Copyright (C) by The Regents of The University of California, 1988
c  ALL RIGHTS RESERVED.
c
c  10/6/94 gmk added Warning message regarding spatial aliasing
c  7 Oct 94 - increase number of input and output traces
c  9 Sep 96 - Add prestk
c
      PARAMETER ( npars = 15 )                                          ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      CHARACTER*1 types(npars)                                          ! the type of parameter
      REAL vals(npars)                                                  ! holds the REAL parameter values
      DIMENSION lvals(npars)                                            ! holds the INTEGER parameter values
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /tx2tp/ sshift, sep(2), nnp, setau(2), beta, ffc, ppcnti, 
     *               ppcnto, iirev, ffon, dummy, iimft, set(2), lprt, 
     *               lunhdr, txprestk, ntx2tp
      INTEGER fon, ffon, txprestk, prestk
      EQUIVALENCE ( shift, vals(1) ),
     3            ( np, lvals(3) ),
     5            ( b, vals(5) ),
     6            ( fc, vals(6) ),
     7            ( pcnti, vals(7) ),
     8            ( pcnto, vals(8) ),
     9            ( irev, lvals(9) ),
     *            ( fon, lvals(10) ),
     1            ( prestk, lvals(11) ),
     2            ( imft, lvals(12) ),
     4            ( lprint, lvals(14) )
      DATA names /'SHIFT ','SEP   ','NP    ','SETAU ','B     ',
     *            'FC    ','PCNTI ','PCNTO ','IREV  ','FON   ',
     *            'PRESTK','IMFT  ','SET   ','LPRINT','OHPATH' /
      DATA types /2*'F','L',5*'F',4*'L','F','L','A' /
c****  there's some weird shit with the segy header (ohpath).  The problem
c****  occurs when tp2tx is in the same procs list and tp2tx wants the
c****  headers.  So, my logic(?) is:  contro sets lunhdr = 0. tx2ted
c****  opens the file and sets lunhdr.  tp2ted only opens a header file
c****  if lunhdr = 0 or the user gave ihpath.
c**** 
c****    Set the parameter presets and various variable presets
c****
      ntx2tp = 0
      shift = 0.
      b = 2.                                                            ! hidden parameter - scaling factor for representers
      sep(1) = .2
      sep(2) = .7
      np = 0
      fc = 0
      pcnti = 0.
      pcnto = 0.
      irev = 0
      fon = 1
      set(1) = -1.
      set(2) = -1.
      lprint = 0
      setau(1) = 99999.
      prestk = 0
c****
c****     get the user's parameters -  there must be something, at least an "end"
c****
      ntokes = 0                                                        ! count the tokens
  100 CONTINUE
      ns = 0
      CALL getoke( token, nchars )                                      ! get a token and it's length
      CALL upcase( token, nchars )                                      ! convert parameter names to upper case
      IF( nchars .EQ. 0 ) THEN                                          ! anything there?
          CALL rdline                                                   ! nope, get another line
          ntokes = 0
          GOTO 100
      ENDIF
  110 ntokes = ntokes + 1
      DO 200 nparam = 1, npars
         IF( token(1:nchars) .EQ. names(nparam) ) THEN                  ! find the parameter name in our list
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'OHPATH' ) THEN
                 CALL getfil( 3, lunhdr, token, istat )
                 GOTO 100
             ENDIF
             ns = ns + 1
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'SET' ) THEN                        ! set is an array
                 set(ns) = areal
                 IF( ns .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'SEP' ) THEN                        ! sep is an array
                 sep(ns) = areal
                 IF( ns .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'SETAU' ) THEN                        ! setau is an array
                 setau(ns) = areal
                 IF( ns .EQ. 1 ) GOTO 120
                 GOTO 100
             ENDIF
             IF( types(nparam) .EQ. 'L' ) THEN
                 lvals(nparam) = areal                                  ! convert the real to INTEGER*4
             ELSE
                 vals(nparam) = areal                                   ! move the real to the parameter
             ENDIF
             GOTO 100
         ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END') THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     *      token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
c****
c****    Do some ERROR checking
c****
      IF( sep(1)+sep(2) .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  SEP must be given.'
          ierror = ierror + 1
      ENDIF
      IF( np .LT. 0 .OR. (np .GT. 0 .AND. np .LT. 2 ) )THEN
          PRINT *,' ***  ERROR  ***  NP must be greater than 2.'
          ierror = ierror + 1
      ENDIF
      IF (fc .eq. 0 .OR. fc .GT. 30) THEN
          PRINT *,' ***  WARNING  ***  Spatial Aliasing May Occur'
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                          ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) THEN
          PRINT *,' ***  ERROR  ***  TX2TP can have only one list .'
          ierror = ierror + 1
      ENDIF
      sshift = shift
      nnp = np
      beta = b
      ffc = fc
      ppcnti = pcnti
      ppcnto = pcnto
      iirev = irev
      iimft = imft
      lprt = lprint
      ffon = fon
      txprestk = prestk
      IF( lunhdr .EQ. 0 ) THEN                                            ! if the user didn't give ohpath, get a temporary file
          CALL getfil( 1, lunhdr, token, istat )
      ENDIF
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' sshifts=',sshifts,' sep=',sep,' nnp=',nnp,
     *      ' setau=',setau,' beta=',beta,' ffc=',ffc
          PRINT *,' ppcnti=',ppcnti,' ppcnto=',ppcnto,' iirev=',
     *      iirev,' fon=',ffon,' set=',set(1),set(2),' lunhdr=',lunhdr
          PRINT *,' prestk=',txprestk
      ENDIF
      RETURN
      END
