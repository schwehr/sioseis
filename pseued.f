      SUBROUTINE pseued
c                                                                      
c                       PROCESS PSEUDO
c                       ------- ------
c
c  Document date: 21 January 1991
c
c      Process PSEUDO Transforms a time domain trace into a "pseudo
c  reflection coefficient" trace.  PSEUDO requires the user to take
c  two passes at the data; the first one to calculate and print the
c  multiplier used for the conversion and the second pass to apply
c  the multiplier.  The shot/rps used to calculate the multiplier
c  are controled by process DISKIN - PSEUDO uses all the traces given
c  to it by process DISKIN.  All seismic lines within a project 
c  should have the same SCALE applied.
c      PSEUDO determines a multiplier (SCALE) from a window across a
c  selected portion of the seismic section in order to determine the
c  "median reflectance".  The multiplier is then applied to all 
c  ampliudes of all traces.
c      The window used in determining the multiplier must be a
c  constant time from the water bottom.  The window start time is always
c  the water bottom time contained in the SEGY trace header, which in
c  turn may be set using process WBT.  The window is NWIND long.  The
c  window is subdivided into NGROP equal subwindows.  The subwindow 
c  used to determine the multiplier is given with the ISEC parameter.
C       FOR EXAMPLE, IF PWIND = 500, NGROP = 2, ISEC = 2, THEN
C                    MEDIAN REFLECTANCE IS COMPUTED WITHIN LOWER 250 MS OF
C                    SUBBOTTOM REFLECTIONS, THAT IS
C                    BETWEEN WATER BOTTOM  TIME (WBT) + 250 MS AND WBT + 500MS.
C                    IF PWIND = 600, NFROP = 4, ISEC = 3, THEN
C                    MEDIAN REFLECTANCE IS COMPUTED WITHIN WBT + 300MS
C                    AND WBT + 450 MSEC.
c                                               
c      Only one parameter list may be given.
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  SCALE  - The scale factor to convert the trace.
c         = 0, The factor will be calculated using all the traces in 
c              between FNO and LNO.
c         > 0, The trace will be converted using SCALE
c           SCALE corresponds to the third field of the REFC card in Myung
c           Lee's version of PSEUDO.
c           PRESET = 0.      e.g.  SCALE .00015
c  FNO    - The first shot/rp number to use for calculating the scale
c           factor needed to convert amplitude to pseudo reflection
c           coefficients.
c           REQUIRED
c  LNO    - The last shot/rp number to use for calculating the scale
c           factor needed to convert amplitude to pseudo reflection
c           coefficients.
c           REQUIRED
c  PWIND  - The length of the window used to calculate the "median
c           reflectance".  The window starts at the water bottom.
c           PWIND corresponds to the second field of the REFC card in Myung
c           Lee's version of PSEUDO.
c           REQUIRED
c  NGROP  - The number of subwindows to use.
c           NGROP corresponds to the third field of the REFC card in Myung
c           Lee's version of PSEUDO.
c           REQUIRED
c  ISEC   - The subwindow number.
c           ISEC corresponds to the fourth field of the REFC card in Myung
c           Lee's version of PSEUDO.
c           REQUIRED
c  WINLEN - The length, in seconds, of the search window for the water 
c           bottom detection.  
c           REQUIRED
c  ITYPE  - The type of amplitude to use in detecting the water bottom
c           reflection.
c         = 1, Use trace amplitude.
c         = 2, Use peak amplitude.
c         = 3, Use trough amplitude.
c           ITYPE corresponds to the second field of the TEST card in Myung
c           Lee's version of PSEUDO.
c           PRESET = 1
c  SLOPE  - A geologic dependent parameter.  This is also known as AA.
c           PRESET = -5.36 / 100.
c  BB     - A geology dependent parameter.
c           PRESET = 0.411
c  WBCREF - A geology dependent parameter.  The water bottom correction
c           reference time.
c           PRESET = 3.
c  PATH   - The pathname of an output file containing a list of 
c           1) the corrected water bottom reflection coefficient.
c           2) The median reflectance.
c           REQUIRED when scale <> 0
c           Preset to a scratch file when scale = 0.
c  LPRINT - A "debug" switch.
c         = 2, The data written to PATH is also written to "screen".
c           Preset = 0
c           
c
c
      PARAMETER ( npars = 14 )                                          ! the number of user parameters
      CHARACTER*80 token, pathname
      CHARACTER*6 names(npars)
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /HYDBK1/ ITYPE, JTYPE, BULK, SCALE, DTT, NGROP, ISEC, 
     &     pwind, nwind, slope, bb, wbcref, lprint, fno, lno, winlen,
     &     iunit
      INTEGER fno, lno, ngrop, isec, nwind, itype
      REAL pwind, slope, bb, wbcref, bulk
      DATA names / 'SCALE ','FNO   ','LNO   ','PWIND ','NGROP ',
     &             'ISEC  ','WINLEN','ITYPE ','SLOPE ','BB    ',
     &             'WBCREF','LPRINT','BULK ','PATH  ' /
c****   this is a hidden parameter.  I'm assuming it is really
c****   the deep water delay of the data trace.
c  BULK   - A time delay in seconds.
c           BULK corresponds to the sixth field of the TEST card in Myung
c           Lee's version of PSEUDO.
c           PRESET = 3.
c****
c****   The user's parameters are given in seconds, but the program
c****  requires time to be in milliseconds, except for WBCREF and BULK,
c****  which are in seconds.
c***
c**** 
c****    Set the parameter presets and various variable presets
c****
      scale = 0.
      fno = -1234567
      lno = -1234567
      pwind = 0.
      ngrop = 0
      isec = 0
      winlen = 0.
      itype = 1
      slope = -5.36 / 100.
      bb = .411
      wbcref = 3.
      bulk = 3.
      pathname = ' '
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
             ns = ns + 1
             IF( names(nparam) .EQ. 'PATH' ) THEN
                 pathname = token(1:nchars)
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'SCALE' ) scale = areal
             IF( names(nparam) .EQ. 'FNO' ) fno = areal
             IF( names(nparam) .EQ. 'LNO' ) lno = areal
             IF( names(nparam) .EQ. 'PWIND' )  pwind = areal
             IF( names(nparam) .EQ. 'NGROP' ) ngrop = areal
             IF( names(nparam) .EQ. 'ISEC' ) isec = areal
             IF( names(nparam) .EQ. 'WINLEN' ) winlen = areal
             IF( names(nparam) .EQ. 'NWIND' ) nwind = areal
             IF( names(nparam) .EQ. 'ITYPE' ) itype = areal
             IF( names(nparam) .EQ. 'SLOPE' ) slope = areal
             IF( names(nparam) .EQ. 'BB' ) bb = areal
             IF( names(nparam) .EQ. 'WBCREF' ) wbcref = areal
             IF( names(nparam) .EQ. 'BULK' ) bulk = areal
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
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
c****   Take care of the file 
c****
      CALL getfil( 2, iunit, token, istat )                             ! get a unit number
      IF( pathname .EQ. ' ' ) THEN
          OPEN( UNIT = iunit,
     &          STATUS = 'SCRATCH',
     &          ACCESS = 'SEQUENTIAL',
     &          FORM = 'FORMATTED')
      ELSE
          OPEN( UNIT = iunit,
     $          FILE = pathname,
     &          STATUS = 'UNKNOWN',
     &          ACCESS = 'SEQUENTIAL',
     &          FORM = 'FORMATTED')
      ENDIF
c****
c****    Do some ERROR checking
c****
      IF( itype .LT. 1 .OR. itype .GT. 3 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal ITYPE value of ',itype
          ierror = ierror + 1
      ENDIF
      IF( scale .EQ. 0 .AND. fno .EQ. -1234567 ) THEN
          PRINT *,' ***  ERROR  ***  FNO and LNO are required ',
     &            'when SCALE = 0.'
          ierror = ierror + 1
      ENDIF
      IF( scale .NE. 0 .AND. ngrop .LE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  NGROP must be positive.'
          IERROR = IERROR + 1
      ENDIF
      IF( scale .NE. 0 .AND. isec .LE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  ISEC must be positive.'
          IERROR = IERROR + 1
      ENDIF
      IF( scale .NE. 0 .AND. pwind .LE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  ISEC must be positive.'
          IERROR = IERROR + 1
      ENDIF
      IF( scale .NE. 0 .AND.  pathname .EQ. ' ' ) THEN
          PRINT *,' ***  ERROR  ***  PATH must be given.'
          IERROR = IERROR + 1
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
          PRINT *,
     &        ' ***  ERROR  ***  process PSEUDO can have only one list.'
          ierror = ierror + 1
      ENDIF
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' scale=',scale,' fno=',fno,' lno=',lno
          PRINT *,' pwind=',pwind,' ngrop=',ngrop,' isec=',isec
          PRINT *,' winlen=',winlen,' itype=',itype,' slope=',slope
          PRINT *,' bb=',bb,' wbcref=',wbcref,' bulk=',bulk
      ENDIF
      RETURN
      END
