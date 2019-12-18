      SUBROUTINE resaed
c
c                       PROCESS RESAMP
c                       ------- ------
c
c  Document date: 16 November 1993
c
c      Process RESAMP resamples the seismic trace from one sample
c  interval to another by: 1) transforming the trace into the frequency
c  domain then back to the time domain; or 2) doing a polynomial
c  interpolation in the time domain.
c      This process is useful for converting data that is recorded at 
c  "not nice" sample rates.  E.g. OBS data is recorded with 128 samples
c  per second which is .0078125 seconds per sample.  The SIOSEIS process
c  PLOT has problems with that, but not with .008 or 125 samples per 
c  second.
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  NEWSI  - The output sample interval, in seconds.
c           REQUIRED.            e.g.   newsi  .004
c  TYPE   - The method of resampling.
c         = 1, Performed in the frequency domain by using the IMSL 
c              FFT routines FFTRC and FFTCC.
c             *****    Available ONLY if IMSL is available.   ******
c         = 2, Performed in the time domain using polynomial 
c              interpolation of order n, as discussed in "Numerical
c              Recipes", section 3.1
c         Preset = 1    limits   0 < type < 2         e.g.   type 2
c  ORDER  - The order of the interpolation when using type 2 
c           interpolation.  According to "Numerical Recipes", "We
c           enthusiastically endorse interpolations with 3 or 4 points,
c           we are perhaps tolerant of 5 or 6; but we rarely go higher".
c         Preset = 4   limits   1 < order < 7        e.g.   order 3
c
c
c  Copyright (C) 1991 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 17 July 2008 - Change type preset to time domain (type 2)
c
      PARAMETER ( npars = 4 )                                           ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /resamp/ si, lprint, type, order
      INTEGER type, order
      DATA names / 'NEWSI ', 'LPRINT', 'TYPE  ', 'ORDER ' /
c**** 
c****    Set the parameter presets and various variable presets
c****
      si = -99999.
      lprint = 0
      type = 2
      order = 4
c****
c****     get the user's parameters
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
             CALL upcase( token, nchars )
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'NEWSI' ) si = areal
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'TYPE' ) type = NINT(areal)
             IF( names(nparam) .EQ. 'ORDER' ) order = NINT(areal)
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
      IF( si .EQ. -99999. ) THEN
          PRINT *,' ***  ERROR  ***  NEWSI must be given.'
          ierror = ierror + 1
      ENDIF
      IF( si .LE. 0. .AND. si .NE. -99999. ) THEN
          PRINT *,' ***  ERROR  ***  NEWSI must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( type .LT. 1 .OR. type .GT. 2 ) THEN
          PRINT *,' ***  ERROR  ***  type must be 1 or 2.'
          ierror = ierror + 1
      ENDIF
      IF( order .LT. 2 .OR. order .GT. 6 ) THEN
          PRINT *,' ***  ERROR  ***  ORDER must be between 2 and 6.'
          ierror = ierror + 1
      ENDIF
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' newsi=', si,' type=',type,' order=',order
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                      ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                   ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
      RETURN
      END
