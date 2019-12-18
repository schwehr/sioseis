      SUBROUTINE dmoed
c
c                       PROCESS DMO
c                       ------- ---
c
c  Document date:  1 November 1991
c
c  Process DMO, Dip Move Out, applies a phase filter in Omega-K domain
c  to correct for move-out when dip is present.  Typically, NMO is used
c  to produce a zero-offset section which is then migrated to produce a
c  final image.  However, if dips exist, then the mapping of seismic
c  data using the NMO equation is dip dependent, and causes subsurface
c  smear updip away from the midpoint.  To mitigate this problem, dip
c  moveout algorithms have been developed to allow all dips to stacked
c  simultaneously without up-dip smear.  The algorithm used in process
c  DMO is the EXACT LOG DIP MOVEOUT formulation by LINER and assumes
c  constant velocity.
c
c  The traces input to DMO must be sorted by offset distance (range),
c  which may be accomplished with process SORT with the SORT parameter
c  FLAG51 -1, the "end-of-sort" flag.
c
c  This DMO algorithm has "stretch" problems which may be a alleviated
c  in the time domain using process LOGST1 and LOGST2.
c
c  PROCESS DMO requires the data to be transformed into the FK
c  (frequency-wavenumber) domain using process TX2FK.  The data may
c  be converted back to the time domain after DMO using FK2TX.
c
c  A typical DMO processing sequence is:
c      procs sort diskin nmo logst1 tx2fk dmo fk2tx logst2 diskoa end
c
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  DELTAX - The distance between traces, also called the group spacing.
c           REQUIRED
c  WINDOW - The type of window to apply before computing the FFTs.
c         = HANN, Hanning window.
c         = RECT, Rectangular or box car window (no window).
c           Preset = RECT
c  OFFSET - The source-receiver offset (range) which if invoked will
c           override header value.  This is only useful when only one
c           offset is input to DMO and the header value must be
c           overridden.
c           Preset - none
c
c  Copyright (C) 1991 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c  Written by Graham Kent, September 1991
c
c  DTL    - Time increment of log stretched trace (DelTa Log)
c           Preset = 0.004
      PARAMETER ( npars = 5 )                                           ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /dmocom/ idmounit, ndmolists, ndmowrds
      DATA names / 'WINDOW', 'DELTAX', 'LPRINT', 'DTL   ', 'OFFSET'/
c****
c****    Set the parameter presets and various variable presets
c****
      ifiltype = 999
      rdx = 25.0
      ndmolists = 0
      lprint = 0
      rdtl = 0.004
      roffset = 999999.0

      CALL getfil( 1, idmounit, token, istat )                          ! get a file for the DMO parameters
c****
c****     get the user's parameters -  there must be something, at least an "end"
c****
      ntokes = 0                                                        ! count the tokens
  100 CONTINUE
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
             CALL upcase( token, nchars )

             IF( names(nparam) .EQ. 'WINDOW' ) THEN
                 IF( token(1:4) .EQ. 'RECT') ifiltype = 0
                 IF( token(1:7) .EQ. 'HANNING') ifiltype = 1
                 GOTO 100
             ENDIF

             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'DELTAX' ) rdx = areal
             IF( names(nparam) .EQ. 'LPRINT') lprint = NINT(areal)
             IF( names(nparam) .EQ. 'DTL') rdtl = areal
             IF( names(nparam) .EQ. 'OFFSET') roffset = areal
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
      IF( rdx .LT. 0.0 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal DX of ',rdx
          ierror = ierror + 1
      ENDIF
      IF( roffset .LT. 0.0 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal OFFSET of ',roffset
          ierror = ierror + 1
      ENDIF
      IF( rdx .GT. 33.0 ) THEN
          PRINT *,' ***  WARNING ***  DX Value of' , rdx, 'MAY CAUSE
     &ALIASING DURING DMO FOR SEVERE DIPS'
      ENDIF

c****
c****   Write the parameter list to disk
c****
      lscr(1) = ifiltype
      scr(2) = rdx
      lscr(3) = lprint
      scr(4) = rdtl
      scr(5) = roffset
      ndmowrds = npars
      ndmolists = ndmolists + 1

      CALL wrdisc( idmounit, scr, ndmowrds )

      IF( IAND(lprint,1) .NE. 0 ) THEN
        PRINT *,'filttype =',ifiltype,' dx =', rdx,' lprint =',lprint,
     &' sample rate = ', rdtl, 'idmounit:', idmounit,
     &'ndmowrds', ndmowrds,'ndmolists ', ndmolists,
     &'offset: ', roffset
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                    ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
      RETURN
      END
