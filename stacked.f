      SUBROUTINE stacked
c                         PROCESS STACK
c                         ------- -----
c
c
c    DOCUMENT DATE: 22 December 1992
c
c         Process STACK adds consecutive traces, completing the sum when
c   the special SIOSEIS end-of-gather flag (header word 51 set to -1) is
c   detected.  The end-of-gather flag may be set by process GATHER,
c   process INPUT or DISKIN parameter NTRGAT, or process HEADER.
c         The summed trace is scaled or averaged by the number of
c   live trace samples contributing to each stacked trace.  I.E. mute
c   times are accounted for, as well as the number of live traces
c   in the summation.
c         STACK honors trace length changes within the gather as well as
c   changes of the deep water delay with the gather.
c         Some of the SEGY trace headers are modified by process STACK.
c   The rp number is always set to the rp number of the first trace of
c   the gather.
c   The rp trace number is always 1.
c   The X and Y shot coordinates are set to zero.
c   The number of stacked traces (cdp or fold) is set.
c
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  header - The type of header replacement.  The SEGY trace header words
c           for the shot number, shot trace number, range and GMT are
c           the only header words affected.
c         = FIRST, The above header values are taken from the first
c                  trace of each gather.
c         = NORMAL, The shot number, the shot trace number, and the
c                  trace range are set to 0.  The GMT is of the first
c                  trace of each gather.
c         = LAST, The above header values are taken from the last
c                  trace of each gather.
c           Preset = NORMAL
c
c  END    - Terminates the parameter list.
c
c  Copyright (C) 1992 The Regents of the University of California
c  ALL RIGHTS RESERVED.
c
c  mod 28 aug 95 - allow parameter lprint
c  mod 25 Sep 05 - Add parameter NEW
c  mod 25 May 06 - Change NEW preset from 0 to 1
c
      PARAMETER ( npars = 3 )                                           ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*6 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /stack/ lheader, lprint, new
      DATA names / 'HEADER', 'LPRINT', 'NEW' /
c****
c****    Set the parameter presets and various variable presets
c****
      lheader = 2
      new = 1
c****
c****     get the user's parameters
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
             IF( names(nparam) .EQ. 'HEADER' ) THEN
                 IF( token(1:nchars) .EQ. 'FIRST' ) THEN
                     lheader = 1
                 ELSEIF( token(1:nchars) .EQ. 'NORMAL' ) THEN
                     lheader = 2
                 ELSEIF( token(1:nchars) .EQ. 'LAST' ) THEN
                     lheader = 3
                 ELSE
                     PRINT *,' ***  ERROR  ***  Illegal HEADER type.'
                     ierror = ierror
                 ENDIF
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'NEW' ) THEN
                 IF( token(1:2) .EQ. 'NO' ) new = 0
                 IF( token(1:3) .EQ. 'OFF' ) new = 0
                 IF( token(1:1) .EQ. '0' ) new = 0
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
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
