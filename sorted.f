      SUBROUTINE sorted
c
c                       PROCESS SORT
c                       ------- ----
c
c  Document date: 12 June 1995
c
c      Process SORT creates a disk file containing a list of traces
c  ordered differently from the input SEGY disk file.  Process DISKIN
c  can read the traces from the SEGY file in the order contained in
c  the output of this process.  The sort is done so that the values being
c  sorted are increasing.  Process DISKOX can "desort" the sorted
c  SEGY file using this same "sort" file.
c      The following example sorts and desorts disk file data:
c        procs sort diskin diskoa end
c        sort
c          lkey2 3 limit1 2 3 ipath data opath test lkey1 4 end end
c        diskin
c             spath test ipath data end end
c        diskoa
c            opath junk spath test end end
c        end
c
c  LIMITATIONS:
c  1)  Only 1 input diskin file may be used at a time.
c  2)  A maximum of 50,000 traces may be sorted at a time, unless the
c      large version of sort has been installed which permits 350,000
c      traces to be sorted.  (The large version uses subroutine
c      sortex.BIG.f and the small version uses subroutine sortex.small.f
c      The big version requires the computer running sioseis to have
c      more than 8mb of memory).
c
c  PARAMETER DICTIONARY
c  --------- ----------
c  IPATH  - The input SEGY pathname.
c           REQUIRED.         e.g.  ipath /usr/seismic/line1
c  OPATH  - The output pathname that will contain the ordered list of
c           traces.
c           REQUIRED.         e.g.  opath sort1.line1
c  LKEY1  - The index to the SEGY trace header of the primary key to
c           use in sorting.  The index is of the 32 bit integer
c           or long integer variables of the header.
c           REQUIRED         e.g.  lkey1 4
c  IKEY1  - The index to the SEGY trace header of the primary key to
c           use in sorting.  The index is of the 16 bit integer
c           or short integer variables of the header.
c           Preset = none    e.g.  ikey 83
c  LKEY2  - The index to the SEGY trace header of the secondary key to
c           use in sorting.  The index is of the 32 bit integer
c           or long integer variables of the header.
c           Preset = none    e.g.   lkey2 3
c  IKEY2  - The index to the SEGY trace header of the secondary key to
c           use in sorting.  The index is of the 16 bit integer
c           or long integer variables of the header.
c           Preset = none
c  LIMIT1 - The limits of the primary sort.  Traces outside the limits
c           are not read by process diskin.  The limit values MUST be
c           increasing in value.
c           Preset = none    e.g. limit1 2 3
c  LIMIT2 - The limits of the secondary sort.  Traces outside the limits
c           are not read by process diskin.  The limit values MUST be
c           increasing in value.
c           Preset = none   e.g. limit2 -500 -200
c  FLAG51 - The value to place in 32 bit integer header word 51
c           whenever the value of the primary key changes.  Word 51 is
c           the "end-of-sort" flag for many SIOSEIS process (e.g. stack),
c           where a -1 is used to indicate the end of "gather".  Word 51
c           will be set to zero if it is not the "end-of-sort".  SORT
c           will not modify word 51 unless FLAG51 is given.
c           Preset = none           e.g.  flag51 -1
c  REVERSE - When given, the sort is done in reverse.  This is done by
c           negating the header values of both keys.  This parameter does
c           not need a value following it.  i.e. give the name reverse
c           and follow it with the next parameter.  The parameters
c           LIMIT1 and LIMIT2 are negated by the program also when
c           REVERSE is given.
c           Preset = none       e.g.     reverse
c
c
c  Copyright (C) 1991 The Regents of the University of California.
c  ALL RIGHTS RESERVED.
c
c  Written by Paul Henkart, Scripps Institution of Oceanography,
c       La Jolla, CA 92093-0225
c
c  mod July 11, 1997 - Add reverse1 and reverse2 and remove reverse
c        reverse1 is for key1 and reverse2 is for key2.
c  mod June 2000 - g77 had to have opath be a CHARACTER
c  mod June 2006 - IKEY2 was not declared. (Thanks Alistair)
c  mod 24 July 09 - Print warning on files > 2GB
c
      PARAMETER ( npars = 13 )                                          ! the number of user parameters
      CHARACTER*80 token
      CHARACTER*7 names(npars)
      DIMENSION scr(npars), lscr(npars)
      EQUIVALENCE (scr(1),lscr(1))
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /sort1/ lprint, lunin, lunout, lkey1, ikey1, limit1(2),
     &     lkey2, ikey2, limit2(2), iflag51, reverse1, reverse2
      INTEGER reverse1, reverse2, lltemp(2)
      COMMON /sort1a/ ipath, opath
      CHARACTER*80 ipath, opath
      LOGICAL iexist
      REAL limit1, limit2
      DATA names / 'LPRINT', 'IPATH ', 'OPATH ', 'LKEY1 ','IKEY1',
     &             'LIMIT1', 'LKEY2 ', 'IKEY2 ', 'LIMIT2','FLAG51',
     &             'REVERSE','REV1  ', 'REV2  ' /
      DATA lunin/0/, lunout/0/, lkey1/0/, ikey1/0/, limit1/2*-999999./,
     &     lkey2/0/, ikey2/0/, limit2/2*-999999./, iflag51/-99999/,
     &     opath/' '/, reverse1/0/, reverse2/0/
c****
c****    Set the parameter presets and various variable presets
c****
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
             IF( names(nparam) .EQ. 'REVERSE' ) THEN
                 reverse1 = 1
                 reverse2 = 1
                 GOTO 100
             ENDIF
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'IPATH' ) THEN
                 ipath = token
                 CALL getfil(4, lunin, token, istat )
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' ***  ERROR  ***  Could not open file ',
     &                      token(1:nchars)
                     ierror = ierror + 1
                 ENDIF
                 CALL filsiz( token, lltemp )
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
                     itemp = lltemp(1)
                     lltemp(1) = lltemp(2)
                     lltemp(2) = itemp
                 ENDIF
                 IF( lltemp(1) .NE. 0 ) THEN
                     PRINT *,' ***  WARNING  *** ',
     &                              ' SORT may fail on files > 2GB.'
                     iwarn = iwarn + 1
                 ENDIF
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'OPATH' ) THEN
                 token(nchars+1:nchars+1) = ' '
                 CALL getfil( 4, lunout, token, istat )
                 IF( istat .NE. 0 ) THEN
                     CALL getfil( 3, lunout, token, istat )
                 ENDIF
                 IF( istat .NE. 0 ) THEN
                     PRINT *,' Can not open opath ',token
                     ierror = ierror + 1
                 ENDIF
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'REV1' ) THEN
                 IF( token(1:nchars) .EQ. 'YES' ) reverse1 = 1
                 IF( token(1:nchars) .EQ. 'NO' ) reverse1 = 0
                 IF( token(1:nchars) .EQ. 'ON' ) reverse1 = 1
                 IF( token(1:nchars) .EQ. 'OFF' ) reverse1 = 0
                 IF( token(1:nchars) .EQ. '1' ) reverse1 = 1
                 IF( token(1:nchars) .EQ. '0' ) reverse1 = 0
                 GOTO 100
             ENDIF
             IF( names(nparam) .EQ. 'REV2' ) THEN
                 IF( token(1:nchars) .EQ. 'YES' ) reverse2 = 1
                 IF( token(1:nchars) .EQ. 'NO' ) reverse2 = 0
                 IF( token(1:nchars) .EQ. 'ON' ) reverse2 = 1
                 IF( token(1:nchars) .EQ. 'OFF' ) reverse2 = 0
                 IF( token(1:nchars) .EQ. '1' ) reverse2 = 1
                 IF( token(1:nchars) .EQ. '0' ) reverse2 = 0
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! convert the alpha number to an internal machine number
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             IF( names(nparam) .EQ. 'LPRINT' ) lprint = NINT(areal)
             IF( names(nparam) .EQ. 'LKEY1' ) lkey1 = NINT(areal)
             IF( names(nparam) .EQ. 'IKEY1' ) ikey1 = NINT(areal)
             IF( names(nparam) .EQ. 'LKEY2' ) lkey2 = NINT(areal)
             IF( names(nparam) .EQ. 'IKEY2' ) ikey2 = NINT(areal)
             IF( names(nparam) .EQ. 'FLAG51' ) iflag51 = NINT(areal)
             IF( names(nparam) .EQ. 'LIMIT1' ) THEN
                 limit1(1) = areal
  140            CALL getoke( token, nchars )
                 IF( nchars .EQ. 0 ) THEN
                     CALL rdline
                     ntokes = 0
                     GOTO 140
                 ENDIF
                 ntokes = ntokes + 1
                 CALL dcode( token, nchars, limit1(2), istat )
                 IF( istat .NE. 2 ) ierror = ierror + 1
             ENDIF
             IF( names(nparam) .EQ. 'LIMIT2' ) THEN
                 limit2(1) = areal
  150            CALL getoke( token, nchars )
                 IF( nchars .EQ. 0 ) THEN
                     CALL rdline
                     ntokes = 0
                     GOTO 150
                 ENDIF
                 ntokes = ntokes + 1
                 CALL dcode( token, nchars, limit2(2), istat )
                 IF( istat .NE. 2 ) ierror = ierror + 1
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
      IF( lunin .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  IPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( lunout .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  OPATH must be given.'
          ierror = ierror + 1
      ENDIF
      IF( lkey1+ikey1 .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  LKEY1 or IKEY1 must be given.'
          ierror = ierror + 1
      ENDIF
      IF( lkey1 .NE. 0 .AND. ikey1 .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Both LKEY1 and IKEY1 are given.'
          ierror = ierror + 1
      ENDIF
      IF( lkey2 .NE. 0 .AND. ikey2 .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Both LKEY2 and IKEY2 are given.'
          ierror = ierror + 1
      ENDIF
      IF( lkey1 .LT. 0 .OR. lkey1 .GT. 60 ) THEN
          PRINT *,' ***  ERROR  ***  LKEY1 must be between 1 and 60.'
          ierror = ierror + 1
      ENDIF
      IF( lkey2 .LT. 0 .OR. lkey2 .GT. 60 ) THEN
          PRINT *,' ***  ERROR  ***  KEYS must be between 1 and 60.'
          ierror = ierror + 1
      ENDIF
      IF( ikey1 .LT. 0 .OR. ikey1 .GT. 120 ) THEN
          PRINT *,' ***  ERROR  ***  IKEY1 must be between 1 and 120.'
          ierror = ierror + 1
      ENDIF
      IF( ikey2 .LT. 0 .OR. ikey2 .GT. 120 ) THEN
          PRINT *,' ***  ERROR  ***  IKEY2 must be between 1 and 120.'
          ierror = ierror + 1
      ENDIF
      IF( limit1(1) .NE. -999999. .AND. limit1(2) .EQ. -999999. ) THEN
          PRINT *,' ***  ERROR  ***  LIMIT1 must contain two numbers.'
          ierror = ierror + 1
      ENDIF
      IF( limit2(1) .NE. -999999. .AND. limit2(2) .EQ. -999999. ) THEN
          PRINT *,' ***  ERROR  ***  LIMIT1 must contain two numbers.'
          ierror = ierror + 1
      ENDIF
c****
c****    finish up the parameter reading
c****
 2000 CONTINUE
      CALL getoke( token, nchars )                                       ! get the next token
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline                                                          ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) GOTO 100
      IF( IAND(lprint,1) .NE. 0 ) PRINT *,lunin, lunout, lkey1, ikey1,
     &    limit1, lkey2, ikey2, limit2, reverse
      RETURN
      END
