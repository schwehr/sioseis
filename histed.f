      SUBROUTINE histed
c                                PROCESS HISTORY
c                                ------- -----
c  
c  Document Date: 10 February 1997
c  
c  Process HISTORY keeps a running history or log of the SIOSEIS
c  processing steps.  The log is an ASCII file that is appended
c  on each SIOSEIS "job".  The file may be edited with your
c  favorite text editor too.
c
c  Two lines are always appended to the file stating the start
c  start time of the job and the list of processes.  A terminating
c  line is also written when the job completes normally.  e.g.
c  Job      1 started on Thu Feb 13 11:41:57 1997, SIOSEIS ver 97.2 (13 Feb. 1997)
c  PROCS SYN     HISTORY PROUT
c  Job      1 finished on Thu Feb 13 11:41:57 1997
c  
c  HISTORY may be placed anywhere in the PROCS list, but will
c  operate only on the first and last trace of the job.
c
c  THE PARAMETER DICTIONARY
c  --- --------- ----------
c  
c  HPATH  - The filename of the HISTORY file.  
c           REQUIRED.      e.g.  hpath ew9607.line1.hist
c
c  ALL    - When set to YES, all user given parameters are
c           logged.  The entire SIOSEIS parameter script is
c           copied to the history file.
c           Preset = NO       e.g.  all yes
c  A      - Append mode similar to the append mode of many text
c           editors.  This mode is terminated by a placing
c           a period or dot in the first character of a line. As
c           in the "ed" editor,e.g.
c           A
c           This line is inserted into the log file.
c           Remember to terminate with a period in column 1.
c           .
c
c  END    - Terminates each parameter list.
c  
c  
c  Written and copyrighted (c) by:
c  Paul Henkart, Scripps Institution of Oceanography, February 1997
c  ALL RIGHTS RESERVED.
c  
c
      PARAMETER (NPARS=3)                                               ! THE NUMBER OF USER PARAMETERS
      CHARACTER*8 NAMES(NPARS)
      CHARACTER*80 TOKEN, ctemp
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /sioln1/ cbuf
      COMMON /sioln2/ ischar, ncinline, iprint, lunpo
      CHARACTER*200 cbuf
      COMMON /history/ lunhist, all, lunappend
C
C
      DATA NAMES/'HPATH ','ALL','A'/
C****
C****      SET THE PRESETS
C****
      lunhist = 0
      all = 0.
      lunappend = 0
****
c****     get the user's parameters
c****
      ntokes = 0                                                        ! count the tokens
  100 CONTINUE
      CALL getoke( token, nchars )                                      ! get a token and it's length
      IF( token(1:1) .EQ. 'A' .AND. nchars .EQ. 1 ) THEN
          CALL getfil( 2, lunappend, ctemp, istat )                     ! reserve a unit number
          OPEN( UNIT=lunappend, STATUS='SCRATCH' )
          IF( ischar .LE. ncinline ) THEN
              ctemp = cbuf(ischar+1:ncinline)
              cbuf = ctemp
              ncinline = ncinline - ischar + 1
              CALL wline( lunappend )
          ENDIF
  110     CALL rdline
          IF( cbuf(1:1) .NE. '.' ) THEN
              CALL wline( lunappend )
              GOTO 110
          ENDIF
          CALL rdline
          GOTO 100
      ENDIF
      CALL upcase( token, nchars )                                      ! convert parameter names to upper case
      IF( nchars .EQ. 0 ) THEN                                          ! anything there?
          CALL rdline                                                   ! nope, get another line
          ntokes = 0
          GOTO 100
      ENDIF
      ntokes = ntokes + 1
      DO 200 nparam = 1, npars
         IF( token(1:nchars) .EQ. names(nparam) ) THEN                  ! find the parameter name in our list
  120        CALL getoke( token, nchars )                               ! get the value
             IF( nchars .EQ. 0 ) THEN
                 CALL rdline
                 ntokes = 0
                 GOTO 120
             ENDIF
             ntokes = ntokes + 1
             IF( names(nparam) .EQ. 'HPATH' ) THEN
                 CALL getfil( 2, lunhist, token, istat )                ! reserve a unit number
                 OPEN( UNIT=lunhist, FILE=token, STATUS='UNKNOWN' )
                 GOTO 100
             ENDIF
             CALL upcase( token, nchars )
             IF( names(nparam) .EQ. 'ALL' ) THEN
                 IF( token(1:3) .EQ. 'YES' ) all = 1.
                 GOTO 100
             ENDIF
             CALL dcode( token, nchars, areal, istat )                  ! conver
             IF( istat .NE. 2 ) ierror = ierror + 1                     ! was the an error decoding it?
             GOTO 100
         ENDIF
  200 CONTINUE
      IF( token(1:nchars) .NE. 'END') THEN
          PRINT *,' ***  ERROR  ***  No such parameter as ',
     *      token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
C****
C****   FINISHED A LIST, NOW DO THE ERROR CHECKING
C****
 1000 CONTINUE
      IF( lunhist .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  HPATH must be given.',
     &      token(1:nchars)
          ierror = ierror + 1
      ENDIF
      CALL getoke( token, nchars )
      IF( nchars .LE. 0 ) THEN
          CALL rdline
          ntokes = 0
          GOTO 1000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) THEN
          PRINT *,' ***  ERROR  ***  Unknown HISTORY parameter.'
          ierror = ierror + 1
      ENDIF
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
