      SUBROUTINE fkshed
C                                 PROCESS FKSHIFT
C                                 ------- -------
C
C  DOCUMENT DATE: 10 November 1992
c
c       Process FKSHIFT performs an extrapolation via a phase shift in 
c  the FK domain.  FKSHIFT is depth migration of a horizontally layered
c  media whose velocity is always increasing.
c       The data must have been transformed into the FK domain prior to c  process FKSHIFT and it is left in the FK domain.  Use process TX2FK 
c  prior to FKSHIFT and FK2TX after FKSHIFT.
c       FKSHIFT can be used to forward extrapolate shot gathers by
c  specifying the true velocity and extrapolation height (thickness).
c       FKSHIFT can be used to backwards extrapolate stacked data to thec  seafloor by halving the specified velocity and specifying the output
c  time delay.
c
c       Only one parameter may be given.
c
c  PARAMETERS DICTIONARY
c  ---------- ----------
C  VEL     - The constant velocity.
C            Preset = 0    e.g. 1500
C  DELTAX  - The distance between traces.
C            Preset 1.   e.g. deltax 100.
c  ZEXTRAP - The extrapolation height.
c            Preset = 0.
c  ODELAY  - The time of the first output sample, in seconds.
c            Preset = 0.     e.g.  odelay 1.
c  DELTAT  - The sample interval of the data in the time domain, in
c            seconds.
c            Preset = SEGY header.    e.g.  deltat .004
C  END     - TERMINATES EACH PARAMETER LIST.
C
c  Copyright (C) by The Regents of The University of California, 1992
c  ALL RIGHTS RESERVED.
C
      PARAMETER (NPARS=7)                                               ! THE NUMBER OF USER PARAMETERS
      CHARACTER*7 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /FKSHIFT/V,NINT,LPR,DX,DT, realk, zextra, odelay1
C
C
      EQUIVALENCE (VEL,VALS(1)),
     2            (LPRINT,LVALS(2)),
     3            (NFINT,LVALS(3)),
     4            (DELTAX,VALS(4)),
     5            (DELTAT,VALS(5)),
     6            (zextrap,vals(6)),
     7            (odelay,vals(7))
      DATA NAMES/'VEL   ',
     2           'LPRINT',
     3           'NFINT ',
     4           'DELTAX',
     5           'DELTAT',
     6           'ZEXTRAP',
     7           'ODELAY'/
      DATA LENGTH/3,6,5,6,6,7,6/
      DATA TYPES /'F','L','L',4*'F'/
C****
C****      SET THE PRESETS
C****
      VEL=1.
      LPRINT=0
      NFINT=10
      DELTAX=1.
      DELTAT=0.
      IADDWB=0
      LLNO = 0
      zextrap = 0.
      odelay = 0.
      NLISTS=0
      NS=0
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                       ! GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                     ! GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF(NS.NE.0) GO TO 230
      PRINT *, ' ***  ERROR  *** FKSHIFT DOES NOT HAVE A PARAMETER ',
     &    TOKEN(1:NCHARS)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NS=0
      NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF(TYPES(NPARAM).NE.'A') GO TO 240
      IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *    IADDWB=1
      GO TO 100
  240 CONTINUE
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          ! =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      VALS(NPARAM)=AREAL                                                !  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          !  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
      V=VEL
      LPR=LPRINT
      NINT=NFINT
      DX=DELTAX
      DT=DELTAT
      zextra = zextrap
      odelay1 = odelay
      IF( IAND(LPRINT,1) .EQ. 1 ) PRINT *,V,NINT,DX,DT, zextra, odelay
      NLISTS=NLISTS+1
      IF( nlists .GT. 1 ) THEN
          PRINT *,' ***  ERROR  ***  Only one parameter list is used.'
          ierror = ierror + 1
      ENDIF
      LLNO=LNO
      LNO=32768                                                         ! DEFAULT THE DEFAULTS
      NS=0                                                              ! SET THE NUMBER OF MULTI-VALUED PARAMETER ENTRIES BACK TO ZER0
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
