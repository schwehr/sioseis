      SUBROUTINE NMOED(BUF,LBUF, ibuf, nmonum )
C                             PROCESS NMO, NMO2
C                             ------- ---  ----
C
C  DOCUMENT DATE:
C
c     NMO applies a travel-time correction to each trace.  The following
c  corrections are available:
c  1)  Normal MoveOut or NMO: T(0) = SQRT(T(X)**2 - X**2/V(T)**2)
c  2)  MoveIn or denmo: T(X) = SQRT(T(0)**2 + X**2/V(T)**2)
c  3)  Slant MoveOut or SMO:  T(0) = T(X) + X/V
c  4)  XMO - Move out to range X (AMO? - Abnormal MoveOut)
c            NMO followed by MoveIn
c  5)  NMO with interval velocities
c     Process NMO2 is identical to PROCESS NMO, enabling NMO to be given
c  twice in a job.
C     IN SOLVING NMO, THE RANGE X IS IN TRACE HEADER AND THE VELOCITY MUST BE
C  SPECIFIED BY THE USER.  IT IS USUALLY THOUGHT THAT ALL TRACES COMMON TO THE
C  SAME REFLECTION POINT SHOULD HAVE THE SAME VELOCITY FUNCTION.  THUS, THE
C  VELOCITY FUNCTION GIVEN BY THE USER MAY BE SPECIFIED AT VARIOUS POINTS ALONG
C  THE SEISMIC LINE (CONTROL POINTS).  TRACES LYING BETWEEN THE USER SPECIFIED
C  CONTROL POINTS ARE OBTAINED THROUGH INTERPOLATION OF THE ADJACENT SPECIFIED
C  VELOCITY FUNCTIONS.  THE TIMES ASSOCIATED WITH THE VELOCITY FUNCTION MAY BE
C  RELATIVE TO THE WATER BOTTOM IF SPECIFICIED BY THE USER, BUT ARE RELATIVE
C  TO TIME ZERO OTHERWISE.
C     TIMES NOT SPECIFIED SPECIFICALLY IN A VELOCITY FUNCTION HAVE A VELOCITY
C  OBTAINED THROUGH LINEAR INTERPOLATION.  THE SPATIAL VARIATION OF VELOCITY
C  FUNCTIONS IS PERFORMED THROUGH LINEAR INTERPOLATION ALONG ISO-VELOCITY
C  LINES.  E.G. IF RP 1 HAS VTP 1500 0. 2000 .4 2500 1.0 AND RP 3 HAS
C  VTP 1500 0. 2000 .6 2500 1.0, THEN RP 2 WILL GET VTP 1500 0. 2000 .5 2500 1.
C     VELOCITY CONTROL POINTS ARE DEFINED AS EITHER SHOT POINT NUMBERS OR RP
C  NUMBERS. EACH PARAMETER LIST HAS A START AND END CONTROL POINT WITH THE
C  PARAMETERS BEING CONSTANT FOR ALL POINTS BETWEEN THE FIRST AND LAST CONTROL
C  POINT OF THE LIST.  E.G.  IF THE FIRST CONTROL POINT=100 AND THE LAST CONTROL
C  POINT=110, THE POINTS 101 TO 109 WILL ALSO HAVE THE SAME PARAMETERS APPLIED.
C     SPATIAL VARIATION IS BY SHOT IF THE DATA IS SORTED BY SHOT AND IS
C  VARIED BY RP IF THE DATA IS SORTED BY RP. SEE VARIABLE VTRKWB FOR VARIATION
C  USING WATER DEPTH, A USEFUL OPTION DURING SHIPBOARD PROCESSING.
C  *** NOTE ***  SPATIAL VARIATION OF VELOCITIES WITH VELOCITY INVERSIONS HAS
C                PROBLEMS!!!!
C     EACH PARAMETER LIST MUST BE TERMINATED WITH THE WORD END.  THE ENTIRE SET
C  OF NMO PARAMETERS MUST BE TERMINATED BY THE WORD END.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FNO    - THE FIRST SHOT (OR RP) TO APPLY THE VELOCITIES TO.  SHOT (RP) NUMBERS
C           MUST INCREASE MONOTONICALLY.
C           PRESET=1
C  LNO    - THE LAST SHOT (RP) NUMBER TO APPLY THE VELOCITIES TO.  LNO MUST BE
C           LARGER THAN FNO IN EACH LIST AND MUST INCREASE LIST TO LIST.
C           DEFAULT=FNO
C  VTP    - VELOCITY-TIME-PAIRS.  A LIST OF STACKING VELOCITY AND TWO-WAY
C           TRAVEL TIME (IN SECONDS) PAIRS.  VTP MUST BE GIVEN IN EACH NMO
C           PARAMETER LIST.  A MAXIMUM OF 25 PAIRS MAY BE GIVEN.  DATA TIMES
C           BEFORE THE FIRST GIVEN TIME IN VTP ARE HELD CONSTANT FROM THE FIRST
C           GIVEN TIME.  LIKEWISE, DATA TIMES EXCEEDING THE LAST GIVEN TIME IN
C           VTP ARE HELD CONSTANT FROM THE LAST GIVEN VTP TIME.
C           DEFAULT=NONE. E.G. VTP 1490 1.0 2000 2.0
C  STRETC - THE AMOUNT OF STRETCH (NMO OR DELTA T), IN SECONDS, PERMISSIBLE.
C           DATA EXCEEDING STRETCH WILL BE MUTED BY PROCESS NMO.  Valid
c           with NMO (type 1) only.
C           PRESET =1.
C  ADDWB  - WHEN GIVEN A VALUE OF YES, THE WATER BOTTOM TIME IS ADDED TO THE
C           VELOCITY FUNCTION AFTER VARIATION HAS BEEN DONE.
C           PRESET=NO
c  TYPE   - The type of travel time correction to apply.
c         =1,  Normal MoveOut or NMO: T(0) = SQRT(T(X)**2 - X**2/V(T)**2)
c         =2,  MoveIn or denmo: T(X) = SQRT(T(0)**2 + X**2/V(T)**2)
c         =3,  Slant MoveOut or SMO:  T(0) = T(X) + X/V
c         =4,  XMO
c         =5,  NMO with interval velocities
c           PRESET = 1            e.g. type 2
c  VINTPL - The type of velocity interpolation between successive velocity
c           control points.
c         = 1  Velocity spatial interpolation according to "iso-velocity"
c         = 2  Allows regions of constant velocity to be interpolated
c              correctly, however vtp pairs must be equal across control
c              points.
c           Preset = 1
c  VTRKWB - Velocity Tracking Waterbottom.  When given a positive value,
c           velocity interpolation is cued from the water depth as defined
c           in the SEGY header, word buf(54). This is typically the center-
c           beam depth value which is placed in the header during acquistion
c           for each individual shot.  The value of VTRKWB represents a
c           disallowable jump in centerbeam depth, from one gather, shot or
c           cdp, to the next. If a disallowable jump in centerbeam depth
c           occurs, the centerbeam depth from the previous gather is used.
c           Although each trace in a gather may have a different centerbeam
c           depth values, the depth value from first trace will be applied
c           to the entire gather to minimize static shift problems. This
c           option is useful during realtime shipboard processing and allows
c           a pre-defined 2-D velocity field to be used during processing,
c           with fno and lno values representing water depth, vtp pairs
c           defining the stacking velocity function for a given water depth.
c           This scheme allows more flexibility for varying velocities along
c           a reflection line (as opposed to hanging a single velocity
c           function from seafloor).
c           Preset = -9999.0      e.g. vtrkwb 100.0
c
C  END    - TERMINATES EACH PARAMETER LIST.
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, APRIL 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
c  mod Nov 1991 by gmk to add VINTPL
c  mod May 1995 to add NMO2
c  mod Nov 1995 by gmk to add vtrkwb
c  mod 21 May 1997 - Add parameter VMUL
c  mod 21 Jul 97 - Add parameter VADD
c                - Allow process NMO3
c  mod 11 Nov 97 - ADD OPATH for writing the velocities out
c  mod 2 March 98 - Initialize on the first call to nmoed when more than
c                 one nmo given (nmo2, nmo3)
c  mod 12 Mar 98 - Write the SEGY header to OPATH on first list only.
c  mod 16 Mar 98 - Allow vintpl 3
c  mod 2 Nov 98 - Added XMO (parameter NEWX)
c  mod 18 Jan 00 - Insist velocities within each VTP monotonically increase.
c  mod 17 Jul 02 - Make maxdt equivalent to stretc
c                - Add parameters dstretch and maxdt (old stretc)
c  mod 10 Mar 03 - Add paramter IVTP - Interval VTP
c  mod 11 Apr 03 - Change vintpl preset for IVTP (vintpl=4).
c  mod 30 Sep 08 - Add parameter xfactor
c  mod 27 Jun 11 - Add parameter HIRES
c  mod 5 Jul 12 - Allow non-increasing velocities on VINTPL 2
c  mod 16 Jul 12 - Make HIRES a yes/no switch as well as 1/0.
c  mod 18 Jul 18 - Swap the trace header when writing a segy OPATH
c                - populate the binary header since some other system demand some entries.
c
C  ARGUMENTS:
C  BUF    - A SCRATCH ARRAY AT LEAST 60 32BIT WORDS LONG.
C  LBUF   - THE SAME ARRAY BUT USED FOR LONG INTEGERS.  PRIME DOESN'T ALLOW
C           EQUIVALENCING OF ARGUMENTS.
C
C  DISC PARAMETER LIST ORDER:
C  WORD 1)  FNO      (ALL ENTRIES ARE 32 BITS LONG)
C       2   LNO
C       3)  STRETC
C       4)  ITYPE
C       5)  ADDWB
C       6)  LPRINT
C       7)  VINTPL
C       8)  VTRKWB
c       11) lunvtp
c       12) newx
c       14) dstretch
c       16) xfactor
c       17) hires
C       npars)  NS  - THE NUMBER OF ENTRIES IN THE VTP GIVEN BY THE USER
C       18-68) VTP  - ALWAYS 50 LONG
C
C
C
      PARAMETER (NPARS=18)                                              ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MAXVTP=50)                                             ! THE MAXIMUM NUMBER OF VTPS THAT NMOEX CAN HANDLE
      PARAMETER (NWRDS=MAXVTP+NPARS)
      PARAMETER (MULTIV=NPARS)                                        ! THE INDEX OF THE FIRST MULTIVALUED PARAMETER
c**** (That means vtp must be the last parameter)
      CHARACTER*8 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN, ctemp
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /readt/ itunit, numhdr, numdat, ihunit
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /NMOCOM/ NUNIT(3), NLISTS(3), nsegyfile(3)
      DIMENSION BUF(111),LBUF(111)
      INTEGER*2 ibuf(200)
      INTEGER FNO
      INTEGER VINTPL
      REAL VTRKWB, maxdt
      LOGICAL iexist, first
C
C
      EQUIVALENCE (FNO,LVALS(1)),
     2            (LNO,LVALS(2)),
     3            (ADDWB,LVALS(3)),
     4            (LPRINT,LVALS(4)),
     5            (STRETC,VALS(5)),
     6            (ITYPE,LVALS(6)),
     7            (VINTPL,LVALS(7)),
     8            (VTRKWB,VALS(8)),
     9            (vmul,vals(9)),
     *            (vadd,vals(10)),
     1            (opath,vals(11)),
     2            (newx,lvals(12)),
     3            (maxdt,lvals(13)),
     4            (dstretch,vals(14)),
     6            (xfactor,vals(16)),
     7            (hires,vals(17)),
     *            (VTP,VALS(NPARS))
      DATA NAMES/'FNO   ','LNO   ','ADDWB ','LPRINT','STRETC',
     *           'TYPE  ','VINTPL','VTRKWB','VMUL  ','VADD  ',
     &           'OPATH ','NEWX  ','MAXDT','DSTRETCH','IVTP  ',
     &           'XFACTOR','HIRES ',
     &           'VTP   '/
      DATA LENGTH/3,3,5,6,6,4,6,6,2*4,5,4,5,8,4,7,5,3/
      DATA TYPES/'L','L','A','L','F','L','L',3*'F','A','L',4*'F',
     &           'A','F'/
      DATA first/.TRUE./
      SAVE first
C****
C****      SET THE PRESETS
C****
      FNO=1
      LNO=32768
      VTP=-1.
      STRETC=1.
      ITYPE=0
      IADDWB=0
      LPRINT=0
      VINTPL = 99999
      VTRKWB = -9999.0
      IF( first ) THEN
          first = .FALSE.
          DO i = 1, 3
             nunit(i) = 0
             nlists(i) = 0
             nsegyfile(i) = 0
          ENDDO
      ENDIF
      LLNO=0
      NS=0
      nvtps = 0
      vmul = 1.
      vadd = 0.
      lunvtp = 0
      newx = -123456
      maxdt = 0.
      dstretch = 0.
      xfactor = 1.
      hires = 0.
C****
C0****    GET A PARAMETER FILE
C****
      CALL GETFIL(1,NUNIT(nmonum),token,ISTAT)
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
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
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** NMO DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         ! END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF( TYPES(NPARAM) .EQ. 'A') THEN
          ctemp = token
          CALL upcase( token, nchars )
          IF( NAMES(NPARAM) .EQ. 'HIRES' .AND.
     &        (TOKEN(1:NCHARS).EQ.'YES' .OR. TOKEN(1:NCHARS).EQ.'1'))
     &        hires = 1
          IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *        IADDWB = 1
          IF( names(nparam) .EQ. 'OPATH' ) THEN
              IF( icompt .EQ. 4 .OR. icompt .EQ. 7 )
     *            ctemp(nchars+1:nchars+1) = CHAR(0)                    ! Unix wants a null terminator
              IF( token(nchars-3:nchars) .EQ. 'SEGY' .OR.
     *            token(nchars-2:nchars) .EQ. 'MAT') THEN
                  nsegyfile(nmonum) = 1
                  IF ( token(nchars-2:nchars) .EQ. 'MAT' )
     &                 nsegyfile(nmonum) = 2
                  CALL getfil( 3, lunvtp, ctemp, istat )
              ELSE
                  nsegyfile(nmonum) = 0
                  CALL getfil( 2, lunvtp, ctemp, istat )
                  INQUIRE( FILE = ctemp, EXIST = iexist )
                  IF( iexist ) THEN
                      OPEN( UNIT=lunvtp, FILE=ctemp, STATUS='OLD')
                      CLOSE( UNIT = lunvtp, STATUS = 'DELETE' )
                  ENDIF
                  OPEN( UNIT=lunvtp, FILE=ctemp, ACCESS='SEQUENTIAL',
     &                  FORM = 'FORMATTED', STATUS = 'NEW' )
              ENDIF
          ENDIF
          GOTO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          !=2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      IF(NPARAM.LT.MULTIV) GO TO 490                                    !  IS IT A MULTIVALUED PARAMETER
      NS=NS+1                                                           ! THE TOKEN WAS A MULTI-VALUED PARAMETER
      nvtps = ns
      IF( NAMES(NPARAM) .EQ. 'VTP' .OR. names(nparam) .EQ. 'IVTP' )
     &    BUF(NS+NPARS)=AREAL
      IF( names(nparam) .EQ. 'IVTP' ) itype = 5
      VTP=0.
      GO TO 100
  490 VALS(NPARAM)=AREAL                                                !  FLOATING POINT VALUES
      GO TO 100
  500 CONTINUE                                                          ! 32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE                                                          ! MAKE SURE ALL SHOT & RP NUMBERS INCREASE
      IF(LNO.EQ.32768) LNO=FNO                                          ! DEFAULT LNO TO FNO
      IF(FNO.GT.LLNO) GO TO 1020                                        ! IS FNO LARGER THAN THE LAST LNO
      PRINT 1010
 1010 FORMAT(' ***  ERROR  ***  SHOT AND RP NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1020 IF(LNO.GE.FNO) GO TO 1030                                         ! DO THEY INCREASE IN THIS LIST
      PRINT 1010
      IERROR=IERROR+1
 1030 LLNO=LNO
      IF(VTP.GE.0.) GO TO 1070
      PRINT 1060
 1060 FORMAT(' ***  ERROR  ***  VTP NOT GIVEN.')
      IERROR=IERROR+1
 1070 CONTINUE
      IF(STRETC.GT..001) GO TO 1090                                     !  STRETCH 0F 0 KILLS MOST DATA!!
      PRINT 1080
 1080 FORMAT(' ***  ERROR  ***  STRETC MUST BE .GT. .001')
      IERROR=IERROR+1
 1090 CONTINUE
      IF(VTRKWB.GT.0.0 .OR. VTRKWB.EQ.-9999.0) GOTO 1120
      PRINT 1100
 1100 FORMAT(' ***  ERROR  ***  VTRKWB MUST BE A POSITIVE NUMBER')
      IERROR=IERROR+1
 1120 CONTINUE                                                          ! CHECK ON ITYPE
      IF( newx .NE. -123456 ) THEN
          IF( itype .EQ. 0 ) itype = 4
          IF( itype .NE. 4 ) THEN
              PRINT *,' ***  ERROR  ***  ITYPE must be 4 with NEWX.'
              ierror = ierror + 1
          ENDIF
      ENDIF
      IF( itype .EQ. 0 ) itype = 1
      IF( ITYPE .LE. 0 .OR. ITYPE .GT. 5 ) THEN
          PRINT *,'  ***  ERROR  *** TYPE must be between 1 and 5.'
          ierror = ierror + 1
      ENDIF
      IF( itype .EQ. 4 .AND. newx .EQ. -123456 ) THEN
          PRINT *,' ***  ERROR  ***  NEWX must be given with TYPE 4 NMO'
          ierror = ierror + 1
      ENDIF
      DO II=1,nvtps,2                                                   !  CHECK THE VTP FOR ERRORS
         I = II+NPARS
         IF( BUF(I) .LE. 0 ) THEN
             PRINT 1150,BUF(I)
 1150        FORMAT(' ***  ERROR  ***  ILLEGAL VTP VELOCITY OF ',F10.4)
             IERROR=IERROR+1
         ENDIF
         IF( vmul .NE. 1. .OR. vadd .NE. 0. )
     &           buf(i) = (buf(i) - vadd) * vmul + vadd
         IF( II .NE. 1 ) THEN
             IF( buf(i) .LE. buf(i-2) .AND. vintpl .NE. 2 ) THEN
                 PRINT 1160
 1160            FORMAT(' ***  WARNING  *** Spatial variation with ',
     &              'non-increasing velocities will be wrong.')
                 iwarn = iwarn+1
             ENDIF
         ENDIF
         J=I+1                                                      ! now do the time checks
         IF(BUF(J).GE.0.AND.BUF(J).LT.20.) GO TO 1180
            PRINT 1170,BUF(J)
 1170       FORMAT(' ***  ERROR  ***  ILLEGAL VTP TIME OF ',F10.4)
            IERROR=IERROR+1
 1180    CONTINUE
         IF( II .NE. 1 ) THEN
             IF( BUF(J) .LE. BUF(J-2) ) THEN
                 PRINT 1191,BUF(J)
 1191            FORMAT(' ***  ERROR  ***  VTP TIME OF ',F10.4,
     &                 ' DECREASED.')
                 IERROR=IERROR+1
             ENDIF
         ENDIF
      ENDDO
      IF( nlists(nmonum) .GT. 0 .AND. itype .EQ. 5 .AND.
     &    nvtps .NE. lastnvtps ) THEN
          PRINT *,' ***  ERROR  ***  Different number of IVTP pairs.'
          ierror = ierror + 1
      ENDIF
      lastnvtps = nvtps
      IF( stretc .EQ. 1. .AND. itype .NE. 1 ) stretc = 100.
      IF( vintpl .EQ. 99999 ) THEN
          IF( itype .NE. 5 ) THEN
              vintpl = 1
          ELSE
              vintpl = 4
          ENDIF
      ENDIF
      IF( vintpl .LT. 1 .OR. vintpl .GT. 4 ) THEN
          PRINT *,' ***  ERROR  ***  VINTPL must be between 1 and 4.'
          ierror = ierror + 1
      ENDIF
C****
C****      WRITE THE PARAMETER LIST TO DISC
C****
      IF(nvtps.LE.MAXVTP) GO TO 1360
      ITEMP=MAXVTP
      PRINT 1350,ITEMP
 1350 FORMAT(' ***  ERROR  ***  NMO CAN HANDLE ONLY ',I3,' VTPS.')
      IERROR=IERROR+1
 1360 CONTINUE
      LBUF(1)=FNO
      LBUF(2)=LNO
      BUF(3)=STRETC
      LBUF(4)=ITYPE
      LBUF(5)=IADDWB
      LBUF(6)=LPRINT
      LBUF(7)=VINTPL
      BUF(8)=VTRKWB
      lbuf(11) = lunvtp
      lbuf(12) = newx
      IF( maxdt .NE. 0 ) buf(3) = maxdt
      buf(14) = dstretch
      buf(16) = xfactor
      buf(17) = hires
      LBUF(npars)=nvtps
      ITEMP=NPARS+1
      ITEMP1=NPARS+nvtps
      IF(IAND(LPRINT,1).EQ.1)  PRINT 2010,(LBUF(I),I=1,2),
     * BUF(3),(LBUF(J),J=4,7),BUF(8), LBUF(11),lbuf(12),
     & (BUF(J),J=ITEMP,ITEMP1)
 2010 FORMAT(' NMO PARAMS:',/,2(1X,I10),1X,F10.4,1X,I10,1X,I4,
     *    2(1X,I10),1x,F10.4,2(1X,I10),/,2(1X,F10.3))
      CALL WRDISC(NUNIT(nmonum),BUF,NWRDS)
      nlists(nmonum) = nlists(nmonum)+1
c****
c**** If we're to write a SEGY file for the velocities, do the SEGY headers
c****
      IF( lunvtp .NE. 0 .AND. nsegyfile(nmonum) .EQ. 1 .AND.
     &    nlists(nmonum) .EQ. 1 ) THEN
          CALL podisc( ihunit, 0, 0 )
          CALL rddiscb( ihunit, buf, 3200, istat )
          CALL wrdiscb( lunvtp, buf, 3200 )
          CALL rddiscb( ihunit, buf, 400, istat )
c****   syn doesn't set the sample interval
          ibuf(11) = numdat  !  number of samples
          ibuf(13) = 5   ! IEEE FP
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     *        CALL SWAP16( ibuf, 200 )
          CALL wrdiscb( lunvtp, buf, 400 )
      ENDIF
c**** finish up.  buf has been clobbered!
      NS=0
      nvtps = 0
      LLNO=LNO
      LNO=32768                                                         ! DEFAULT THE DEFAULTS
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        !WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
