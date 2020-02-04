      SUBROUTINE GETPRO(NAMES,LENGTH,ITYPE)
C     GETPRO READS A USER'S LIST OF PROCESSES AND BUILDS ARRAY IORDER.
C  THE ORDER IN WHICH THE USER LISTS THE PROCESSES IS THE ORDER IN WHICH THE
C  PROCESSES WILL BE APPLIED TO THE DATA.
C     THE LIST OF LEGAL PROCESS NAMES IS COMMUNICATED VIA COMMON /PNAMES/ AND
C  THE ORDER OF PROCESSING IS A LIST OF PROCESS NUMBERS AND IS SET BY GETPRO
C  IN COMMON /PORDER/. THE NAMES EDIT AND EXEC ARE ALLOWED TO BE IN THE USER'S
C  LIST OF PROCESSES.  THE USER MUST SPECIFY EXEC IN ORDER TO GET THE JOB TO
C  EXECUTE AS WELL AS TO EDIT.  IF EDIT IS SPECIFIED OR EXEC IS NOT SPECIFIED,
C  THEN NO TAPES WILL BE HUNG AND THE JOB WILL NOT EXECUTE - THE JOB WILL THEN
C  BE AN EDIT ONLY JOB.
C     GETPRO READS A PROCESS NAME FROM A CARD IMAGE SUPPLIED BY THE USER AND IS
C  IN A PRIMOS BUFFER READ BY CONTRO.  THE PROCESS NAME IS CHECKED FOR VALIDITY
C  BY SCANNING THE ARRAY NAMES WHICH ALSO HAS THE PROCESS NUMBER IMPLIED THROUGH
C  IT'S ORDER IN THE ARRAY.  THIS NUMBER IS THEN PUT INTO THE IORDER ARRAY.  THE
C  USER'S LIST OF PROCESSES MUST BE TERMINATED BY 'END', UPON WHICH GETPRO
C  RETURNS TO CONTRO.
C
C PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, FEBRUARY 1979
c
cmod 13 July 99 - Add exemption for diskoe-diskoj
c mod  May 00 - Also, set all processes to require parameters and only set
c               the ones that do not to 0.
c mod 2 Feb 07 - Check for duplicate names in the PROCS list.
c mod 18 Oct 10 - Check for syn, diskin, segddin being in the same list
C
      PARAMETER (NAMESS=100)
      CHARACTER*7 NAMES(NAMESS)
      DIMENSION LENGTH(1),ITYPE(1)
      CHARACTER*10 CTOKEN
      COMMON /PORDER/ NUM,IORDER(NAMESS)
      COMMON /pnames/ pnames
c****  beware of length of pnames since chkprc uses it!
      CHARACTER*7 pnames(namess)
C    iok array is to signal that a process's parameters are required
      COMMON /REQUIR/ IOK(NAMESS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite, maxsamps,
     & nbperw, ireal, nsevere, override
c
C

      NUM=0                                                             ! /* THE NUMBER OF PROCESS TO PROCESS
C****
C****    GET A PROCESS NAME FROM THE USER
C****
   10 CALL GETOKE(CTOKEN,NCHARS)                                        ! /* GET A TOKEN FROM THE USERS INPUT LINE
      IF(NCHARS.GT.0) GO TO 30
C****
C****   END OF LINE  -  GET ANOTHER COMMAND LINE FROM THE USER
C***
      IF(NOW.EQ.1) PRINT 20
   20 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                       ! /* GET AN INPUT LINE FROM THE USER
      GO TO 10
C****
C****    GOT A TOKEN  -  IS IT A VALID PROCESS NAME?
C****
   30 CONTINUE
      CALL UPCASE(CTOKEN,NCHARS)                                        ! /* CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GE.3.AND.NCHARS.LE.7) GO TO 50                          ! /* IS THE TOKEN THE RIGHT LENGTH?
   35 PRINT 40, CTOKEN(1:NCHARS), nchars
   40 FORMAT(' ***  ERROR  ***  PROCESS NAME ',A,' IS NOT A VALID',
     *    ' PROCESS.',I3)
      IERROR=IERROR+1
      GO TO 10
   50 CONTINUE                                                          ! /* NOW SEE IF IT IS IN THE NAMES LIST
      DO 60 I=1,NAMESS
      N=LENGTH(I)                                                       ! /* GET THE NUMBER OF CHARACTERS IN THE LEGAL PROCESS NAME
      IF( n .EQ. 0 ) GOTO 60
      J=I
      IF(CTOKEN(1:NCHARS).EQ.NAMES(I)(1:N).AND.N.EQ.NCHARS) GO TO 100
   60 CONTINUE                                                          ! /* STILL LOOKING FOR THE NAMES
      IF(CTOKEN(1:NCHARS).NE.'EDIT') GO TO 61
      IRUN=0
      GO TO 10
   61 IF(CTOKEN(1:NCHARS).NE.'EXEC') GO TO 62
      IRUN=1
      GO TO 10
   62 IF(CTOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 70            ! /* THE END?
      GO TO 35
c****
c****    Got the END of the procs list
c****
   70 DO 80 I=1,NUM
      IF(IORDER(I).EQ.2) GO TO 90                                       ! /* WAS INPUT IN THE LIST OF PROCESSES
      IF(IORDER(I).EQ.13) GO TO 90                                      ! /* WAS IT SYNTHETIC INPUT
      IF(IORDER(I).EQ.23) GO TO 90                                      ! /* WAS IT DISK INPUT
      IF( iorder(i) .EQ. 36) GOTO 90                                    ! /* was it real time input?
      IF( iorder(i) .EQ. 37) GOTO 90                                    ! /* was it udecon
      IF( iorder(i) .EQ. 41) GOTO 90                                    ! /* was it SEGDIN
      IF( iorder(i) .EQ. 59 ) GOTO 90                                   ! /* was it MAXIN
      IF( iorder(i) .EQ. 72 ) GOTO 90                                   ! /* was it SEG2IN
      IF( iorder(i) .EQ. 87 ) GOTO 90                                   ! /* was it SEGDDIN
   80 CONTINUE
      PRINT 85
   85 FORMAT(' ***  ERROR  ***  PROCESS INPUT MUST BE IN THE LIST.')
      IERROR=IERROR+1
   90 CONTINUE
      DO 95 I=1,NUM                                                     ! /*  MAKE SURE SOME SORT OF OUTPUT WAS GIVEN
         IF( iorder(i) .EQ. 54 .AND. i .NE. 1 ) THEN
             PRINT *,' ***  WARNING  ***  SORT is normally first.'
             iwarn = iwarn + 1
         ENDIF
      IF(IORDER(I).EQ.3) RETURN                                         ! /* WAS TAPE OUTPUT GIVEN
      IF(IORDER(I).EQ.15) RETURN                                        ! /* WAS IT PRINTER OUTPUT
      IF(IORDER(I).EQ.21) RETURN                                        ! /* WAS IT VELAN
      IF(IORDER(I).EQ.22) RETURN                                        ! /* WAS IT DISKO
      IF(IORDER(I).EQ.27) RETURN                                        ! /* WAS IT SPECTR
      IF(IORDER(I).EQ.30) RETURN                                        ! /* WAS IT PLOT?
      IF(IORDER(I).EQ.37) RETURN                                        ! /* WAS IT UDECON
      IF( iorder(i) .GE. 43 .AND. iorder(i) .LE. 46 ) RETURN            ! /* was it diskoa, diskob, diskoc, or diskod?
      IF( iorder(i) .EQ. 53 ) RETURN                                    ! /* PSEUDO can be to print only!
      IF( iorder(i) .GE. 75 .AND. iorder(i) .LE. 80 ) RETURN            ! /* was it diskoa, diskob, diskoc, or diskod?
      IF( iorder(i) .EQ. 81 ) RETURN                                    ! /* was it GRDOUT
   95 CONTINUE
      PRINT *, ' ***  ERROR  ***  An output process must be in the ',
     *     'PROCS list.'
      IERROR=IERROR+1
      RETURN
C****
C****      GOT A PROCESS NAME
C****
  100 CONTINUE
      NUM=NUM+1
      pnames(num) = names(j)
      IORDER(NUM)=J
      DO i = 1, num-1
         IF( j .EQ. iorder(i) ) THEN
             PRINT *,' ***  WARNING  ***  Process ',names(j),
     &          ' is in the PROCS list multiple times.'
             iwarn = iwarn + 1
         ENDIF
      ENDDO
      IF(J.EQ.3) IOK(3)=1                                               ! /* REQUIRE INPUT PARAMS TO BE GIVEN IF INPUT IS A PROCESS
      IF(J.EQ.4) IOK(4)=1                                               ! /*  IF GEOM IS A PROCESS THEN GEOM PARAMS ARE REQUIRED
      if(j.eq.5) iok(5)=1                                               ! /* gather edit needs to preset common
      IF(J.EQ.7) IOK(7)=1
      IF(J.EQ.8) IOK(8)=1                                               ! /*  NMO PARAMS MUST BE GIVEN
      IF(J.EQ.10) IOK(10)=1                                             ! /* REQUIRE AVENOR EVEN THOUGH NO PARAMS ARE REQUIRED,
C****     BECAUSE THE EDIT SETS THE PRESETS.  ALSO THE EXECUTE REQUIRES THAT
C****     THE PARAMETER FILE EXIST.
      IF(J.EQ.11) IOK(J)=1                                              ! /* REQUIRE FILT PARAMS IF FILT IS A PROCESS
      IF(J.EQ.12) IOK(J)=1                                              ! /* REQUIRE DECON PARAMETERS TO BE GIVEN
      IF(J.EQ.13) THEN
         iok(j) = 1
         DO k = 1, namess
            IF( iorder(k) .EQ. 23 .OR. iorder(k) .EQ. 41 ) THEN
                PRINT *,
     &'*** WARNING ***  SYN and DISKIN or SEGDDIN are in the same list.'
                iwarn = iwarn + 1
            ENDIF
         ENDDO
      ENDIF
      IF(J.EQ.14) IOK(J)=1                                              ! /* REQUIRE AUTOCORRELATION PARAMETERS
      IF(J.EQ.15) IOK(15)=1                                             ! /* PRINTER OUTPUT
      IF(J.EQ.16) IOK(16)=1                                             ! /* REQUIRE WEIGHT PARAMETERS TO BE GIVEN
      IF(J.EQ.17) IOK(17)=1                                             ! /* SHIFT
      IF(J.EQ.19) IOK(19)=1
      IF(J.EQ.20) IOK(20)=1                                             ! /* MIX
      IF(J.EQ.21) IOK(21)=1                                             ! /* VELAN
      IF(J.EQ.22) IOK(22)=1                                             ! /* DISKO
      IF(J.EQ.23) THEN
         iok(j) = 1
         DO k = 1, namess
            IF( iorder(k) .EQ. 13 .OR. iorder(k) .EQ. 41 ) THEN
                PRINT *,
     &'*** WARNING ***  DISKIN and SYN or SEGDDIN are in the same list.'
                iwarn = iwarn + 1
            ENDIF
         ENDDO
      ENDIF
      IF(J.EQ.25) IOK(25)=1                                             ! /* SMUTE
      IF(J.EQ.26) IOK(26)=1                                             ! /* COMPEN
      IF(J.EQ.27) IOK(27)=1                                             ! /* SPECTR
      IF(J.EQ.28) IOK(28)=1                                             ! /* UFILTR
      IF(J.EQ.29) IOK(29)=1
      IF(J.EQ.30) IOK(J)=1                                              ! /* PLOT
      IF(J.EQ.31) IOK(J)=1                                              ! /* TX2FK
      IF( j .EQ. 41 ) THEN
         iok(j) = 1
         DO i = 1, num
            IF( iorder(i) .EQ. 13 .OR. iorder(i) .EQ. 23 ) THEN
                PRINT *,
     &'*** WARNING ***  SEGDDIN and SYN or DISKIN are in the same list.'
                iwarn = iwarn + 1
            ENDIF
         ENDDO
      ENDIF

c     32 is fk2tx - no params needed
c****  Geez, that's stupid.   Set them all to 1 and just set the ones that
c**** don't require parameters to 0.
      DO i = 33, namess
         IF( j .EQ. i ) iok(j) = 1
      ENDDO
      iok(35) = 0                                                         ! F2T
      iok(49) = 0                                                         ! TP2TX
      iok(50) = 0                                                         ! IRIS
      iok(86) = 0                                                         ! XSTAR
      GO TO 10
      END
