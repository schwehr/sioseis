      SUBROUTINE TOUTED
C                          PROCESS OUTPUT  (WRITES TO MAGNETIC TAPE)
C                          ------- ------
C
C  DOCUMENT DATE:
C
C     PROCESS OUTPUT WRITES SEISMIC DATA TO MAGNETIC TAPE.  THE OUTPUT DATA
C  WILL BE WRITTEN IN SEGY OR MODIFIED SEGY FORMAT.  THE SEGY FORMAT HAS AN
C  EBCDIC TAPE HEADER AND A BINARY TAPE HEADER AT THE BEGINNING OF EVERY TAPE.
C  EVERY TRACE IS THE CONCATENATION OF A TRACE HEADER AND THE DATA TRACE.  SEE
C  ONE OF THE APPENDICES FOR MORE INFORMATION ABOUT THE SEGY TRACE HEADER.
C       OUTPUT RECORD (SHOT OR RP) NUMBERS SHOULD NORMALLY BE MONOTONICALLY
C   INCREASING BY 1 SINCE PROCESS INPUT EXPECTS THEM THIS WAY.
C      OUTPUT TAPE CHANGES ARE HANDLED BY PROCESS OUTPUT THROUGH OPERATOR
C  INTERVENTION WHEN THE PHYSICAL END OF TAPE IS DETECTED.  THE OPERATOR MUST
C  SPECIFY THE NEW TAPE UNIT NUMBER TO THE PROGRAM VIA FILE OUT.  THE FILE
C  'OUT' MUST BE IN THE UFD THAT THE JOB IS RUNNING IN.  EXAMPLE:
C        ED
C        1
C
C        FILE OUT
C
C      ONLY ONE PROCESS OUTPUT MAY BE PERFORMED IN THE JOB, EVEN THOUGH A
C  SECOND OUTPUT WOULD BE A GOOD QUALITY CONTROL STEP.
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  OUNIT  - OUTPUT TAPE DRIVE UNIT NUMBER.  OUTPUT REEL CHANGES ARE PERFORMED
C           BY THE PROGRAM WHEN A TAPE IS FULL.  OUTPUT REEL UNIT NUMBERS ARE
C           CHANGED BY THE COMPUTER OPERATOR ON REEL CHANGES.
C           Preset = 0.  E.G. OUNIT 2
c  DEVICE - The tape device name for writing.  When given, the Fortran
c           magnetic tape device independent driver is used to write on 
c           the specified device.
c           See the document input.forum for addition information.
c           Valid on UNIX machines only.  The use of this parameter causes
c           the tape to be closed with a single file mark, rather than the
c           normal two.  Use the input parameter nfiles 1 when reading tapes
c           created with the output parameter DEVICE.
c           Preset = none       e.g.  device  /dev/nrst8
C  FON    - FIRST OUTPUT NUMBER.
C           PRESET = 0.  E.G. FON 41
C         <0,  WHEN USED IN CONJUNCTION WITH POSAFT<0, FON IS SET TO 1 GREATER
C              THAN THE LAST RECORD ON TAPE.
C         =0,  THE OUTPUT RECORD NUMBERS ARE THE SAME AS THE INPUT (OR
C              CALCULATED IN PROCESS GEOM).
C         >0,  THE OUTPUT RECORD NUMBERS WILL START WITH FON AND WILL INCREASE
C              BY 1 ON SUBSEQUENT OUTPUT RECORDS.
C  POSAFT - Output tape position indicator.  Positioning is done prior to
c           writing the first output trace.  Not valid when DEVICE is used since
c           POSAFT requires the use of a backspace, which device can not do.
C           PRESET = 0.
C         <0, THE TAPE WILL BE POSITIONED AFTER THE LAST TRACE ON THE TAPE.
C         =0, THE TAPE WILL NOT BE POSITIONED.
C         >0, THE TAPE WILL BE POSITIONED AFTER THE SHOT (RP) WITH NUMBER POSAFT
C
C  INFREQUENTLY USED PARAMETERS
C  ------------ ---- ----------
C  OFMT   - THE OUTPUT FORMAT.  SPECIFIES THE TYPE OF SEGY FORMAT TO WRITE.
C           NOTE: PRIME FLOATING POINT IS NOT A STANDARD GEOPHYSICAL FORMAT.
C           PRESET = 5  E.G. OFMT 2
C         =0, SAME FORMAT AS THE INPUT TAPE.
C         =1, IBM 360 FLOATING POINT (available on machines with APs only).
C         =2, INTEGER*4.  (32 BIT INTEGER)
C         =3, INTEGER*2 (NOT RECOMMENDED).
C         =4, NOT AVAILABLE.
C         =5, HOST 32 BIT FLOATING POINT **** NOT A STANDARD SEGY FORMAT ****.
C  ONTRCS - THE NUMBER OF TRACES PER OUTPUT RECORD.  OUTPUT SHOT NUMBERS ARE
C           NORMALLY INCREMENTED AFTER ONTRCS HAVE BEEN WRITTEN.  RP NUMBERS ARE
C           INCREMENTED AFTER AN END OF GATHER FLAG IS DETECTED.  POST STACK
C           RP NUMBERS ARE INCREMENTED ON EACH RP.  WORKS ONLY WHEN FON <>0.
C           PRESET=SAME AS INPUT (SHOTS ONLY)
c  NFSKIP - The number of files to skip before writing the first output.  Both
c           SEGY "tape" headers are written.  While this is a violation of the
c           SEGY standard, it is extremely useful when writing DATs or Exabyte
c           tapes.  The number is from the beginning of tape (a rewind is done).
C           Preset = 0
c  REWIND - Rewinds the output tape PRIOR to writing the first shot (or
c           positioning with POSAFT or doing NFSKIP).
c         = 1, (TRUE), rewind.
c         = 0, (FALSE), NO rewind.
c           Preset = 1,      e.g.  rewind 0    # don't rewind
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, LA JOLLA, CA, JAN. 1980
C
C
C     ONLY 1 OUTPUT MAY BE DONE IN A JOB.  THIS RESTRICTION MAY BE REMOVED BY
C  CREATING AN IDENTICAL SET OF SUBROUTINES WITH SLIGHTLY DIFFERENT SUBROUTINE
C  AND COMMON NAMES.  SUBROUTINES TOUTED, TOUT, WRTTRC AS WELL AS COMMON TAPOUT,
C  WRITET MUST CHANGE.
C
c  mod 29 aug 91 -  add device
c  mod 22 Jun 94 - set iounit = 2 when DEVICE is given.
c  mod 13 Nov 95 - add REWIND
c  mod 7 Sept 96 - Add newfile to common
c  mod 30 June 97 - Make REWIND a YES/NO switch in addition to 1/0.
c  mod 4 Sept 00 - Add parameter trace0
c  mod 7 Sept 00 - Add parameter OUNIT2
c  mod 1 July 02 - Allow OUNIT2 to be given without OUNIT being given
c                  so that OUNIT2 can be tested on a 1 drive system.
c  mod 31 Jul 02 - Add warning if DEVICE is not 'bn'
c  mod 8 May 03 - The above isn't true on SGI
c
      PARAMETER ( NPARS = 12 )                                           ! THE NUMBER OF USER PARAMETERS
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /WRITET/IOUNIT,NSAMPS,OREEL,POSAF,FMT,NTRCS,LFOR,ONUMTR,
     &       nskip, rewindo, newfile, itrace0, iounit2
      COMMON /outdev/outdevice
      CHARACTER*80 outdevice
      INTEGER fon,OFMT,FMT,OUNIT,POSAFT,POSAF,ONTRCS,ONUMTR,oreel,
     &        rewind, rewindo, oreeln, trace0, ounit2
      EQUIVALENCE (OREELN,LVALS(1)),
     2            (OUNIT,LVALS(2)),
     3            (OFMT,LVALS(3)),
     4            (FON,LVALS(4)),
     5            (POSAFT,LVALS(5)),
     6            (LPRINT,LVALS(6)),
     7            (ONTRCS,LVALS(7)),
     9            (nfskip,lvals(9)),
     *            (rewind,lvals(10)),
     1            (trace0,lvals(11)),
     2            (ounit2,lvals(12))
      DATA NAMES/'OREELN',
     2           'OUNIT ',
     3           'OFMT  ',
     4           'FON   ',
     5           'POSAFT',
     6           'LPRINT',
     7           'ONTRCS',
     8           'DEVICE',
     9           'NFSKIP',
     *           'REWIND',
     1           'TRACE0',
     2           'OUNIT2'/
      DATA TYPE /'L',6*'L','A','L',2*'A','L'/
      DATA LENGTH /6,5,4,3,8*6/
C****
C****    SET THE PRESETS
C****
      OREEL=-1
      OUNIT = -1
      OFMT=5
      FON=0
      POSAFT=0
      ONTRCS=0
      LPRINT=0
      nfskip = 0
      outdevice = ' '
      rewind = 1
      newfile = 0
      trace0 = 0
      ounit2 = -1
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
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** OUTPUT DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      IF( type(nparam) .EQ. 'A') THEN
          IF( names(nparam) .EQ. 'DEVICE' ) THEN
              outdevice = token(1:nchars)
              iounit = 2
c****   sgi needs    nrns  (no-rewind, no-swap)
              IF( token(nchars-3:nchars) .EQ. 'nrns' ) GOTO 100
c****  sun needs   bn
              DO i = 1, nchars-1
                 IF( token(i:i+1) .EQ. 'bn' ) GOTO 100
              ENDDO
              PRINT *,
     &    ' ***  WARNING  ***  DEVICE should be BSD and No Rewind (bn).'
              iwarn = iwarn + 1
              GOTO 100
          ENDIF
          CALL upcase( token, nchars )
          IF( names(nparam) .EQ. 'REWIND' ) THEN
              IF( token(1:nchars) .EQ. 'YES' ) rewind = 1
              IF( token(1:nchars) .EQ. 'NO' ) rewind = 0
              IF( token(1:nchars) .EQ. 'ON' ) rewind = 1
              IF( token(1:nchars) .EQ. 'OFF' ) rewind = 0
              IF( token(1:nchars) .EQ. '1' ) rewind = 1
              IF( token(1:nchars) .EQ. '0' ) rewind = 0
          ENDIF
          IF( names(nparam) .EQ. 'TRACE0' ) THEN
              IF( token(1:nchars) .EQ. 'YES' ) trace0 = 1
              IF( token(1:nchars) .EQ. 'NO' ) trace0 = 0
          ENDIF
          GOTO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN                                          ! END OF LINE?
          IF(NOW.EQ.1) PRINT 140                                        ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
          CALL RDLINE                                                   ! GET ANOTHER LINE
          NTOKES=0
          GOTO 210
      ENDIF
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF( ISTAT .NE. 2 ) THEN                                           ! =2 MEANS IT IS A NUMERIC
          IERROR = IERROR + 1                                           ! DCODE PRINTED AN ERROR
          GOTO 100
      ENDIF
      IF( TYPE(NPARAM) .EQ. 'F') THEN
          VALS(NPARAM) = AREAL                                          !  FLOATING POINT VALUES
          GOTO 100
      ENDIF
      IF( type(nparam) .EQ. 'L' ) THEN                                  !  32 BIT INTEGER VALUES
          LVALS(NPARAM) = AREAL
          GOTO 100
      ENDIF
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
 1010 FORMAT(' ***  ERROR  ***  THE PARAMETER ',A6,' IS INCORRECT.')
 1030 IF(OFMT.GE.0.AND.OFMT.LE.6) GO TO 1040
      PRINT 1010, NAMES(3)
      IERROR=IERROR+1
 1040 CONTINUE
      IF( ounit2 .GE. 0 .AND. outdevice .NE. ' ' ) THEN
          PRINT *,' *** ERROR ***  OUNIT2 and DEVICE are incompatible.'
          ierror = ierror + 1
      ENDIF
      IF( ounit .LT. 0 .AND. ounit2 .LT. 0 ) ounit = 0
C****
C****   SET THE COMMON VARIABLES FOR THE EXECUTION PHASE
C****
      IOUNIT=OUNIT
      FMT=OFMT
      LFOR=FON
      POSAF=POSAFT
      oreeln=oreel
      ONUMTR=ONTRCS
      nskip = nfskip
      rewindo = rewind
      itrace0 = trace0
      iounit2 = ounit2
      IF( IAND(LPRINT,1) .EQ. 1 ) THEN
          PRINT *,' Output parameters:'
	      PRINT *,OREEL,IOUNIT,FMT,LFOR,POSAF, ONUMTR,
     *       nskip, outdevice, nskip, rewindo, itrace0, iounit2
      ENDIF
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
