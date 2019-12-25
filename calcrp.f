      SUBROUTINE CALCRP(LHEAD,GXP,GGX,ngxps)
C     CALCRP COMPUTES AND STORES (IN THE SEGY TRACE HEADER) SOME OF THE INVARIANT
C  PARAMETERS ASSOCIATED WITH A TRACE.  CALCRP ASSUMES THE SHOT NUMBER AND TRACE
C  NUMBER ARE ALREADY IN THE HEADER.
C     CALCRP PUTS THE FOLLOWING INFO IN THE HEADER:
C      WORD 6 - THE CDP OR RP NUMBER
C      WORD 10 - THE RANGE OR OFFSET (THE SHOT-RECEIVER DISTANCE)
c  stopped doing 19 and 21 on 26 Oct 00
C      WORD 19 - THE X COORDINATE OF THE SHOT
C      WORD 21 - THE X COORDINATE OF THE RECEIVER
C
C *** NOTE ***
C   THIS ROUTINE ASSUMES IN LINE SHOOTING.  NO ANGLES ARE USED.
C
C  METHOD:
C     CALCRP ASSUMES THE SEISMIC LINE IS SHOT IN A STRAIGHT LINE.
c  If itype = 1, THE SHOT HAS AN X-COORDINATE ASSIGNED TO IT BY ADDING
c THE DISTANCE FROM THE LAST SHOT TO THE X-COORDINATE OF THE LAST SHOT
c  If itype = 2, THE SHOT HAS AN X-COORDINATE ASSIGNED TO IT BY MULTIPLYING
c THE DISTANCE FROM THE LAST SHOT by the shot point number.
c EACH RECEIVER HAS AN X-COORDINATE OF

C  THE SHOT COORDINATE PLUS THE RANGE TO THE RECEIVER.  THE RP X-COORDINATE IS
C  THEN CALCULATED BY ASSUMING THAT THE RP IS HALFWAY BETWEEN THE SHOT AND THE
C  RECEIVER.  THE RP NUMBER IS THEN CALCULATED BY DIVIDING THE RP X-COORDINATE
C  BY THE DISTANCE BETWEEN RPS AND TRUNCATING TO AN INTEGER.
C     THE COORDINATE OF THE FIRST SHOT IS THE SHOT NUMBER (FROM THE HEADER)
C  TIMES THE DISTANCE FROM THE PREVIOUS SHOT.
C
C  ARGUMENTS:
C     LHEAD  - THE HEADER ARRAY.
C     GXP    - THE GROUP NUMBER-RANGE PAIR ARRAY.  THIS ARRAY MUST BE TERMINATED
C              BY A -1 IN THE LAST ELEMENT.  SEE FUNCTION RANGE.
C     GGX    - THE GROUP TO GROUP DISTANCE FOR CALCULATING RANGES.
C
C  COMMON NEEDED:
C     COMMON /RPCALC/ DFLS,DBRPS,SMEAR,itype
C     DFLS   - THE DISTANCE FROM THE LAST SHOT
C     DBRPS  - THE DISTANCE BETWEEN RPS.
C     SMEAR  - THE DISTANCE IN THE SUBSURFACE IN WHICH TO SEARCH FOR AN RP.
C              THE SMEAR IS CENTERED AROUND THE RP.  IE  ALLOWABLE RP
C              COORDINATE LIES IN THE INTERVAL (RP-SMEAR/2.,RP+SMEAR/2.]
C
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, DECEMBER 1979
c  mod Mar 1991 to add itype 2
c  mod Mar 1997 - stop writing shot and reciver coordinates in
c                 SEG-Y header.
c  mod 15 Apr. 98 - Add type 6 (ASA geom)
c  mod 21 Dec. 98 - Write shot and receiver coordinates in SEG-Y header.
c  mod 26 Oct. 00 - Stopped clobbering header words 19 and 21
c  mod 21 May 01 - Add lunelev to COMMON /RPCALC/ so words 19 and 21
c                  get the x coordinates when elevations are given.
c  mod Oct 02 - FLOAT a couple of integers before doing the math.
c  mod 6 Nov 02 - Add "EXTERNAL RANGE" for the HPs
c  mod 21 May 03 - Write the source and receiver x,y into header
c                  words 19,20,21,22 if told to!
c  mod 12 Aug 05 - Type 14 (Healy 05) is really type 9
c  mod 21 Jul 06 - Type 16 (Healy 06) is really type 9
C
      COMMON /RPCALC/ DFLS,DBRPS,SMEAR, itype, lunelev, iwritexy
      DIMENSION LHEAD(111),GXP(111)
      LOGICAL FIRST
      SAVE
      EXTERNAL RANGE
      DATA FIRST /.TRUE./, LS/-32768/
C
      IF(.NOT.FIRST) GO TO 100
      XS = FLOAT(LHEAD(3)) * DFLS                                       ! GET A STARTING COORDINATE
      LS=LHEAD(3)
  100 CONTINUE
      IF(LS.EQ.LHEAD(3)) GO TO 200                                      !  SAME SHOT AS BEFORE
      IF( itype .EQ. 1 .OR. itype .EQ. 6 .OR. itype .EQ. 9 .OR.
     &    itype .EQ. 14 .OR. itype .EQ. 16 )
     &     XS=XS+DFLS                                                   !  ADD IN THE DISTANCE FROM THE LAST SHOT
      IF( itype .EQ. 2 ) xs = FLOAT(lhead(3)) * dfls
      LS=LHEAD(3)                                                       !  SET THE LAST SHOT TO THIS SHOT
  200 CONTINUE                                                          !  CALCULATE THE X-COORDINATE OF THE RECEIVER
      SMEAR2=SMEAR/2.
      TRNO=LHEAD(4)                                                     !  FLOAT THE TRACE NUMBER
      IF( ngxps .NE. 0 ) THEN
          RX=RANGE(GXP,GGX,TRNO)                                        !  FIND THE RANGE
      ELSE
          rx = FLOAT(lhead(10))
      ENDIF
      XR=XS+RX                                                          !  THE X-COORDINATE OF THE RECEIVER
      XRP=(XR+XS)/2.                                                    ! THE X-COORDINATE OF THE RP
      TEMP=XRP/DBRPS
      IF(TEMP.LT.0) TEMP=TEMP-.5
      IF(TEMP.GT.0) TEMP=TEMP+.5
      LTEMP=TEMP                                                        !  THE CLOSEST RP, TRUNCATED
      TEMP1=LTEMP*DBRPS
      IF(.NOT.FIRST) GO TO 220
      FIRST=.FALSE.
      ADD=TEMP1-XRP
  220 CONTINUE
      TEMP1=TEMP1-ADD
      IF(XRP.LE.TEMP1+SMEAR2) GO TO 250
      TEMP1=TEMP1+DBRPS
      LTEMP=LTEMP+1
      IF(XRP.LT.TEMP1-SMEAR2) GO TO 350
      GO TO 400                                                         ! IT IS IN THE BIN
  250 IF(XRP.GE.TEMP1-SMEAR2) GO TO 400
      TEMP1=TEMP1-DBRPS
      LTEMP=LTEMP-1
      IF(XRP.LE.TEMP1+SMEAR2) GO TO 400
  350 LTEMP=524272                                                      ! INSTEAD OF KILLING THE TRACE, JUST DROP IT IN GATHER
      GO TO 400
  400 CONTINUE                                                          ! FINISH BY PUTTING THE DATA IN THE HEADER
      LHEAD(6)=LTEMP                                                    ! THE RP NUMBER
      LHEAD(10)=NINT(RX)                                                !  THE SHOT-RECEIVER DISTANCE
      IF( iwritexy .GT. 0 .OR. lunelev .NE. 0 .AND. itype .NE. 14 .AND.
     &    itype .NE. 16 ) THEN
          LHEAD(19)=XS                                                  !  THE X-COORDINATE OF THE SHOT
          LHEAD(21)=XR                                                  !  THE X-COORDINATE OF THE RECEIVER
      ENDIF
      RETURN
      END
