      SUBROUTINE WBTED( numwbt )
C                  PROCESS WBT  (WATER BOTTOM TIME DEFINITION)
C                  ------- ---
C
C  WRITEUP DATE:  29 April 1993
C
C     PROCESS WBT ASSOCIATES A WATER BOTTOM TIME WITH EVERY TRACE.  THIS TIME IS
C  PLACED IN THE TRACE HEADER AND THEN MAY BE USED BY OTHER PROCESSES SUCH
C  AS MUTE, NMO, OR DECON FOR 'HANGING' WINDOWS FROM. 'HANGING' WINDOWS MEANS
C  THAT THE WINDOW TIMES ARE ADDED TO THE WATER BOTTOM TIME, THUS THE
C  WINDOW TIMES MAY BE THE SAME DISTANCE FROM THE WATER BOTTOM ON ALL TRACES.
C    THE WATER BOTTOM TIMES MAY BE ENTERED IN ONE OF SEVERAL WAYS: 1) BY RP-TIME
C  PAIRS;   2) BY GMT TIME-TIME PAIRS;  3)  CONVERTING DEPTHS TO TIME.
C     THE RP-TIME PAIRS METHOD OF ENTERING THE WATER BOTTOM TIMES CONSIST OF
C  MEARLY ENTERING A LIST OF PAIRS OF RP NUMBERS FOLLOWED BY THE TWO WAY TRAVEL
C  TIME FOR THE RP.  THE RP NUMBERS MUST STRICTLY INCREASE.  RP numbers
c  may be assigned using SIOSEIS processes GEOM or HEADER.
C  THE WATER BOTTOM TIME SPECIFIED WILL BE ASSOCIATED WITH ALL TRACES WITH
C  THE SAME RP NUMBER (A SINGLE CDP HAS THE SAME RP NUMBER!).  WATER BOTTOM
C  TIMES NOT SPECIFIED ARE CALCULATED BY INTERPOLATION OR HELD CONSTANT.
C     THE GMT TIME METHOD CONSISTS OF SPECIFING THE JULIAN DAY VIA THE PARAMETER
C  'DAY' AND THEN A LIST OF GMT TIME - WATER BOTTOM TIME PAIRS.  THE PARAMETER
C  'DAY' MUST BE GIVEN PRIOR TO THE GMT TIME AND MUST BE GIVEN ON THE FIRST
C  PAIR.  FURTHERMORE, THE DAY MUST BE GIVEN ON DAY CHANGES.  THE GMT TIME IS
C  GIVEN IN TERMS OF DECIMAL MINUTES OF THE 24 HOUR CLOCK, THUS 1532.75 
c  REPRESENTS 1532 AND 45 SECONDS.  WATER BOTTOM TIMES NOT
C  SPECIFIED ARE CALCULATED BY INTERPOLATION OR ARE HELD CONSTANT.
C  EXAMPLE:
C      WBT
C         41 1.197
C         43 1.199
C      END
C  RPS LESS THAN 41 WILL RECEIVE A WATER BOTTOM TIME OF 1.197, RP 41 A TIME
C  OF 1.197, 42 A TIME OF 1.198, 43 A TIME OF 1.199, AND RPS LARGER THAN 43 A
C  TIME OF 1.199.
C  EXAMPLE 2):
C      WBT
C       DAY 265 1045 .21
C               1100 .25
C               2200 2.0
C       DAY 266 0300 3.5
C      END
c
c    THE DEPTH CONVERSION METHOD CONVERTS THE WATER BOTTOM DEPTH IN WORD 107
C  OF THE TRACE HEADER INTO A TIME USING A VELOCITY OF 1500 METERS PER SECOND.
C
c  THE PARAMETER DICTIONARY
C  --- --------- ----------
C
C  DAY    - The Julian day of the GMT-time pairs that follow.
c  VEL    - The velocity to use in converting the SeaBeam depth to seismic time.
c           Preset = 0.
c  OFFSET - The number of traces between the SeaBeam depth (center of
c            the ship) and the reflection point. 
c           Preset = 3
c  NSCAN  - The number of traces to scan for the "closest" SeaBeam depth.
c           only.   NSCAN/2 traces will be scanned in both forward and aft 
c           directions.
c           Preset = 10
C
C    *** NOTE ***
C    1)  THE ONLY 'END' NEEDED IS THE 'END' TO TERMINATE THE WBT PARAMETERS.
C
C  COPYRIGHT (C), PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
c    mod April 1987 to add the SeaBeam picker.
c    mod 24 July 1990 to make the day/gmt a REAL
c    mod 29 Apr 93 - change the documentation for depth option
c    mod 21 Sep 95 - Add SEL, SES, SOLRAT - an auto picker.
c                  - Remove TYPE
c                  - add PRESTK
c    mod 7 Feb 96 - Adding SEl, SES etc made lists need extra end
c    mod 6 June 97 - Add threshold picking
c    mod Aug 97 - Correctly preset thres
c    mod 22 Oct 98 - Add PEAK
c    mod May 99 - Add SEPP
c    mod June 00 - Add INDEX
c    mod June 01 - Add numwbts and make it re-enterable
c    mod July 01 - Add an edit check for solrat when ses or sel given
c    mod 3 Aug 01 - Add parameter TRACK
c    mod 21 Nov 02 - preset the common to zero FIRST
c                  - Add some error checks
c    mod 9 Sep 05 - Change nscan preset from 11 to 0
c    mod 22 May 07 - Add error if PRESTK is not YES or 1
c    mod 5 Oct 09 - Add bpass
c    mod 10 Jul 14 - Add guided & seg
c
      PARAMETER (npars = 18)
      PARAMETER (MAXWBTS=3)
      REAL ldummy, lrp, lastrp
      DIMENSION ldummy(2)
      EQUIVALENCE (lrp,ldummy(1)), (time,ldummy(2))
      CHARACTER*80 TOKEN
      CHARACTER *6 names(npars)
      CHARACTER *1 types(npars)
      DIMENSION length(npars)
      DIMENSION vals(npars),lvals(npars)
      EQUIVALENCE (vals(1),lvals(1))
      INTEGER day, offset, ns, prestk, prestack
      REAL solrat, selong, seshort, sepeak
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /WBOT/ IWUNIT(MAXWBTS), iday(MAXWBTS), maxwd(MAXWBTS),
     &  lprin(MAXWBTS), loff(MAXWBTS), vela(MAXWBTS), nsca(MAXWBTS),
     &  sol(MAXWBTS), sel(MAXWBTS,2), ses(MAXWBTS,2), prestack(MAXWBTS),
     &  threshold(MAXWBTS), peakp(MAXWBTS), sep(MAXWBTS),
     & sepp(MAXWBTS,2), index1(MAXWBTS), trac(MAXWBTS), pass(MAXWBTS,2),
     &  guide(MAXWBTS), seg(MAXWBTS,2)
      EQUIVALENCE (day,lvals(1)),
     2            (lprint,lvals(3)),
     3            (offset,lvals(4)),
     4            (vel,vals(5)),
     5            (nscan,lvals(6)),
     7            (maxwrd,lvals(7)),
     8            (solrat,vals(8)),
     9            (selong,vals(9)),
     *            (seshort,vals(10)),
     1            (thres,vals(11)),
     2            (peak,vals(12)),
     3            (sepeak,vals(13)),
     4            (index,vals(14)),
     5            (track,vals(15)),
c     6            (ipass,vals(16))
     7            (guided,vals(17)),
     8            (seguide,vals(18))
      DATA names/'DAY   ','PRESTK','LPRINT','OFFSET','VEL   ',
     *           'NSCAN ','MAXWRD','SOLRAT','SEL   ','SES   ',
     *           'THRES ','PEAK  ','SEPP  ','INDEX ','TRACK ',
     *           'PASS  ','GUIDED','SEG   '/
      DATA length/3,6,6,6,3,5,6,6,3,3,5,4,4,5,5,4,6,3/
      DATA types/'L','L','L','L','F','L','L',4*'F','A','F','L','F',
     &           'F','F','F'/
C****
C****       Preset the parameters and GET A FILE FOR THE PARAMETERS
C****
      LASTRP=-32768
      lprint = 0
      day = 0
      lrp = 0
      iend = 0
      prestk = 0
      offset = 3
      vel = 0.
      nscan = 0
      maxwrd = 11000
      solrat = 0.
      selong = 0.
      seshort = 0.
      thres = 0.
      peak = 0.
      sepeak = 0.
      track = 99.
      guided = 0.
      CALL GETFIL(1,IWUNIT(numwbt),TOKEN,ISTAT)
      ns = 0
      ntokes = 1
      index = 50
      iday(numwbt) = 0
      maxwd(numwbt) = 0
      lprin(numwbt) = 0
      loff(numwbt) = 0
      vela(numwbt) = 0.
      nsca(numwbt) = 0
      sol(numwbt) = 0.
      sel(numwbt,1) = 0.
      sel(numwbt,2) = 0.
      ses(numwbt,1) = 0.
      ses(numwbt,2) = 0.
      prestack(numwbt) = 0.
      threshold(numwbt) = 0.
      peakp(numwbt) = 0.
      sep(numwbt) = 0.
      sepp(numwbt,1) = 0.
      sepp(numwbt,2) = 0.
      index1(numwbt) = 0
      trac(numwbt) = 0.
      pass(numwbt,1) = 0.
      pass(numwbt,2) = 0.
      guide(numwbt) = 0.
      seg(numwbt,1) = 0.
      seg(numwbt,2) = 0.
C****
C****      GET A TOKEN
C****
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)
      IF( NCHARS .EQ. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE
          NTOKES=0
          GO TO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
  150 ntokes = ntokes + 1
      DO 160 i = 1, npars
         len = length(i)
         nparam = i
         IF( token(1:nchars) .EQ. names(i)(1:len) ) GOTO 200
  160 CONTINUE
      IF( token(1:nchars) .EQ. 'END' ) THEN
          iend = iend + 1
          GOTO 300
      ENDIF
      CALL dcode( token, nchars, value, istat )
      IF( istat .NE. 2 ) THEN
          PRINT *,' ***  ERROR  ***  WBT does not have a parameter ',
     *    'named ',token(1:nchars)
          ierror = ierror + 1
          GOTO 100
      ENDIF
      IF( ns .EQ. 0 ) THEN
          lrp = value
          ns = 1
          GOTO 100
      ELSE
          time = value
          ns = 0
      ENDIF
      IF( day .NE. 0 ) THEN
          IF( day .LT. 0 .OR. day .GT. 366 ) THEN
              PRINT *,' ***  ERROR  ***  Incorrect julian day of ',day
              ierror = ierror + 1
          ENDIF
          IF( lrp .LT. 0 .OR. lrp .GT. 2400 ) THEN
              PRINT *,' ***  ERROR  ***  Incorrect GMT of ',lrp
              ierror = ierror + 1
          ENDIF
          lrp = day * 10000. + lrp
          IF( lrp .LT. lastrp ) THEN
              PRINT *,' ***  ERROR  ***  GMT and DAY must increase.'
              ierror = ierror + 1
          ENDIF
      ELSE
          IF( lrp .LT. lastrp ) THEN
              PRINT *,' ***  ERROR  ***  RP numbers and GMT must ',
     *             'increase.'
              ierror = ierror + 1
          ENDIF
      ENDIF
      CALL wrdisc( iwunit(numwbt), ldummy, 2 )
      IF( IAND(lprint,1) .NE. 0 ) PRINT *,' lrp=',lrp,' time=',time
      GOTO 100
  200 CONTINUE
      ns = 0
  210 CALL getoke( token, nchars )
      IF( nchars .EQ. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  Enter parameters  >'
          CALL rdline
          ntokes = 0
          GOTO 200
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF( names(nparam) .EQ. 'PRESTK' ) THEN
          IF( token(1:1) .NE. 'Y' .AND. token(1:1) .NE. '1' ) THEN
              PRINT *,' ***  ERROR  ***  PRESTK must be YES or 1'
              ierror = ierror + 1
          ENDIF
          IF( token(1:1) .EQ. 'Y' ) prestk = 1
          IF( token(1:1) .EQ. '1' ) prestk = 1
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'PEAK' ) THEN
          IF( token(1:3) .EQ. 'POS' ) peak = 1.
          IF( token(1:3) .EQ. 'NEG' ) peak = 2.
          IF( token(1:3) .EQ. 'ABS' ) peak = 3.
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'GUIDED' ) THEN
          IF( token(1:3) .EQ. 'POS' ) guided = 1.
          IF( token(1:3) .EQ. 'NEG' ) guided = 2.
          IF( token(1:3) .EQ. 'ABS' ) guided = 3.
          GOTO 100
      ENDIF
      CALL dcode( token, nchars, value, istat )
      IF( istat .NE. 2 ) THEN
          PRINT *,' ***   ERROR  ***  WBT expected a number instead of',
     *      token(1:nchars)
         ierror = ierror + 1
         GOTO 100
      ENDIF
      IF( types(nparam) .EQ. 'F' ) THEN
          vals(nparam) = value
          IF( names(nparam) .EQ. 'SEL' ) THEN
              ns = ns + 1
              sel(numwbt,ns) = value
              IF( ns .EQ. 1 ) GOTO 210
              ns = 0
              GOTO 100
          ENDIF
          IF( names(nparam) .EQ. 'SES' ) THEN
              ns = ns + 1
              ses(numwbt,ns) = value
              IF( ns .EQ. 1 ) GOTO 210
              ns = 0
              GOTO 100
          ENDIF
          IF( names(nparam) .EQ. 'SEPP' ) THEN
              ns = ns + 1
              sepp(numwbt,ns) = value 
              IF( ns .EQ. 1 ) GOTO 210
              ns = 0
              GOTO 100
          ENDIF
          IF( names(nparam) .EQ. 'PASS' ) THEN
              ns = ns + 1
              pass(numwbt,ns) = value 
              IF( ns .EQ. 1 ) GOTO 210
              ns = 0
              GOTO 100
          ENDIF
          IF( names(nparam) .EQ. 'SEG' ) THEN
              ns = ns + 1
              seg(numwbt,ns) = value 
              IF( ns .EQ. 1 ) GOTO 210
              ns = 0
              GOTO 100
          ENDIF
      ENDIF
      IF( types(nparam) .EQ. 'L' ) lvals(nparam) = value
      GOTO 100
c****
c****     Check the parameters for validity
c****
  300 CONTINUE
      IF( sel(numwbt,2) + ses(numwbt,2).NE. 0 .AND. solrat .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  SOLRAT must be given with ses/sel.'
          ierror = ierror + 1
      ENDIF
      IF( sel(numwbt,1) .GT. sel(numwbt,2) ) THEN
          PRINT *,' ***  ERROR  ***  Bad SELL.'
          ierror = ierror + 1
      ENDIF
      IF( ses(numwbt,1) .GT. ses(numwbt,2) ) THEN
          PRINT *,' ***  ERROR  ***  Bad SES.'
          ierror = ierror + 1
      ENDIF
      IF( sepp(numwbt,1) .GT. sepp(numwbt,2) ) THEN
          PRINT *,' ***  ERROR  ***  Bad SEPP.',
     &        sepp(numwbt,1), sepp(numwbt,2) 
          ierror = ierror + 1
      ENDIF
      IF( track .NE. 99. .AND. track .GT. 1 ) THEN
          PRINT *,' ***  WARNING  ***  Unusually large TRACK.'
          PRINT *,' TRACK is in units of time, not depth.'
          iwarn = iwarn + 1
      ENDIF
      IF( guided .NE. 0 .AND. seg(numwbt,1) .EQ. 0 .AND. 
     &    seg(numwbt,2) .EQ. 0 ) THEN
      PRINT *,' ***  ERROR  ***  SEG must be given when GUIDED is used'
          ierror = ierror + 1
      ENDIF
      iday(numwbt) = day
      nsca(numwbt) = nscan
      vela(numwbt) = vel
      loff(numwbt) = offset
      maxwd(numwbt) = maxwrd
      lprin(numwbt) = lprint
      sol(numwbt) = solrat
      prestack(numwbt) = prestk
      threshold(numwbt) = thres
      peakp(numwbt) = peak
      sep(numwbt) = sepeak
      index1(numwbt) = index
      trac(numwbt) = track
      guide(numwbt) = guided
      IF( IAND(lprint,1) .NE. 0 ) THEN
          PRINT *,' numwbt=',numwbt,' off=',loff,' vel=',vela,
     &            ' nscan=',nscan
          PRINT *,' solrat=',solrat,' sel=',sel,' ses=',ses
          PRINT *,' prestack=',prestack,' thres=',threshold
          PRINT *,' peak=',peakp,' index=',index1,' track=',track
          PRINT *,' guided=',guide,' seg=',seg
      ENDIF
      IF( ns .EQ. 1 ) THEN
          PRINT *,' ***  ERROR  ***  WBT pairs pairs are not pairs!'
          ierror = ierror + 1
      ENDIF
 2000 IF( lrp .NE. 0 ) RETURN
c 2000 IF( solrat+selong+seshort+thres+peak .EQ. 0. ) RETURN
      CALL getoke( token, nchars )
      IF( nchars .LE. 0 ) THEN
          IF( now .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL rdline
           ! get a new line of parameters
          ntokes = 0
          GOTO 2000
      ENDIF
      CALL upcase( token, nchars )
      ntokes = ntokes + 1
      IF(  token(1:nchars) .NE. 'END' .OR. nchars .NE. 3 ) THEN
          PRINT *,' ***  ERROR  ***  WBT allows only one fno/lno list.'
          ierror = ierror + 1
      ENDIF
      RETURN

      END
