      SUBROUTINE GENTL(IBUF,TLINES,VSCALE,TIMEL,XOFF,idir)
C     GENTL GENERATES AN ARRAY OF TIME LINE RASTERS.  THE OUTPUT IS AN ARRAY
C IN ZEROES AND ONES WITH ONES BEING IN THE APPROPRIATE BITS FOR THE TIME
C LINE MARKINGS.  THE ARRAY SHOULD BE OR'ED WITH THE DATA ARRAY BEFORE SENDING
C TO THE RASTER PLOTTER.
C     THE TIME LINE FREQUENCY WILL BE GENERATED ACCORDING TO THE VALUES IN THE
C ARRAY TLINES , WITH THE VERTICAL SCALE OF VSCALE.  THE FIRST ENTRY OF TLINES
C WILL BE ONE DOT WIDE, THE SECOND WILL BE TWO DOTS WIDE, THE THIRD WILL BE
C THREE DOTS WIDE, ETC.  ODD ENTRIES WILL HAVE THE LINE CENTRED WHILE THE
C EVEN ENTRIES WILL HAVE THE EXTRA LINE PRIOR TO THE CORRECT PLACE.
C
C ARGUMENTS:
C        IBUF   - AN INTEGER*2 ARRAY THAT WILL CONTAIN THE RASTERS.
C                 THE LENGTH OF IBUF MUST BE AT LEAST 180 16 WORDS LONG.
C
C        TLINES - TIME LINE ARRAY.  TIMES ARE FLOATING POINT SECONDS.
C                 UP TO 4 TIME LINES MAY BE GIVEN.
C
C        VSCALE - VERTICAL SCALE IN INCHES PER SECOND OF THE TIME LINES
C                 TO BE GENERATED.
C
C        TIMEL  - TIME LENGTH ( THE MAXIMUM TIME) TO PLOT.
C                 FLOATING POINT SECONDS.
C
C        XOFF   - OFFSET OF THE FIRST POINT IN INCHES.
c        idir   - The direction of the plot.
c               >0, a Normal plot (time 0 at the top) - right to left plot
c               <0, a reverse plot (time 0 at the bottom) - left to right plot
C
C    COMMON REQUIREMENTS:
C    COMMON /VERSAT/NIBS,RNIBS
C      NIBS  - THE NUMBER OF NIBS (DOTS OR RASTERS PER INCH) OF THE PLOTTER.
C              32 BIT INTEGER.
C      RNIBS - THE REAL OF NIBS!
C
C
C   COPYRIGHTED BY:
C         PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, NOVEMBER 1978
c  modified by pch 23 Aug 89 for the difference between DEC UNIX and DEC VMS
c mod 12 May 92  Remove DUTABLE - DecStation is same as Dec Vax
c mod 22 May 92 - add color
c mod 8 feb 93 - add rgb (rgb=0 means CYM, where white and black are
c                opposite to RGB.  ie in CYM white is no planes and
c                black is all planes - sorta like a B&W plot.
c  20 Apr 98 - Change common defs and colors from 6 to 8
c  13 Jan 04 - Change common defs and colors from 8 to 9
c  10 Dec 07 - Honor TRIM
C
c****  This falls apart if OFFSET = 0 or less.
C
      DIMENSION IBUF(1),TLINES(1)
      INTEGER*2 IBUF
      COMMON /SEISPL/OFFSET,DEF,PCTFIL,dummy,TRPIN,ITYPE,dummies(4),
     *  STIMEL,dumm1,IUNPLT,WIGGLE,BIAS,idummy,ICDOTS,scalar,clip,itlan,
     &  irectify, ndptr, chart(2), size, itrim
      COMMON /colors/ defs(9), colors(9), ncolors, bcolor, rgb
      INTEGER colors, bcolor, rgb
      COMMON /VERSAT/ NIBS,RNIBS
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
C*** ICOMPT=1 MEANS PRIME, ICOMPT=2 MEANS DEC
      INTEGER*2 ITABLE(16),DVTABLE(16),DUTABLE(16)
C****  NON-DEC GOES 2**15,2**14,2**13,...,2**0
C****  DEC UNIX GOES 2**0,2**1,2**2,....,2**15    , THERFORE USE DTABLE
c***** DEC VMS goes 2**8,2**9,..2**15, 2**0,2**1,....2**7
      DATA ITABLE/-32768,16384,8192,4096,2048,1024,512,
     *          256,128,64,32,16,8,4,2,1/
      DATA DUTABLE /1,2,4,8,16,32,64,128,256,512,1024,
     *   2048,4096,8192,16384,-32768/
      DATA DVTABLE/128,64,32,16,8,4,2,1,
     *      -32768,16384,8192,4096,2048,1024,512,256/
C
      NBITS=IFIX(TIMEL*VSCALE*RNIBS)+1
      IPOINT=0
      NDONE=0
      joff = xoff * rnibs
      OFF=XOFF*RNIBS
      IF( itrim .GT. 0 ) THEN
          IF( idir .GT. 0 .AND. ( itrim .EQ. 1 .OR. itrim .EQ. 2 )) THEN
             off = 17
             joff = 17
          ENDIF
      ENDIF
      IF( idir .LT. 0 ) off = off + nbits - 1                            ! start at the bottom!
      IOFF=OFF
  100 NDONE=NDONE+1
      IBIT=IFIX(OFF)
      MBIT=IFIX(TLINES(NDONE)*VSCALE*RNIBS+.5)                          ! THE INC BETWEEN TLINES
      IF(TLINES(NDONE).EQ.0.OR.NDONE.GT.4) RETURN
  200 CONTINUE
      IF( idir .GT. 0 .AND. IBIT .GT. NBITS+IOFF ) GO TO 300
      IF( idir .LT. 0 .AND. ibit .LT. joff ) GOTO 300
      IWORD=IBIT/16
      JBIT=IBIT-IWORD*16                                                ! THE BIT WITHIN IWORD
      IF( JBIT .EQ. 0 )THEN
          IWORD = IWORD - 1
          JBIT = 16
      ENDIF
      IF( ICOMPT .EQ. 2 .OR. icompt .EQ. 4 ) THEN
          IF( ncolors .EQ. 0 .OR. rgb .EQ. 0 ) THEN
              IBUF(IWORD) = IOR(IBUF(IWORD),DVTABLE(JBIT))               ! IT'S A DEC
          ELSE
              ibuf(iword) = IAND(ibuf(iword),NOT(dvtable(jbit)))
          ENDIF
      ELSE
          IF( ncolors .EQ. 0 .OR. rgb .EQ. 0 ) THEN
              IBUF(IWORD) = IOR(IBUF(IWORD),ITABLE(JBIT))                ! IT'S not DEC
          ELSE
              ibuf(iword) = IAND(ibuf(iword),NOT(itable(jbit)))
          ENDIF
      ENDIF
      IF( idir .GT. 0 ) THEN
          IBIT=IBIT+MBIT
      ELSE
          ibit = ibit - mbit
      ENDIF
      GO TO 200
C
C    MAKE SURE THE NEXT SET OF TIME LINES IS DARKER
  300 CONTINUE
      IF( NDONE .EQ. 1) THEN
          OFF = OFF + 1.                                                ! CHANGE THE OFFSET BY 1 DOT
          GO TO 100
      ENDIF
      IF( NDONE .EQ. 2 ) THEN
          OFF = OFF - 2.                                                ! 1 DOT BEFORE THE ORIGINAL OFFSET
          GOTO 100
      ENDIF
      IF( NDONE .EQ. 3 ) THEN
          OFF = OFF + 3.                                                ! 2 DOTS AFTER THE ORIGINAL OFFSET
          GOTO 100
      ENDIF
      END
