      SUBROUTINE PLOTED(BUF,IBUF,LBUF,ISCR,LSCR,SCR)
C                          PROCESS PLOT    (SECTION PLOT)
C                          ------- ----
c
C
C  COPYRIGHT (C), PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, AUGUST 1983
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
c
c  mod 15 July 90 - add anntyp 9 (GMT+rp)
c  mod 1 May 90 - Make Apollo a Unix machine
c  mod 18 Apr 90 - add the Epson "rugged Writer 480" (nibs=120)
c  mod 5 Apr 90 - use -wiggle when dir = l2r
c  mod 23 jan 90 - add the Versatec 7424 plotter
c  mod 16 Nov 89 - make the plot header blank if column 1 is not ASCII
c  mod 14 Nov 89 - add anntyp 8 (espn)
c  mod 3 Nov 89 - add the Calcomp 5845 plotter.
c  mod 6 feb 91 - remove the CHAR(15) from character 80 when a Versatec
c  mod 6 Aug 91 - add frange, lrange, srpath, sipath
c  mod 14 Aug 91 - Add Versatec 9242 and 8242
c  mod 20 Aug 91 - Add rstime
c  mod 22 Nov 91 - Add Versatec 7436 (Larry Mayer)
c  mod 30 Jan 92 - Change stime preset from -1. to -99999. to allow stime -1
c  mod 4 Feb 92 - Add parameters absval, ann, hdr, ihdr, lhdr.
c  mod 24 Feb 92 - Add the local time to the plot header.
c  mod 20 Mar 92 - Add the Raytheon 850 and the HP 7600
c  mod 14 May 92 - Add color
c  mod 30 Sep 92 - add NovaJet, 300 dpi, 36 in., Color inkjet plotter
c  mod 12 Oct 92 - add HP 2848, 300 dpi, 36 in., B&W inkjet plotter
c  mod 2 Nov 92 - add HP 2847, 300 dpi, 24 in., B&W inkjet plotter
c  mod 4 Feb 93 - The !@#$%^&* Versatec 3444 is CYM not RGB
c               - add the Versatec 3444
c  mod 26 Mar 93 - Add common ver for print the SIOSEIS Version
c  mod 20 Apr 94 - Add the OYO GS-624 plotter
c  mod 3 May 94 by gk - Allow 45 seconds of data to be plotted (was 30).
c  mod ????      - Do not create plot file if plot is not in procs
c  mod 10 Nov 94 - Correct bcolor and colors for 'BLACK' and 'WHITE'
c  mod 30 Nov 94 - Add screen plotter (nibs 75)
c  mod 13 Feb 95 - Write nibs value on line 1 of sioseis plot file.
c                - add nibs 3436 (Versatec), 9800, 9315
c  mod 24 Feb 95 - Change icdots preset to 1
c                - Add parameter tlann
c  7 June 95 - Change nibs 300 from NovaJet to HP color LaserJet
c              (NovaJet should be the same as the HP DesignJet)
c  4 Jan 96 - DesignJet (any 300 dpi) should have vscale at .41666 or .42666
c  12 Feb 96 - flip color order and def and negate def when dir = ltr
c  23 Oct 97 - Add parameter WRAP
c  17 Apr 98 - Increase defs and colors from 6 to 8
c  4 June 98 - Add error check for decreasing tlines.
c  11 Nov 98 - Add annotation type sh&tr and rp&tr
c  8 Nov 98 - Add the Alantec 2400R (23,52in, 200dpi, 588 bytes)
c  19 Apr 99 - Change ASCII format size of trpin and vscale so
c              they don't overflow and confuse program overlay
c  27 Oct 99 - Allow color gray - for gray scale or variable density.
c  24 Sep 00 - Add parameter RECSP
c  2 Mar 01 - The plotter size on nibs 200 (Versatec 1200) is 10.56 not 11.
c 30 Mar 01 - Add parameter HPATH - Header file output
c 4 May 01 - Change format of vscale in plot header to allow vscale > 99
c  15 Jan 02 - Linux was choking with the null on opath and hpath
c  14 Oct 02 - Add parameters ANNTYP2, ANN2 and RECTIFY.
c  31 Jan 03 - colors gray needed the deflections to be * 2.
c  7 Feb 03 - Do error check for deflection-graylevel pair order.
c           - Add parameter dptr (dots per trace).
c  24 Feb 03 - Make new paramter NOINC equivalent to NINC.
c  13 Jan 04 - Increase defs and colors to 9 in common
c  14 Apr 04 - Add nibs 2368 - iSys V12
c  11 May 06 - Don't do nsecs 0 in ploted since diskin might change nsamps
c            - Change nsecs preset to 0
c  14 Mar 07 - Add CHART
c  5 April 07 - Add PLOTTER and make NIBS = PLOTTER
c  27 Apr. 07 - Make HPATH a SEG-Y file by adding the ASCII and binary headers.
c  19 July 07 - Change offset to 0 for iSys (2368) plotter.
c  23 July 07 -  Add size to common/seispl/
c  14 Sep 07 - Add some error checks for chart
c  2 Oct 07 - colors didn't work because an undocumented change was bad!
c  7 Dec 07 - Add parameter TRIM
c  22 Apr 08 - Change nibs preset to 2959
c  4 Aug 08 - Add ann/ann2 lat/log
c  28 Aug 09 - Add SECS and make it same as NSECS
c  29 Apr 10 - Add 24 and 44 inch HP Z2100 as 2124 and 2144
c  22 Jul 10 - Add lat100 and long100 to ann and ann2
c  15 Oct 10 - Remove all the warnings about uneven vscale and sample interval
c  28 Oct 10 - HPATH needs binary header swapped on Intel
c  15 Oct 14 - The 11 May 06 change of nsecs preset from -1 to 0 ruined checks for big plots,
c              so change it from an error to a warning.
c            - Use si and delay from common
c
c
      PARAMETER (NPARS=61)                                              ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MULTI=59)                                              ! THE FIRST MULTIVALUED PARAMETER
      CHARACTER*7 NAMES(NPARS), label
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite
      COMMON /porder/ num, iorder(4)
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
     *   SECS,LRENUM,ISRCF,IDTYPE,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles, irewind, delay, segyrev, si
      COMMON /PLOT/MUNIT,NLISTS,FTAGG,TAGINK,FSPAC,NSPAC,SPACI,IDIR,
     &   irecsp, lunhead, nraslines
      INTEGER FTAGG,TAGINK,FSPAC,NSPAC,SPACI, plotter
      COMMON /SEISPL/ OFFSET,DEFLEC,PCTFI,VSCAL,TRCPIN,JTYPE,TLINE(4),
     *       STIMEL,TIMEL,IUNPLT,WIGGL,BIASS,LANN,ICDOT,scalr,clipp,
     *       itlan, irectify, ndptr, chart(2), size, itrim
      INTEGER  DIR,FNO,LNO,FDAY,LDAY,FTR,LTR,OPATH,ANNTYP,FSPACE,
     *    NSPACE,SPACEI,NIBS,TAGINC,FTAG,ICDOTS,DECIMF,ANNINC, fanno,
     *    frange, ptype, absval, hdr, wrap, recsp, anntyp2, dptr, trim
      REAL NSECS
      COMMON /VERSAT/ NIB,RNIBS
      COMMON / SUNRAS / lunras, iwrdcount, ilincount, irashead(8),
     *                  iptype
      COMMON /imaget/ maxx, maxy, lunimg
      COMMON /colors/ defs(9), colors(9), ncolors, bcolor, rgb, ngray
      INTEGER colors, bcolor, rgb
      COMMON /ver/ver
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      CHARACTER*80 ver
      CHARACTER*84 CARD
      CHARACTER*80 FILENM
      CHARACTER*80 CHEADR(40)
      EQUIVALENCE (ivms,card)
      EQUIVALENCE (ivms1,cheadr(1))
      CHARACTER*4 LMONTH(12)
      LOGICAL IEXIST
      DIMENSION BUF(100),LBUF(100),IBUF(100),ISCR(100),SCR(100),LSCR(10)
      INTEGER*2 IBUF,ISCR
      EQUIVALENCE (STIME,VALS(1)),
     2            (NSECS,VALS(2)),
     3            (VSCALE,VALS(3)),
     4            (DEF,VALS(4)),
     5            (TRPIN,VALS(5)),
     6            (RELAMP,VALS(6)),
     7            (DIR,LVALS(7)),
     8            (FNO,LVALS(8)),
     9            (LNO,LVALS(9)),
     *            (FDAY,LVALS(10)),
     1            (LDAY,LVALS(11)),
     2            (FTR,LVALS(12)),
     3            (LTR,LVALS(13)),
     4            (OPATH,LVALS(14)),
     5            (ANNTYP,LVALS(15)),
     6            (FSPACE,LVALS(16)),
     7            (NSPACE,LVALS(17)),
     8            (SPACEI,LVALS(18)),
     9            (NIBS,LVALS(19))
      EQUIVALENCE (TAGINC,LVALS(20)),
     1            (FTAG,LVALS(21)),
     2            (PCTFIL,VALS(22)),
     3            (WIGGLE,VALS(23)),
     4            (BIAS,VALS(24)),
     5            (ICDOTS,LVALS(25)),
     6            (LPRINT,LVALS(26)),
     7            (FANNO,LVALS(27)),
     8            (DECIMF,LVALS(28)),
     9            (ANNINC,LVALS(29)),
     *            (NINC,LVALS(30)),
     1            (hscale,vals(31)),
     2            (scalar,vals(32)),
     3            (clip,vals(33)),
     4            (plotsi,vals(34)),
     5            (ptype,lvals(35)),
     6            (frange,lvals(36)),
     7            (lrange,lvals(37)),
     8            (srpath,vals(38)),
     9            (sipath,vals(39))
      EQUIVALENCE (rstime,vals(40)),
     1            (absval,lvals(41)),
     2            (ann,lvals(42)),
     3            (hdr,lvals(43)),
     4            (ihdr,lvals(44)),
     5            (lhdr,lvals(45)),
     7            (tlann,lvals(47)),
     6            (wrap,lvals(48)),
     8            (recsp,lvals(49)),
     1            (anntyp2,lvals(51)),
     2            (ann2,lvals(52)),
     3            (rectify,vals(53)),
     4            (dptr,lvals(54)),
     5            (noinc,lvals(55)),
     6            (plotter,lvals(56)),
     7            (trim,lvals(57))
c    8            secs would be (58), but is nsecs is vals(2)
      DATA NAMES/'STIME ','NSECS ','VSCALE','DEF   ','TRPIN ','RELAMP',
     *  'DIR   ','FNO   ','LNO   ','FDAY  ','LDAY  ','FTR   ','LTR   ',
     *  'OPATH ','ANNTYP','FSPACE','NSPACE','SPACEI','NIBS  ','TAGINC',
     *  'FTAG  ','PCTFIL','WIGGLE','BIAS  ','ICDOTS','LPRINT','FANNO ',
     *  'DECIMF','ANNINC','NINC  ','HSCALE','SCALAR','CLIP  ','PLOTSI',
     *  'PTYPE ','FRANGE','LRANGE','SRPATH','SIPATH','RSTIME','ABSVAL',
     *  'ANN   ','HDR   ','IHDR  ','LHDR  ','BCOLOR','TLANN ','WRAP  ',
     *  'RECSP ','HPATH ','ANNTYP2','ANN2 ','RECTIFY','DPTR ','NOINC ',
     *  'PLOTTER','TRIM ','SECS',
     *  'TLINES','COLORS','CHART ' /
      DATA TYPE/6*'F','A',6*'L','A',7*'L',3*'F',5*'L','L',4*'F',3*'L',
     &    2*'A','F','L','A',3*'L','A','L','A','A','A','L','A','A','L',
     &   'L','L','A','F',
     &   'F','A','F'/
      DATA LENGTH/5,5,6,3,5,6,3*3,4,4,3,3,5,4*6,4,6,4,6,6,4,2*6,5,6,6,4,
     *     6,6,4,6,5,6*6,3,3,4,4,6,5,4,5,5,7,4,7,4,5,7,4,4,
     &     6,6,5/
      DATA LMONTH/'JAN.','FEB.','MAR.','APR.','MAY ','JUNE',
     *      'JULY','AUG.','SEPT','OCT.','NOV.','DEC.'/
      DATA NCOMMS/0/
C****
C****    SET THE PRESETS
C****
      nibs = 2859
      NLISTS=0
c      NS=0
      STIME=-99999.
      NSECS = 0
      VSCALE=0.
      DEF=.1
      TRPIN=20.
      RELAMP = -99999.
      IDIR=1
      FNO = -1
      LNO=999999999
      FDAY=0
      LDAY=0
      FTR=0
      LTR=99999
      ninc=1
      FILENM=' '
      ANNTYP=5
      FSPACE=0
      NSPACE=3
      SPACEI=-1
      TAGINC=10
      FTAG=1
      PCTFIL=100.
      WIGGLE=100.
      BIAS=0.
      ICDOTS=1
      LPRINT=0
      TLINE(1)=.1
      TLINE(2)=.5
      TLINE(3)=1.0
      TLINE(4)=0.
      nlines = 0
      FANNO=1
      ANNINC=0
      DECIMF=1
      hscale = 0.
      scalar = -1.
      clip = .5
      plotsi = 0.
      iplot = 0
      frange = 99999
      lrange = 99999
      ptype = 0
      iunplt = 0
      lunras = 0                                                        ! logical unit number of the SunRaster file
      lunimg = 0                                                        ! logical unit number of the ImageTool file
      rstime = -99999.
      absval = 0
      hdr = 0
      ihdr = 0
      lhdr = 0
      tlann = 1
      wrap = 0
      recsp = 0
      anntyp2 = 10
      irectify = 0
      nraslines = 0
      DO 10 i = 1, num
         IF( iorder(i) .EQ. 30 ) iplot = 1
   10 CONTINUE
      ncolors = 0
      colors(1) = 0
      bcolor = 7                                                        ! white
      rgb = 1
      ngray = 0
      dptr = 0
      nchart = 0
      chart(1) = 0.
      chart(2) = 0.
      itrim = 0
C****
C****   GET A DISC FILE TO HOLD THE PARAMETERS FOR THE EXECUTION PHASE
c****  PLOT uses FORTRAN I/O, not the usual SIOSEIS I/O
C****
      CALL GETFIL(2,MUNIT,token,ISTAT)
      IF( ICOMPT .EQ. 1) MUNIT = MUNIT + 12
      OPEN( unit = munit, FORM='UNFORMATTED', STATUS = 'SCRATCH' )
C****
C****  GET THE EBCDIC TAPE HEADER SQUARED AWAY
C****
      CALL PODISC(IUNHDR,1,0)
      CALL RDDISC(IUNHDR,ivms1,800,ISTAT)
      DO 50 I=1,40
         CALL EBCASC(CHEADR(I),80,CHEADR(I))                            ! CONVERT TO ASCII
         IF( cheadr(i)(1:1) .EQ. ' ' ) GOTO 50
         itemp = ICHAR(cheadr(i)(1:1))
         IF( itemp .GE. 65 .AND. itemp .LE. 90 ) GOTO 50                ! is it between A and Z
         IF( itemp .GE. 97 .AND. itemp .LE. 122 ) GOTO 50               ! is it between a and z
         cheadr(i) = ' '
   50 CONTINUE
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)
      IF( NCHARS .LE. 0 ) THEN
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
         GO TO 100
      ENDIF
  150 NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
         LEN=LENGTH(I)                                                  ! GET THE LEGAL PARAMETER NAME LENGTH
         IPARAM=I                                                       ! SAVE THE INDEX
         IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN)
     *       GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF(TOKEN(1:1).EQ.'C'.AND.NCHARS.EQ.3) GO TO 192                   ! IS IT A COMMENT CARD?
      IF( nlines .NE. 0 .OR. nchart .NE. 0 ) GO TO 230
      PRINT *,' ***  ERROR  *** PLOT does not have a parameter ',
     *     token(1:nchars)
      IERROR=IERROR+1
      GO TO 100
  192 CONTINUE                                                          ! PROCESS THE COMMENT CARDS HERE
      READ(TOKEN,'(1X,I2)') ITEMP
      WRITE(cheadr(itemp)(1:4), '(2Hc ,I2)') itemp
      CALL GETOKE(TOKEN,NCHARS)
      CHEADR(ITEMP) (5:80) = TOKEN(1:NCHARS)
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      nlines = 0
      NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN                                          ! END OF LINE?
          IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'                ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
          CALL RDLINE                                                   ! GET ANOTHER LINE
          NTOKES=0
          GO TO 210
      ENDIF
  230 IF(TYPE(NPARAM).NE.'A') GO TO 240
      IF( names(nparam) .EQ. 'DIR' ) THEN
          CALL upcase(token,nchars)
          IF( token(1:nchars) .EQ. 'LTR' ) idir = -1
          IF( token(1:nchars) .EQ. 'L2R' ) idir = -1
      ENDIF
      IF( names(nparam) .EQ. 'WRAP' ) THEN
          CALL upcase(token,nchars)
          IF( token(1:1) .EQ. 'Y' ) wrap = 1
          IF( token(1:1) .EQ. '1' ) wrap = 1
          IF( token(1:nchars) .EQ. 'ON' ) wrap = 1
      ENDIF
      IF( names(nparam) .EQ. 'RECSP' ) THEN
          CALL upcase(token,nchars)
          IF( token(1:1) .EQ. 'Y' ) recsp = 1
      ENDIF
      IF( names(nparam) .EQ. 'OPATH' ) THEN
          token(nchars+1:nchars+1) = CHAR(0)
          filenm = token
      ENDIF
      IF( names(nparam) .EQ. 'HPATH' ) THEN
          token(nchars+1:nchars+1) = CHAR(0)
          CALL getfil( 3, lunhead, token, istat )                       ! create the file
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  *** Could not open file', token
              ierror = ierror + 1
          ELSE
              CALL wrdiscb( lunhead, cheadr, 3200 )
              DO i = 1, 200
                 iscr(i) = ibinhdr(i)
              ENDDO
              iscr(11) = 0
              iscr(15) = 10
              IF( icompt .EQ. 2 .OR. icompt .EQ.4) CALL swap16(iscr,200)
              CALL wrdiscb( lunhead, iscr, 400 )
          ENDIF
      ENDIF
      IF( names(nparam) .EQ. 'SRPATH' .AND. iplot .EQ. 1 ) THEN
c****     We can assume Sun rasterfiles won't be created on non Sun
c****     computers or with oddball plotters like the Printronix & CItoh
          CALL getfil( 3, lunras, token, istat )                        ! create the file
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  *** Could not open file', token
              ierror = ierror + 1
          ENDIF
          DO 235 i = 1, 8
             irashead(i) = 0
  235     CONTINUE
          CALL wrdisc( lunras, irashead, 8 )
      ENDIF
      IF( names(nparam) .EQ. 'SIPATH' .AND. iplot .EQ. 1 ) THEN
          CALL getfil( 3, lunimg, token, istat )
          IF( istat .NE. 0 ) THEN
              PRINT *,' ***  ERROR  *** Could not open file', token
              ierror = ierror + 1
          ENDIF
          maxx = 0
          maxy = 0
      ENDIF
c**** don't upcase filenames!
      CALL upcase( token, nchars )
      IF( names(nparam) .EQ. 'ANN' ) THEN
          label = token(1:nchars)
          anntyp = 0
          IF( token(1:5) .EQ. 'FANNO' ) anntyp = 1
          IF( token(1:6) .EQ. 'SHOTNO' ) anntyp = 2
          IF( token(1:4) .EQ. 'RPNO' ) anntyp = 3
          IF( token(1:3) .EQ. 'GMT' .AND. nchars .EQ. 3 ) anntyp = 4
          IF( token(1:6) .EQ. 'GMTINT' ) anntyp = 5
          IF( token(1:5) .EQ. 'RANGE' ) anntyp = 6
          IF( token(1:6) .EQ. 'SHOTTR' ) anntyp = 7
          IF( token(1:4) .EQ. 'ESPN' ) anntyp = 8
          IF( token(1:5) .EQ. 'GMTRP' ) anntyp = 9
          IF( token(1:4) .EQ. 'NONE' ) anntyp = 10
          IF( token(1:6) .EQ. 'HEADER' ) anntyp = 11
          IF( token(1:4) .EQ. 'RPTR' ) anntyp = 12
          IF( token(1:5) .EQ. 'SH&TR' ) anntyp = 13
          IF( token(1:5) .EQ. 'RP&TR' ) anntyp = 14
          IF( token(1:6) .EQ. 'GMTSEC' ) anntyp = 15
          IF( token(1:3) .EQ. 'LAT' ) anntyp = 16
          IF( token(1:4) .EQ. 'LONG' ) anntyp = 17
          IF( token(1:6) .EQ. 'LAT100' ) anntyp = 18
          IF( token(1:7) .EQ. 'LONG100' ) anntyp = 19
          IF( token(1:9) .EQ. 'WBDEPTH@S' ) anntyp = 20
          IF( token(1:6) .EQ. 'WBTIME' ) anntyp = 21
          IF( token(1:4) .EQ. 'FOLD' ) anntyp = 22
          IF( anntyp .EQ. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Unknown ANN ',token(1:nchars)
              ierror = ierror + 1
          ENDIF
      ENDIF
      IF( names(nparam) .EQ. 'ANN2' ) THEN
          anntyp2 = 0
          IF( token(1:5) .EQ. 'FANNO' ) anntyp2 = 1
          IF( token(1:6) .EQ. 'SHOTNO' ) anntyp2 = 2
          IF( token(1:4) .EQ. 'RPNO' ) anntyp2 = 3
          IF( token(1:3) .EQ. 'GMT' .AND. nchars .EQ. 3 ) anntyp2 = 4
          IF( token(1:6) .EQ. 'GMTINT' ) anntyp2 = 5
          IF( token(1:5) .EQ. 'RANGE' ) anntyp2 = 6
          IF( token(1:6) .EQ. 'SHOTTR' ) anntyp2 = 7
          IF( token(1:4) .EQ. 'ESPN' ) anntyp2 = 8
          IF( token(1:5) .EQ. 'GMTRP' ) anntyp2 = 9
          IF( token(1:4) .EQ. 'NONE' ) anntyp2 = 10
          IF( token(1:6) .EQ. 'HEADER' ) anntyp2 = 11
          IF( token(1:4) .EQ. 'RPTR' ) anntyp2 = 12
          IF( token(1:5) .EQ. 'SH&TR' ) anntyp2 = 13
          IF( token(1:5) .EQ. 'RP&TR' ) anntyp2 = 14
          IF( token(1:6) .EQ. 'GMTSEC' ) anntyp2 = 15
          IF( token(1:3) .EQ. 'LAT' ) anntyp2 = 16
          IF( token(1:4) .EQ. 'LONG' ) anntyp2 = 17
          IF( token(1:6) .EQ. 'LAT100' ) anntyp2 = 18
          IF( token(1:7) .EQ. 'LONG100' ) anntyp2 = 19
          IF( token(1:9) .EQ. 'WBDEPTH@S' ) anntyp = 20
          IF( token(1:6) .EQ. 'WBTIME' ) anntyp = 21
          IF( token(1:4) .EQ. 'FOLD' ) anntyp = 22
          IF( anntyp2 .EQ. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Unknown ANN2 ',token(1:nchars)
              ierror = ierror + 1
          ENDIF
      ENDIF
      IF( names(nparam) .EQ. 'COLORS' ) THEN
          nlines = nlines + 1
          IF( nlines / 2 * 2 .NE. nlines .AND. token(1:4) .NE. 'GRAY'
     &        .AND. token(1:4) .NE. 'GREY' ) THEN
              CALL dcode( token, nchars, areal, istat )
              defs(nlines/2+1) = areal
          ELSE
              IF( token(1:5) .EQ. 'BLACK' ) colors(nlines/2) = 0
              IF( token(1:3) .EQ. 'RED' ) colors(nlines/2) = 1
              IF( token(1:5) .EQ. 'GREEN' ) colors(nlines/2) = 2
              IF( token(1:6) .EQ. 'YELLOW' ) colors(nlines/2) = 3
              IF( token(1:4) .EQ. 'BLUE' ) colors(nlines/2) = 4
              IF( token(1:7) .EQ. 'MAGENTA' ) colors(nlines/2) = 5
              IF( token(1:4) .EQ. 'CYAN' ) colors(nlines/2) = 6
              IF( token(1:5) .EQ. 'WHITE' ) colors(nlines/2) = 7
              IF( token(1:4) .EQ. 'GRAY' .OR. token(1:4).EQ.'GREY') THEN
c****             allow the gray level to be a color. Legal are 0-7.
                  IF( nchars .GT. 4 ) THEN
                      IF( nlines / 2 * 2 .NE. nlines ) THEN
                          PRINT *,' ***  ERROR  ***  Must be deflection
     &then grayscale.'
                          ierror = ierror + 1
                      ENDIF
                      CALL dcode( token(5:5), nchars-4, areal, istat )
                      colors(nlines/2) = 8 + NINT(areal)
                      IF( areal .LT. 0 .OR. areal .GT. 7 ) THEN
                         PRINT *,' ***  ERROR  ***  Illegal gray scale.'
                          PRINT *,' Gray scales must be 0-7.'
                          ierror = ierror + 1
                      ENDIF
                  ENDIF
                  ngray = ngray + 1
              ENDIF
              ncolors = ncolors + 1
          ENDIF
          GOTO 100
      ENDIF
      nlines = 0
      IF( names(nparam) .EQ. 'BCOLOR' ) THEN
          IF( token(1:5) .EQ. 'BLACK' ) bcolor = 0
          IF( token(1:3) .EQ. 'RED' ) bcolor = 1
          IF( token(1:5) .EQ. 'GREEN' ) bcolor = 2
          IF( token(1:6) .EQ. 'YELLOW' ) bcolor = 3
          IF( token(1:4) .EQ. 'BLUE' ) bcolor = 4
          IF( token(1:7) .EQ. 'MAGENTA' ) bcolor = 5
          IF( token(1:4) .EQ. 'CYAN' ) bcolor = 6
          IF( token(1:5) .EQ. 'WHITE' ) bcolor = 7
      ENDIF
      IF( names(nparam) .EQ. 'RECTIFY' ) THEN
          IF( token(1:3) .EQ. 'YES' ) irectify = 1
          IF( token(1:2) .EQ. 'NO' ) irectify = 0
          IF( token(1:1) .EQ. '1' ) irectify = 1
          IF( token(1:1) .EQ. '0' ) irectify = 0
          IF( token(1:2) .EQ. 'ON' ) irectify = 1
          IF( token(1:3) .EQ. 'OFF' ) irectify = 0
      ENDIF
      IF( names(nparam) .EQ. 'TRIM' ) THEN
          itrim = 1
          IF( token(1:3) .EQ. 'ALL' ) itrim = 1
          IF( token(1:3) .EQ. 'TOP' ) itrim = 2
          IF( token(1:6) .EQ. 'BOTTOM' ) itrim = 3
          IF( token(1:4) .EQ. 'LEFT' ) itrim = -1
          IF( token(1:5) .EQ. 'RIGHT' ) itrim = -2
      ENDIF
      GO TO 100
  240 CONTINUE
      CALL UPCASE(TOKEN,NCHARS)
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          ! =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                   ! DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPE(NPARAM).EQ.'L') GO TO 500
      IF(NPARAM.LT.MULTI) GO TO 490
      IF( names(nparam) .EQ. 'TLINES' ) THEN
          nlines = nlines + 1
          IF( nlines .EQ. 1 ) THEN                                      ! zero the tlines array if it is given
              DO i = 2, 4
                 tline(i) = 0.
              ENDDO
          ENDIF
          tline(nlines) = areal
          GOTO 100
      ENDIF
      IF( names(nparam) .EQ. 'CHART' ) THEN
          nchart = nchart + 1
          chart(nchart) = areal / 100.
          GOTO 100
      ENDIF
  490 IF( names(nparam) .EQ. 'SECS' ) THEN
          nsecs = areal
      ELSE
          VALS(NPARAM)=AREAL                                            !  FLOATING POINT VALUES
      ENDIF
      GO TO 100
  500 CONTINUE                                                          !  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      IF( nparam .EQ. 56 ) nibs = plotter
      GO TO 100
c****
c****    Do some additional preseting for different plotters!
c****
 1000 CONTINUE
      IF( ncolors .GT. 0 .AND. lunras .GT. 0 .AND. colors(1) .NE. 8)THEN
          PRINT *,' ***  WARNING  ***  SRPATH cannot support color.  ',
     &     'Use OPATH and program SIO2SUN for color Sun rasterfiles.'
          iwarn = iwarn + 1
      ENDIF
c**** Set up gray scale.
c**** (Watch out for 1 gray scale given!  e.g. colors 0 gray7)
      IF( ngray .GT. 0 ) THEN
          IF( ncolors .GT. 1 .OR. colors(1) .NE. 0 ) THEN
              ngray = ncolors
              DO i = 1, 8
                 colors(i) = 15 - colors(i)
              ENDDO
          ELSE
              ngray = 8
              ncolors = 8
c	0	255 255 255         gray7	white
c	1	219 219 219         gray6
c	2	181 181 181         gray5
c	3	145 145 145         gray4
c	4	110 110 110         gray3
c	5	 71  71  71         gray2
c	6	 36  36  36         grey1
c	7	  0   0   0         gray0	black
c****     for sioseis plots, the bigger the number, the darker the plot
              DO i = 1, 8
                 colors(i) = 8 - i
                 defs(i) = (def/8) * 2. * FLOAT(i-1)
              ENDDO
           ENDIF
      ENDIF
c      IF(NIBS.NE.60) GO TO 1100                                         ! IS IT A PRINTRONIX 300
c      rnibs = 60.
c      OFFSET=.6                                                         ! NO ANNOTATION IS POSSIBLE
c      SIZE=13.5                                                         ! IT IS COMPUTER PAPER WIDE
c      IF(VSCALE.EQ.0.) VSCALE=4.17
c      DO 1010 I=1,20
c      IF(ABS(VSCALE-FLOAT(I)*2.085).LT..0001) GO TO 1030
c 1010 CONTINUE
c      PRINT 1020
c 1020 FORMAT(' ***  WARNING  ***  VSCALE SHOULD BE A MULTIPLE OF ',
c     *   '2.085 WHEN PLOTTING TIME DATA ON THE PRINTRONIX 300.')
c      IWARN=IWARN+1
c 1030 IF(NSECS.EQ.-1) NSECS=3.
c      GO TO 2000
 1100 IF( nibs .EQ. 160 ) THEN
          rnibs = 160.
          size = 18.56
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 3.125
          IF( nsecs .LE. 0 ) nsecs = 5.0
          GOTO 2000
      ENDIF
      IF( NIBS .EQ. 200 ) THEN                                          ! IS IT A VERSATEC MODEL 1200?
          rnibs = 200.
          OFFSET=1.
          SIZE=10.56
          IF(VSCALE.EQ.0.) VSCALE=2.5
          IF(NSECS .LE. 0 ) NSECS=3.8
          GOTO 2000
      ENDIF
      IF(NIBS.NE.100) GO TO 1400
      rnibs = 100.
      OFFSET=.8
      SIZE=13.5
      IF(VSCALE.EQ.0) VSCALE=2.5
      IF(NSECS .LE. 0 ) NSECS=5.
      GO TO 2000
 1400 IF(NIBS.NE.80) GO TO 1500
      rnibs = 80.
      OFFSET=.3
      SIZE=14.0
      IF(VSCALE.EQ.0) VSCALE=3.125
      IF(NSECS .LE. 0 ) NSECS=4.
      GO TO 2000
 1500 CONTINUE
      IF( nibs .EQ. 201 ) THEN
          rnibs = 200.
          size = 21.5
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 2400 ) THEN
          rnibs = 200.
          size = 23.52
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 5732 ) THEN
          rnibs = 200.
          size = 23.5
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 5845 ) THEN
          rnibs = 400.
          size = 43.03
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 5.
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 4160 ) THEN
          rnibs = 160.
          size = 13.7
          offset = 1.1
          IF( vscale .EQ. 0 ) vscale = 3.125
          IF( nsecs .LE. 0 ) nsecs = 4.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 8122 ) THEN
          rnibs = 100.
          size = 21.12
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 7222 .OR. nibs .EQ. 8222 .OR. nibs .EQ. 7422 ) THEN
          rnibs = 200.
          IF( nibs .EQ. 7422 ) rnibs = 400.
          size = 21.12
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 7224 .OR. nibs .EQ. 7225 .OR. nibs .EQ. 8224 .OR.
     &    nibs .EQ. 7424 .OR. nibs .EQ. 7425 .OR. nibs .EQ. 8424 .OR.
     &    nibs .EQ. 8425 .OR. nibs .EQ. 8625 ) THEN
          IF( nibs .EQ. 7224 .OR. nibs .EQ. 7225 .OR. nibs .EQ.8224)THEN
              rnibs = 200.
          ELSE
              rnibs = 400.
          ENDIF
c****  Interesting problem on the Versatec 7000 series:
c****  The specs list the 7224 as 23.52, but that is WRONG
c****  The 7224 is 23.04, the 7225 is 23.52
          size = 23.04
          IF( nibs .EQ. 7225 ) size = 23.52
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 7436 ) THEN
          rnibs = 400.
          size = 35.2
          offset = .5
          IF( vscale .EQ. 0 ) vscale = 5
          IF( nsecs .LE. 0 ) nsecs = 6.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 2124 .OR. nibs .EQ. 2144 ) THEN
c****  nsecs is now preset to 0
c****  with 600dpi, don't worry about samples being on rasters exactly.
          rnibs = 600.
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 2.5
          size = 43.36  !  43.36 = 26016 dots = 1626 words
          IF( nibs .EQ. 2124 ) size = 24
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 7444 .OR. nibs .EQ. 3444 .OR. nibs .EQ. 3436
     &  .OR. nibs .EQ. 8936 ) THEN
          IF( nibs .EQ. 3444 .OR. nibs .EQ. 3436 .OR. nibs .EQ. 8936 )
     &         rgb = 0                                                  ! It's CYM, not RGB
          rnibs = 400.
          size = 43.04
          IF( nibs .EQ. 3436 .OR. nibs .EQ. 8936 ) size = 34.2
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 5.0
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 120 ) THEN
          rnibs = 120.
          size = 13.7
          offset = 1.1
          IF( vscale .EQ. 0 ) vscale = 2.0833
          IF( nsecs .LE. 0 ) nsecs = 6.0
          IF( plotsi .GT. 0. ) GOTO 2000
          rinc1 = 125. / 120
          rinc2 = 128. / 120
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 8242 ) THEN
          rnibs = 200.
          size = 40.96
          offset = .9
          IF( vscale .EQ. 0 ) vscale = 5.0
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 9242 ) THEN
          rnibs = 200.
          size = 40.
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 5.0
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 850 .OR. nibs .EQ. 9800 .OR. nibs .EQ. 9315 .OR.
     &    nibs .EQ. 2368 ) THEN
c****   850 has 1728 pixels, 9800 has 4096, 9315 has 2048
          rnibs = 203.
          offset = .8
          IF( nibs .EQ. 850 ) THEN
              size = 8.5
              IF( nsecs .LE. 0 ) nsecs = 3.0
              IF( vscale .EQ. 0 ) vscale = 250. / rnibs
          ENDIF
          IF( nibs .EQ. 2368 ) THEN
              size = 11.654
              IF( nsecs .LE. 0 ) nsecs = 4.0
              IF( vscale .EQ. 0 ) vscale = 500. / rnibs
          ENDIF
          IF( nibs .EQ. 9800 ) THEN
              size = 20.17773
              IF( nsecs .LE. 0) nsecs = 7.0
              IF( vscale .EQ. 0 ) vscale = 500. / rnibs
          ENDIF
          IF( nibs .EQ. 9315 ) THEN
              size = 10.08867
              IF( nsecs .LE. 0 ) nsecs = 6.0
              IF( vscale .EQ. 0 ) vscale = 250. / rnibs
          ENDIF
          IF( plotsi .GT. 0. ) GOTO 2000
          DO 1990 i = 1, 200
             IF( ABS(vscale - FLOAT(i)*125./rnibs) .LT. .01 ) THEN
                 vscale = FLOAT(i)*125./rnibs
                 GOTO 2000
             ENDIF
             IF( ABS(vscale - FLOAT(i)*128./rnibs) .LT. .01 ) THEN
                 vscale = FLOAT(i)*128./rnibs
                 GOTO 2000
             ENDIF
 1990     CONTINUE
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 7600 ) THEN
          rnibs = 406.
          size = 14336 / 406.
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 1000. / 406.
          IF( nsecs .LE. 0 ) nsecs = 10.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 75 ) THEN
          rnibs = 75.
          size = 20.
          offset = 1.2
          IF( vscale .EQ. 0 ) vscale = 3.33333
          IF( nsecs .LE. 0 ) nsecs = 5
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 624 ) THEN
          rnibs = 400.
          size = 9472. / rnibs
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 ) nsecs = 8.0
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 2847 .OR. nibs .EQ. 2848 .OR.
     &    nibs .EQ. 2858 .OR. nibs .EQ. 2859 .OR. nibs .EQ. 300 ) THEN
          rnibs = 300.
          IF( nibs .EQ. 2847 .OR. nibs .EQ. 2858 ) THEN
              size = 860. * 8. / rnibs                                  ! 22.933333 in.
          ELSEIF( nibs .EQ. 300 ) THEN
              size = 8.5
          ELSE
              size = 1312. * 8. / rnibs                                 ! 34.9866666 in
          ENDIF
          offset = 1.0
          IF( vscale .EQ. 0 ) vscale = 2.5
          IF( nsecs .LE. 0 .AND. nibs .EQ. 2847 ) nsecs = 8.
          IF( nsecs .LE. 0 .AND. nibs .EQ. 2858 ) nsecs = 8.
          IF( nsecs .LE. 0 .AND. nibs .EQ. 2848 ) nsecs = 10.0
          IF( nsecs .LE. 0 .AND. nibs .EQ. 2859 ) nsecs = 10.0
          IF( nsecs .LE. 0 .AND. nibs .EQ. 300 ) nsecs = 3.
          GOTO 2000
      ENDIF
      IF( nibs .EQ. 128 ) THEN
          rnibs = 128.
          size = 10.
          offset = 1.
          IF( vscale .EQ. 0 ) vscale = 1.28
          IF( nsecs .LE. 0 ) nsecs = 4.0
          GOTO 2000
      ENDIF
      PRINT *,' ***  ERROR  ***  INCORRECT TYPE OF PLOTTER REQUESTED.'
      IERROR=IERROR+1
C****
C****     NOW DO THE ERROR CHECKS
C****
 2000 CONTINUE
      IF( STIME .NE. -99999. ) THEN                                     ! IF IT WASN'T GIVEN IT CAN'T BE WRONG!
          IF( STIME .LT. -10. .OR. STIME .GT. 100. ) THEN
              PRINT *, ' ***  WARNING  ***  STIME might be bad.'
              iwarn = iwarn + 1
          ENDIF
      ENDIF
      IF(NSECS.GT.-45..AND.NSECS.LT.45.) GO TO 2040
      PRINT 2030,NSECS
 2030 FORMAT(' ***  ERROR  ***  ILLEGAL NSECS VALUE OF ',F10.4)
      IERROR=IERROR+1
 2040 IF(DEF.GT.0..AND.DEF.LT.3.) GO TO 2060
      PRINT 2050,DEF
 2050 FORMAT(' ***  ERROR  ***  ILLEGAL DEF VALUE OF ',F10.4)
      IERROR=IERROR+1
 2060 IF(TRPIN.GT.0..AND.TRPIN.LE.NIBS) GO TO 2100
      PRINT 2070,TRPIN
 2070 FORMAT(' ***  ERROR  ***  ILLEGAL TRPIN VALUE OF ',F10.4)
      IERROR=IERROR+1
 2100 IF(FNO.GE.-1)GO TO 2120
      PRINT 2110
 2110 FORMAT(' ***  ERROR  *** FNO MUST BE A POSITIVE NUMBER.')
      IERROR=IERROR+1
 2120 IF( LNO .LT. FNO ) THEN
          PRINT *,' ***  ERROR  ***  LNO MUST BE LARGER THAN FNO.'
          IERROR=IERROR+1
      ENDIF
 2140 IF(FDAY.GE.0.AND.FDAY.LE.366) GO TO 2160
      PRINT 2150,FDAY
 2150 FORMAT(' ***  ERROR  ***  ILLEGAL FDAY (JULIAN DAY) OF ',I5)
      IERROR=IERROR+1
 2160 IF(LDAY.GE.FDAY.AND.LDAY.LE.366) GO TO 2180
      PRINT 2170,LDAY
 2170 FORMAT(' ***  ERROR  ***  LDAY MUST BE A JULIAN DAY AND MUST',
     *    ' NOT BE BEFORE FDAY.')
      IERROR=IERROR+1
 2180 IF(FTR.GE.0.) GO TO 2200                                          ! ALLOW A TRACE NUMBER OF ZERO
      PRINT 2190,FTR
 2190 FORMAT(' ***  ERROR  ***  ILLEGAL FTR VALUE OF ',I6)
      IERROR=IERROR+1
 2200 IF(LTR.GE.FTR.OR.LTR.EQ.-1) GO TO 2220
      PRINT 2210,LTR
 2210 FORMAT(' ***  ERROR  ***  ILLEGAL LTR VALUE OF ',I6)
      IERROR=IERROR+1
 2220 IF( ANNTYP .LE. 0 .OR. ANNTYP .GT. 19) THEN
          PRINT *,' ***  ERROR  ***  ILLEGAL ANNTYP VALUE OF ',anntyp
          IERROR=IERROR+1
      ENDIF
      IF( ANNTYP2 .LT. 0 .OR. ANNTYP2 .GT. 19) THEN
          PRINT *,' ***  ERROR  ***  ILLEGAL ANNTYP2 VALUE OF ',anntyp2
          IERROR=IERROR+1
      ENDIF
      IF( (anntyp .GE. 16 .AND. anntyp .LE. 19) ) THEN
          IF( ibuf(45) .NE. 2 ) THEN
          PRINT *,' ***  WARNING  ***  SEGY lat/long must be arcsec'
          iwarn = iwarn + 1
          anntyp = 10
          ENDIF
      ENDIF
      IF( (anntyp2 .GE. 16 .AND. anntyp2 .LE. 19) .AND.
     &     ibuf(45) .NE. 2 ) THEN
          PRINT *,' ***  WARNING  ***  SEGY lat/long must be arcsec'
          iwarn = iwarn + 1
          anntyp2 = 10
      ENDIF
      IF(FSPACE.GE.0) GO TO 2260
      PRINT 2250,FSPACE
 2250 FORMAT(' ***  ERROR  ***  ILLEGAL FSPACE VALUE OF ',I5)
      IERROR=IERROR+1
 2260 IF(NSPACE.GE.0.AND.NSPACE.LE.500) GO TO 2280
      PRINT 2270,NSPACE
 2270 FORMAT(' ***  ERROR  ***  ILLEGAL NSPACE VALUE OF ',I5)
      IERROR=IERROR+1
 2280 IF(SPACEI.GE.0.OR.SPACEI.EQ.-1) GO TO 2300
      PRINT 2290,SPACEI
 2290 FORMAT(' ***  ERROR  ***  ILLEGAL SPACEI VALUE OF ',I5)
      IERROR=IERROR+1
 2300 IF(TAGINC.GE.0.OR.TAGINC.EQ.-1) GO TO 2320
      PRINT 2310,TAGINC
 2310 FORMAT(' ***  ERROR  ***  ILLEGAL TAGINC VALUE OF ',I5)
      IERROR=IERROR+1
 2320 IF(FTAG.GT.0.OR.FTAG.EQ.-1) GO TO 2340
      PRINT 2330,FTAG
 2330 FORMAT(' ***  ERROR  ***  ILLEGAL FTAG VALUE OF ',I5)
      IERROR=IERROR+1
 2340 CONTINUE
c**** OFFSET must be >0 so indices do go negative in trplot
c      IF( itrim .GE. 1 .AND. itrim .LE. 3 ) offset = 0
      IF( NSECS*VSCALE .GT. SIZE-OFFSET ) THEN
          PRINT *,' ***  WARNING  ***   The plot is bigger than the ',
     &    'plotter.  Reduce NSECS or VSCALE.'
          iwarn = iwarn + 1
      ENDIF
      IF( anntyp .EQ. 5 .AND. anninc .EQ. 0 ) anninc = 5
      IF( anninc .EQ. 0 ) anninc = 1
      IF( hscale .LT. 0 ) THEN
          PRINT *,' ***  ERROR  ***  HSCALE must be positive.'
          ierror = ierror + 1
      ENDIF
      IF( relamp .GT. -99990.0 ) scalar = -99999.
          IF( clip .LT. 0 .OR. clip .GT. 1. ) THEN
          PRINT *,' ***  ERROR  ***  CLIP must be between 0 and 1.'
          ierror = ierror + 1
      ENDIF
      IF( frange .NE. 99999 .AND. lrange .EQ. 99999 ) THEN
          PRINT *,' ***  ERROR  ***  LRANGE must be given when frange ',
     &          'is given.'
          ierror = ierror + 1
      ENDIF
      IF( filenm .EQ. ' ' .AND. lunras+lunimg .EQ. 0) filenm = 'pltfil'
      IF( rstime .LT. -100. .AND. rstime .NE. -99999. ) THEN
          PRINT *,' ***  ERROR  ***  Bad RSTIME of ', rstime
          ierror = ierror + 1
      ENDIF
      IF( stime .NE. -99999. .AND. rstime .NE. -99999. ) THEN
          PRINT *,' ***  ERROR  ***  STIME and RSTIME are both given.'
          ierror = ierror + 1
      ENDIF
      IF( anntyp .EQ. 11 .AND. hdr+ihdr+lhdr .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Annotation by SEGY header requires
     &HDR or LHDR or IHDR to be given.'
          ierror = ierror + 1
      ENDIF
      DO i = 2, 4
         IF( tline(i) .NE. 0. .AND. tline(i) .LE. tline(i-1) ) THEN
             PRINT *,' ***  ERROR  ***   TLINES must increase.'
             ierror = ierror + 1
         ENDIF
      ENDDO
      IF( nchart .NE. 0 .AND. nchart .NE. 2 ) THEN
          PRINT *,' ***  ERROR  ***   CHART must have two values.'
          ierror = ierror + 1
      ENDIF
      IF( nchart .NE. 0 ) THEN
          IF( chart(1) .LT. 0 .OR. chart(1) .GT. .20 .OR.
     &        chart(2) .LT. .4 .OR. chart(2) .GT. 1 ) THEN
              PRINT *,' ***  WARNING  ***  Unusual CHART values.'
              iwarn = iwarn + 1
          ENDIF
      ENDIF
c****
c****      Create the plot header - from the SEGY EBCDIC header
c****
      IF(NLISTS.NE.0.or.irun.eq.0.or.ierror.ne.0
     *    .OR.iplot.EQ.0) GO TO 4000                                    ! IS THIS THE FIRST TIME?
      IFTN=0
      IF( icompt .EQ. 1 .AND. nibs .EQ. 60 ) iftn = 1
      IF(ICOMPT.EQ.1 .OR. nibs .EQ. 60 ) THEN
            CALL GETFIL(2,IUNPLT,token,ISTAT)
            IF(icompt .EQ. 1 ) ITEMP=IUNPLT+12                          ! REMEMBER THAT FORTRAN UNITS = PRIMOS+4
            INQUIRE(FILE=FILENM,EXIST=IEXIST)
            IF(IEXIST) THEN
                OPEN(UNIT=ITEMP,FILE=FILENM,STATUS='OLD')
                CLOSE(UNIT=ITEMP,STATUS='DELETE')
            ENDIF
            OPEN(UNIT=ITEMP,FILE=FILENM,STATUS='NEW',
     *            ACCESS='SEQUENTIAL',FORM='FORMATTED',
     *            IOSTAT=ISTAT)
            IF( NIBS.EQ.60) IFTN=1
            IF( icompt .EQ. 1 .AND. nibs .EQ. 60 ) IUNPLT=IUNPLT+12
      ELSE
            iactiv = 3                                                  ! a plot file or delayed plot
            IF(FILENM(1:5).EQ.'/DEV/'.or.filenm(1:5).eq.'/dev/')
     *             iactiv = 4                                           !  an interactive plot
            IF( filenm .NE. ' ' ) THEN
                CALL GETFIL(iactiv,IUNPLT,FILENM,ISTAT)                 ! GET A FILE NUMBER AND CREATE A NEW FILE
                IF( istat .NE. 0 ) THEN
                  PRINT *,' ***  ERROR  *** Could not open file', filenm
                  STOP
                ENDIF
            ENDIF
      ENDIF
      IF(NIBS.EQ.80.AND. iunplt .NE. 0 ) THEN                           ! THE C.ITOH SHOULD BE IN PICA MODE WITH 1/6 INCH LINE FEED
            CARD(1:8)=CHAR(27)//'N'//CHAR(27)//']'//CHAR(27)//
     *         'A'//CHAR(0)//CHAR(0)
c     *         'A'//CHAR(12)//CHAR(0)
          CALL WRDISC(IUNPLT,ivms,2)
      ENDIF
      IF( nibs .EQ. 120 .AND. iunplt .NE. 0 ) THEN
c         make sure the Epson is set to Pica
          card(1:4) = CHAR(27) // '2  '
          CALL wrdisc( iunplt, citoh, 1 )
      ENDIF
      card = ver
      IF( icompt .NE. 4 ) THEN                                          ! all but VMS
          CALL getdate1( token )
          card(37:60) = token(1:24)
      ENDIF
      IF( ngray .GT. 0 ) WRITE( card(65:65), '(1Hg)' )                 ! signal gray scale
      WRITE( card(67:70), '(I4)') nibs
      card(71:80) = '   0     0)'
      IF( iunplt .NE. 0 ) THEN
          IF( NIBS .EQ. 60 ) THEN                                      ! THE PRINTRONIX ON THE PRIME IS A SPOOL DEVICE
              WRITE(IUNPLT,'(1X,A80)') CARD(1:80)
          ELSE
              CARD(80:80)=CHAR(10)
              IF( icompt .EQ. 4 ) THEN
                  CALL WRDISC(IUNPLT,ivms,20)
              ELSE
                  CALL wrdiscb( iunplt, card, 80 )
              ENDIF
          ENDIF
      ENDIF
c****
c****  Write the SEGY EBCDIC header to the plot header.
c****  Drop the last line and put the plot parameters there instead.
c****
      DO 3020 I=1, 40
          CARD(1:80)=CHEADR(I) (1:80)
          IF(IFTN.EQ.0) THEN                                            ! IS IT A FORTRAN WRITE? (I.E. THE PRINTRONIX OR VERSATEC)
             IF( NIBS .EQ. 80 .OR. nibs .EQ. 120 ) THEN                 ! IF IT'S THE C.ITOH THEN
                 CARD(81:84) = '  ' // CHAR(13) // CHAR(10)
                 IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,21)
             ELSE
c                CARD(80:80)=CHAR(15)                                   ! IT MUST BE A VERSATEC
                IF( nibs .EQ. 4160 .OR. nibs .EQ. 100 )
     *                   card(80:80) = CHAR(10)                         !  Printronix
                IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,20)
             ENDIF
          ELSE
             IF( iunplt .NE. 0 )WRITE(IUNPLT,'(1X,A80)') CARD(1:80)
          ENDIF
 3020 CONTINUE
C****  NOW WRITE 3 LINES OF PLOT INFO
      CARD(1:80)=' '                                                    ! JUST BLANK IT OUT
      WRITE (card, 3030 ) vscale, trpin, def, scalar, hscale
 3030 FORMAT(' vscale= ',F6.2,' trpin = ',F4.0,' def = ',
     *    F4.2,' scalar = ',G12.5,' hscale = ',F6.1)
      IF(IFTN.EQ.0) THEN
            IF( NIBS .EQ. 80 .OR. nibs .EQ. 120 ) THEN
                CARD(81:84)='  '//CHAR(13)//CHAR(10)
                IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,21)
            ELSE
c                CARD(80:80)=CHAR(15)
                IF( nibs .EQ. 4160 .OR. nibs .EQ. 100 )
     *                   card(80:80) = CHAR(10)
                IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,20)
            ENDIF
      ELSE
            IF( iunplt .NE. 0 ) WRITE(IUNPLT,'(1X,A80)') CARD(1:80)
      ENDIF
      CARD(1:80)=' '
      IF(IBUF(58).EQ.0) IBUF(58)=NUMDAT                                 ! PUT THE NUMBER OF SAMPLES PER TARCE INTO THE TRACE HEADER
      temp = buf(49)
      IF( plotsi .GT. 0 ) temp = plotsi
      SINEW = temp * FLOAT(DECIMF)                                      ! FIND THE NEW SAMPLE INTERVAL
      WRITE(CARD,3060) ibuf(58), delay ,SINEW
 3060 FORMAT(I6,' samples with a delay of ',F5.3,
     &   ' secs. Sample interval = ',F7.6,' secs.')
      IF(IFTN.EQ.0) THEN
            IF( NIBS .EQ. 80 .OR. nibs .EQ. 120 ) THEN
                  CARD(81:84)='  '//CHAR(13)//CHAR(10)
                  IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,21)
            ELSE
c                  CARD(80:80)=CHAR(15)
                  IF( nibs .EQ. 4160 .OR. nibs .EQ. 100 )
     *                   card(80:80) = CHAR(10)                         !  Printronix
                  IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,20)
            ENDIF
      ELSE
            IF( iunplt .NE. 0 ) WRITE(IUNPLT,'(1X,A80)') CARD(1:80)
      ENDIF
      ITEMP=IBUF(79)                                                    ! CONVERT TO INTEGER*4
      ITEMP1=IBUF(80)                                                   ! CONVERT TO INTEGER*4
      CALL JULCAL(MONTH,IDAY,ITEMP,ITEMP1)                              ! CONVERT FROM JULIAN TO CALENDAR
      CARD(1:80)=' '
      temp = stime
      IF( stime .EQ. -99999. ) temp = delay
      temp1 = temp + nsecs
      WRITE( card, 3070 ) temp, temp1, label,
     &     ibuf(79), LMONTH(MONTH), iday, ibuf(81), IBUF(82)
 3070 FORMAT(' Plot times: ',F7.4,' to ',F7.4,' Label ',A6,
     &  ' Header date ',I4,1X,A4,1X,I2,1X,I2,':',i2)
      IF(IFTN.EQ.0) THEN
          IF( NIBS .EQ. 80 .OR. nibs .EQ. 120 ) THEN
                 CARD(81:84)='  '//CHAR(13)//CHAR(10)
                 IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,21)
          ELSE                                                          ! it must be a Versatec
c              CARD(80:80)=CHAR(15)
              IF( nibs .EQ. 4160 .OR. nibs .EQ. 100 )
     *                   card(80:80) = CHAR(10)                         !  Printronix
              IF( iunplt .NE. 0 ) CALL WRDISC(IUNPLT,ivms,20)
          ENDIF
      ELSE
          IF( iunplt .NE. 0 ) WRITE(IUNPLT,'(1X,A80)') CARD(1:80)
      ENDIF
      IF( nibs .EQ. 4160 ) THEN
          card = ' '
          card(4:4) = CHAR(10)
          DO 3080 i = 1, 5                                              ! put some distance between the header and the plot
             CALL wrdisc( iunplt, card, 1 )
 3080     CONTINUE
      ENDIF
C****
C****
 4000 CONTINUE
      NIB=NIBS
      DEFLEC=DEF
      PCTFI=PCTFIL
      IF( IDIR .EQ. -1 ) THEN                                           ! IF DIR is LTR
          PCTFI=-PCTFIL                                                 ! FLIP THE SIDE OF THE PULSE TO SHADE
          IF( ncolors .NE. 0 .AND. colors(1) .NE. 8 ) THEN
              DO i = 1, ncolors
                 scr(i) = defs(i)
                 lscr(ncolors+i) = colors(i)
              ENDDO
              DO i = 1, ncolors
                 defs(i) = -scr(ncolors+1-i)
                 colors(i) = lscr(ncolors+ncolors+1-i)
              ENDDO
           ENDIF
      ENDIF
c**** There's a weird color problem when a def is 0, so kludge around it
      IF( ncolors .NE. 0 ) THEN
          itemp = 0
          DO i = 1, ncolors
             IF( defs(i) .EQ. 0. ) itemp = i
          ENDDO
          IF( itemp .NE. 0 ) THEN
              DO i = 1, ncolors
                 scr(i) = defs(i)
                 lscr(ncolors+i) = colors(i)
              ENDDO
              DO i = 1, itemp-1
                 defs(i) = scr(i)
                 colors(i) = lscr(ncolors+i)
              ENDDO
              defs(itemp) = -.0001
              colors(itemp) = lscr(ncolors+itemp)
              DO i = itemp, ncolors
                 defs(i+1) = scr(i)
                 colors(i+1) = lscr(ncolors+i)
              ENDDO
              ncolors = ncolors + 1
          ENDIF
      ENDIF
      JTYPE=2
      WIGGL=WIGGLE
      IF( idir .EQ. -1 ) wiggl = -wiggle
      BIASS=BIAS
      ICDOT=ICDOTS
      FTAGG=FTAG
      IF( anntyp .EQ. 5 ) taginc = 5
      TAGINK=TAGINC
      FSPAC=FSPACE
      NSPAC=NSPACE
      SPACI=SPACEI
      scalr = scalar
      clipp = clip
      itlan = tlann
      irecsp = recsp
      JTYPE=2
      IF(FDAY.NE.0) FNO=FDAY*10000+FNO                                  ! MAKE THE JULIAN DAY AND THE HOUR THE FNO
      IF(FDAY.NE.0) LNO=LDAY*10000+LNO
      WRITE(MUNIT) FNO,FDAY,FTR,LNO,LDAY,LTR,STIME,NSECS,VSCALE,DEF,
     *   TRPIN,RELAMP,ANNTYP,DECIMF,ICFILE,NCOMMS,ANNINC,FANNO,ninc,
     *   hscale, plotsi, ptype, frange, lrange, rstime, absval, hdr,
     *   ihdr, lhdr, wrap, anntyp2, dptr
      IF( IAND(LPRINT,1) .NE. 0 ) THEN
          PRINT *,FNO,FDAY,FTR,LNO,LDAY,LTR,STIME,NSECS,VSCALE,DEF
          PRINT *,TRPIN,RELAMP,ANNTYP,ANNTYP2,DECIMF,ANNINC,FANNO
          PRINT *, NIB,DEF,PCTFI,WIGGL,BIASS,ICDOT,FTAGG,TAGINK,
     *        FSPAC,NSPAC,SPACI,IDIR,ninc,hscale,scalr,clipp
          PRINT *, plotsi, ptype, frange, lrange, rstime, absval, hdr,
     *         ihdr, lhdr, wrap
          PRINT *,' defs=',defs
          PRINT *,' colors=', colors
          PRINT *, ncolors, bcolor, wrap, ngray, dptr, itrim
      ENDIF
      NLISTS=NLISTS+1
c      NS=0
 4030 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 4040                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT *,' <  ENTER PARAMETERS  >'
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 4030
 4040 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
C****
C****
C****
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
