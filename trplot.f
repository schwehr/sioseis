      SUBROUTINE TRPLOT(TRACE,SR,NSAMPSIN,ISIG,JDENS,ITAG)
C****
C      A SUBROUTINE TO PLOT A SEISMIC VARIABLE AREA WIGGLE TRACE PLOT ON A
C VERSATEC PLOTTER FROM A REAL TRACE ALREADY IN MEMORY.  THIS IS FOR THE VERSTEC
C OR ANY RASTER PLOTTER.  THIS IS WRITTEN FOR THE PRIME COMPUTER AND USES
C SEVERAL MACHINE DEPENDANT ITEMS (SUCH AS SUBROUTINE T$VG AND THE FACT THAT
C PRIME FORTRAN WRITES ARRAYS THE WAY IT DOES)
C
C  VARIABLES NEEDED:
C         TRACE  - AN ARRAY OF TYPE REAL DATA TO BE PLOTTED.  TRACE MUST BE
C                  NSAMPS*JDENS (JDENS<>0).
C         SR     - SAMPLE INTERVAL OF THE DATA IN SECONDS.
C         NSAMPS - NUMBER OF SAMPLES TO PLOT.  THE FIRST POINT PLOTTED IS AT
C                  TRACE(1) AND THE LAST IS AT TRACE(NSAMPS).
C         ISIG   - LAST CALL SWITCH.
C                =0, THE CALL IS NOT THE LAST CALL, PLOT THE TRACE
C                =1, FLUSH ALL THE REST OF THE BUFFER
C         JDENS  - THE DENSITY OF THE PLOT - THE NUMBER OF DOTS TO PLOT BETWEEN
C                  DATA SAMPLES. NORMALLY THIS IS 0 SO THE PROGRAM CALCULATES
C                  IT.  THE PROGRAM WILL CALCULATE VALUES FOR EVERY NIB
C                  UNLESS THIS PARAMETER IS SET NON ZERO.
C         ITAG   - A SIGNAL DESIGNATING THE THE CURRENT TRACE SHOULD BE TAGGED.
C                  A TAG IS A LITTLE LINE (.1 INCHES LONG) PRECEDING TIME ZERO
C                  ON THE TRACE.
C                =0, NO TAG IS TO BE PUT ON THE TRACE.
C                =1, TAG THE TRACE.
C                =2, CREATE A SPACE IN THE PLOT (A NULL TRACE).
c                =3, Time line annotation only
C
C  COMMON VARIABLES:
C      COMMON /SEISPL/OFFSET,DEF,PCTFIL,VSCALE,TRPIN,ITYPE,TLINES(4),
C     *  STIMEL,TIMEL,IUNPLT,WIGGLE,BIAS,LANNO,ICDOTS,scalar,clip
C         OFFSET - THE OFFSET IN INCHES OF THE FIRST POINT TO BE PLOTTED FROM
C                  THE TOP OF THE PLOT.
C         DEF    - DEFLECTION IN INCHES. SEE ITYPE FOR THE TYPE OF SCALING.
C                  DEFLECTION IS USUALLY THE DISTANCE BETWEEN THE MIN AND MAX VALUES.
C         PCTFIL - PERCENT FILL - THE PERCENTANGE (E.G. 75.) OF THE MAXIMUM DEFLECTION
C                   THAT SHOULD GET VARIABLE AREA SHADING.
C                >0, POSITIVE NUMBERS ARE SHADED.
C                =0, WIGGLE TRACE ONLY SECTION.
C                <0, NEGATIVE NUMBERS ARE SHADED
C                    A SMALL PCTFIL REDUCES RUN TIME SOMEWHAT.
C         VSCALE - VERTICAL SCALE IN INCHES PER SECOND.
C         TRPIN  - TRACES PER INCH, OR THE HORIZONTAL SCALE
C         ITYPE  - THE TYPE OF SCALING TO PERFORM BEFORE PLOTING
C                =0, NO SCALING.
C                =1, THE MAXIMUM VALUE WILL SPAN DEF INCHES.
C                =2, THE MAXIMUM AND MINIMUM VALUES WILL SPAN DEF INCHES.
C         TLINES - AN ARRAY OF TIMING LINES TO PLOT. EACH SUCCESSIVE TIME LINE SET
C                  WILL GET A DARKER LINE.  EG.  TLINE(1)=.1,TLINE(2)=.5
C         IUNPLT - THE INTEGER DISC FILE UNIT NUMBER.
C         WIGGLE - WHEN SET TO NON-ZERO, NO WIGGLE TRACE IS PLOTTED FOR
C                  AMPLITUDES GREATER THAN WIGGLE*DEF IF WIGGLE>0, OR NO TRACE
C                  PLOT FOR AMPLITUDES LESS THAN WIGGLE*DEF IF WIGGLE<0.
C         BIAS   - THE BIAS OR ADDITIVE OF EACH SAMPLE OF EACH TRACE.
C                  GIVEN AS A PERCENTAGE (OF THE DEFLECTION).
C         LANNO  - THE ANNOTATION (CHARACTERS) TO PRECEDE THE TAG, IF TAG=1. THE
C                  PRESENT LIMIT IS 8 CHARACTERS.
C         ICDOTS - WHEN SET TO 1, THE DOTS (NIBS) ARE CONNECTED AS IF THE PLOT
C                  WAS DONE BY AN INCREMENTAL PEN PLOTTER. I.E. THE PEN IS LEFT
C                  DOWN WHILE MOVING FROM POINT TO POINT.
C
C      COMMON /VERSAT/NIBS,RNIBS
C     NIBS  - THE NUMBER OF NIBS (DOTS PER INCH) OF THE PLOTTER TO BE USED.
C             THIS IS INTEGER*2.
C
C    PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, DECEMBER 1978
C
C    MODIFIED MAY 1979:
C      1. TO USE LESS MEMORY (WAS A 132,400) ARRAY
C    MODIFIED IN NOVEMBER 1979 FOR THE AP
c  mod jan 90 for the Versatec 7424 plotter - increase ibuf to (600,400),
c      which at 400 dots per inch is 1 inch deflection!
c  mod apr 90 for the Epson "RuggedWriter 480" - a 120 dot printer
c  mod summer 1990 to add sipath and srpath  rework hscale
c  mod Nov 9, 1990 for the Versatec 7436
c  mod 16 Jan 1990 to set iwrdcount once and once only
c  mod 2 Apr 92 for HP 7600 to get the plot size
c mod 12 May 92  Remove DUTABLE - DecStation is same as Dec Vax
c mod 14 May 92 - Add color (add arrays buf2 and buf3)
c mod 21 aug 92 - add logic for positive and negative color
c mod 14 Sep 92 - New color logic screwed up LTR plots (really pltfil < 0
c                 and the amplitude < 0). Filled 1 extra nib!
c mod 30 Sep 92 - Add NovaJet - nibs 300 - Color inkjet
c mod 12 Oct 92 - Add HP 2848 - nib 2848 HP B&W 36in.
c mod 2 Nov 92 - Add HP 2847 - nib 2848 HP B&W 24.in.
c mod 4 Feb 93 - Add CYM (versus RGB) plotters.
c mod 5 Feb 93 - Add Versatec 3444
c mod 3 Mar 93 - Make ltr and rtl treat zero the same.  Assume ltr is
c                when pltfil < 0.
c mod 16 Mar 93 - Drop the first 250 and last 250 raster lines on the
c                 Sun raster files.  (it's just timing lines).
c mod 12 May 93 - Add HP 650 color InkJets
c               - put nibs and plot size on line 1 of plotfile
c mod 20 Jul 93 - Don't write the plotsize to opath if opath isn't given
c mod 30 Nov 93 - Add "uneven" sampling
c mod 20 Apr 94 - Add OYO GS-624 plotter
c mod 10 Nov 94 - Removed fuzzy logic for base color
c mod 30 Nov 94 - Add nibs 75 for screen
c mod 13 Feb 95 - Add nibs 3436, 9315, 9800
c mod 23 Feb 95 - Add time line annotation
c mod 7 June 95 - Make nibs 300 the HP color LaserJet
c mod 26 Jul 95 - swap bytes on Alpha color plot header word (8936 only)
c mod 22 Nov 95 - too many byte swaps of Alpha and DesignJets
c mod 12 feb 96 - ltr color plots didn't have negatives plotted
c               - positive/negative  ltr/rtl didn't have same fill/wiggle
c                 due to roundoff converting to integer.
c mod 21 feb 96 - More work on ltr and color
c mod 9 mar 96 - Change the uneven plot sample interpolation spacing
c mod 11 Jun 97 - small negative colors were bad if zero was not defined
c mod 16 Jun 97 - Changed coloring when there isn't any color at zero.
c mod 8 Jul 97 - positive amplitude on normal plots wasn't filled
c                (due to changes made in June!)
c mod 27 Dec 97 - Move where the ImageTool file (sipath) is written.
c mod 17 Apr 98 - Increase defs and colors from 6 to 8
c               - Correct zero amplitudes when l2r
c mod 20 Jun 98 - ltr ne rtl on color plots
c mod 30 Sep 98 - The interpolation index was wrong sometimes (bad logic
c mod 28 Mar 99 - Time line annotation on color plots was writing out of bounds
c mod June 00 - g77 didn't compile on a MIN1 statement
c mod Aug 00 -  Don't subtract -1 on BIGIST
c mod 31 Jan 01 - Add gray scales 0-100
c mod 27 Nov 01 - ltr gray didn't work because all defs are negative
c               - Do trace to def comparison in floating point
c mod 6 Feb 03 - Annotation on ltr grayscale & color was not black
c mod 7 Feb 03 - Add ndptr (number of dots to fill) - sorta like clip
c mod 31 Jul 03 - Do left side time line annotation if nib > 200
c mod 4 Aug 03 - Increase iwrdcount by 6 to accomodate lanno2
c              - Add spp2 to hold lanno2.
c mod 14 Jan 04 - ndptr 1 plots were empty
c mod 7 Mar 04 - That mod caused negatives to be filled when ndptr=0
c mod Apr 04  - Add nibs 2368
c mod 7 May 04 - Now B&W, wiggle 0 was plotting a trace of 0.
c mod 27 Jul 04 - ann2 didn't work on color or gray plots
c mod 29 Oct 04 - Bad index on left side annotation caused color plots
c                 to have bad last raster line.
c mod 16 Feb 05 - subroutine polint gave a huge erroneous answer.
c mod 29 Jul 06 - left justify lanno2
c mod 22 Mar 07 - Remove some Printronix and C.Itoh stuff
c               - Add itag 3 (Time line annotation only)
c mod 14 Nov 07 - Bizzare.  When dir ltr and nsamps changes, the annotation
c              location is wrong - force nsamps to be the same when dir ltr
c mod Dec 07 - Add TRIM
c mod 24 Jun 08 - Skip time line annotation if it'll cause a buffer overflow.
c mod 14 Jul 08 - Add warning when colored and nsamps > 8001
c               - Limit the generation of time lines to nsecs
c mod 20 Aug 08 - nsamps must be the same every time or the annotation location
c                 changes, even on dir rtl (see 14 Nov 07 change!)
c mod 31 Oct 08 - Increase NLINES from 600 to 601 because amplitudes might round up
c mod 19 Nov 09 - Drop decimal from side annotation if depth data.
c mod 12 Feb 10 - Very small sample interval misses uneven check.
c mod 29 Apr 10 - Allow 44inches @ 600dpi (1650 words)
c               - remove citoh stuff
c mod 10 Jun 10 - pctfil 0 ndptr 0 still put in 1 raster of fill
c mod 16 Oct 10 - Do end-of-plot time line annotation when nibs=200 (e,g, 7224)
c
C****
      PARAMETER (MIDJ=301)                                              !  THE J INDEX OF ZERO AMPLITUDE OF A TRACE
      PARAMETER (NLINES=601)                                            ! must be able to hold 1 inch of rasters
      PARAMETER (MWRDS=1650)                                            !  maximum NUMBER OF 16 bit  WORDS TO SEND TO THE PLOTTER
c   44 inch 600 dpi = 44*600/16 = 1650
      DIMENSION TRACE(1)                                                ! THE ARRAY WHERE THE REAL AMPLITUDES ARE
      INTEGER*2 IBUF, ibuf2, ibuf3
      COMMON /PLTCOM/ IBUF(mwrds,nlines), ibuf2(mwrds,nlines),
     &      ibuf3(mwrds,nlines)
C           THE LAST LINE IS THE FIRST SENT TO THE PLOTTER
C IN OTHER WORDS, THE n TH COLUMN REPRESENTS THE RIGHT HAND SIDE OF THE SECTION
C  THE FIRST WORD IS THE HORIZONTAL DIRECTION NIB COUNT OR POINTER.  A LINE
C  WILL BE SHIPPED TO THE PLOTTER WHEN IT'S POINTER OR NIB COMES UP WITH THE
C  SEISMIC TRACE.
      INTEGER*2 JBUF(MWRDS),ISCR(MWRDS), jbuf2(mwrds), jbuf3(mwrds)
      INTEGER*2 ITABLE(16),DVTABLE(16),DUTABLE(16), ioff(16), itsave
      DIMENSION JTABLE(NLINES)                                          ! HOLDS THE INDEXES OF THE RASTERS OR COLUMNS
C            THIS WAY THE BUFFERING WILL BE CIRCULAR
      DIMENSION tlines_trim(4)
      CHARACTER*8 LANN(NLINES), lann2(nlines)                           ! HOLDS THE ANNOTATION FOR EACH RASTER LINE
      COMMON /SEISPL/OFFSET,DEF,PCTFIL,VSCALE,TRPIN,ITYPE,TLINES(4),
     *  STIMEL,TIMEL,IUNPLT,WIGGLE,BIAS,idummy,ICDOTS,scalar,clip,itlan,
     &  irectify, ndptr, chart(2), size, itrim
      COMMON /PLOT/munit,nlists,ftag,taginc,fspace,nspace,spacei,idir,
     &    irecsp, lunhead, nraster
      COMMON /EDITS/IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /lanno/lanno, lanno2
      CHARACTER*8 LANNO, lanno2
      CHARACTER*20 line1
      COMMON /VERSAT/NIBS,RNIBS
      COMMON /SUNRAS/ lunras, iwrdcount, ilincount, irashead(8)
      COMMON /imaget/ maxx, maxy, lunimg
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *          IDECIM
      COMMON /MAXSCS/ NPRMUL,RELAMP
      COMMON /colors/ defs(9), colors(9), ncolors, bcolor, rgb, ngray
      INTEGER colors, bcolor, rgb
      COMMON /apmem/ a(100000)
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      LOGICAL FIRST
      SAVE
      DATA FIRST/.TRUE./
c****  WARNING ***  LOOKOUT  ****  I AM CONFUSED!!!! (What's new?)
c****  There is still some question about bit/byte order on DEC and
c****  Printronix.  Does prntx/prntx2 take care of it?
c****  The confusion is probable because:
C****  NON-DEC GOES 2**15,2**14,2**13,...,2**0
C****  DEC UNIX GOES 2**0,2**1,2**2,....,2**15    , THERFORE USE DTABLE
c***** DEC VMS goes 2**8,2**9,..2**15, 2**0,2**1,....2**7
      DATA ITABLE/-32768,16384,8192,4096,2048,1024,512,
     *          256,128,64,32,16,8,4,2,1/
      DATA DUTABLE /1,2,4,8,16,32,64,128,256,512,1024,
     *   2048,4096,8192,16384,-32768/
      DATA DVTABLE/128,64,32,16,8,4,2,1,
     *      -32768,16384,8192,4096,2048,1024,512,256/
      DATA jbuf/mwrds*0/, it/1/, nplane/1/, ndone/0/, uneven/0/,
     &     nsiolines/0/
C****
C****
c      print *,' in trplot, sr=',sr,' nsampsin=',nsampsin,' isig=',isig,
c     &      ' jdens=',jdens,' itag=',itag,' lanno2=',lanno2,' f',cfirst,
c     &      ' ndone=',ndone,' iunplt=',iunplt
      IF(.NOT.FIRST) GO TO 100
      FIRST=.FALSE.
      nsamps = nsampsin
      nsamps1 = nsampsin
      iwrdcount = 0
c**** nwrds is the number of 16 bit words in one raster line
      IF(NIBS.EQ.0) NIBS=200
      NWRDS=132
      IF(NIBS.EQ.160) NWRDS=180
      IF(NIBS.EQ.100) NWRDS=82
      IF(NIBS.EQ.60) NWRDS=99
      IF( nibs .EQ. 128 ) nwrds = 80                                    ! 128*10/16
      IF( nibs .EQ. 75 ) nwrds  = 200
      IF( nibs .EQ. 80 ) nwrds=64
      IF( nibs .EQ. 120 ) nwrds = 96                                    ! 1536 / 16 =96
      IF( nibs .EQ. 201 ) nwrds = 260
      IF( nibs .EQ. 300 ) nwrds = 159                                   ! 318 bytes or 2544 nibs
      IF( nibs .EQ. 624 ) nwrds = 592
      IF( nibs .EQ. 850 ) nwrds = 108
      IF( nibs .EQ. 2124 ) nwrds = 1012  ! actually 24*600/16 = 1012.5
      IF( nibs .EQ. 2144 ) nwrds = 1626    ! 44 inch HP Z2100
      IF( nibs .EQ. 2368 ) nwrds = 148                                  ! 2368 bytes or 2368 nibs
      IF( nibs .EQ. 2847 .OR. nibs .EQ. 2858 ) nwrds = 450
      IF( nibs .EQ. 2848 .OR. nibs .EQ. 2859 ) nwrds = 656
      IF( nibs .EQ. 3436 .OR. nibs .EQ. 8936 ) nwrds = 856
      IF( nibs .EQ. 3444 ) nwrds = 1076
      IF( nibs .EQ. 5732 ) nwrds = 294
      IF( nibs .EQ. 4160 ) nwrds = 137
      IF( nibs .EQ. 7222 ) nwrds = 264
c**** oh boy, here's the Versatec 7x24 series snafu. Is it 288 or 294?
c**** the problem is whether it is in inches or centimenters
      IF( nibs .EQ. 7224 ) nwrds = 288
      IF( nibs .EQ. 7225 ) nwrds = 294
      IF( nibs .EQ. 7422 ) nwrds = 528
      IF( nibs .EQ. 7424 ) nwrds = 576
      IF( nibs .EQ. 7425 ) nwrds = 588
      IF( nibs .EQ. 7436 ) nwrds = 880
      IF( nibs .EQ. 7444 ) nwrds = 1076
      IF( nibs .EQ. 7600 ) nwrds = 896
      IF( nibs .EQ. 8122 ) nwrds = 132
      IF( nibs .EQ. 8222 ) nwrds = 264
      IF( nibs .EQ. 8242 ) nwrds = 512                                  ! the 9242 in black and white is this too
      IF( nibs .EQ. 8625 ) nwrds = 588
      IF( nibs .EQ. 9242 ) nwrds = 500                                  ! color mode
      IF( nibs .EQ. 9315 ) nwrds = 128
      IF( nibs .EQ. 9800 ) nwrds = 256
      DOTSX=RNIBS                                                       ! THE NUMBER OF DOTS PER INCH ALONG THE X-AXIS
      DOTSY=RNIBS                                                       ! THE NUMBER OF DOTS PER INCH ALONG THE Y-AXIS
      IF( NIBS.EQ.60.) DOTSX=72.                                        ! THE PRINTRONIX 300 IS 60 BY 72 DOTS
      IF( nibs .EQ. 4160 ) dotsx = 168
      IF( DEF*DOTSX/ITYPE .GE. NLINES/2 )THEN
          PRINT *,' ***  ERROR  ***  THE DEFLECTION IS TOO BIG,',
     *   ' DECREASE DEF.'
          STOP
      ENDIF
      BIGIST = rnibs * clip                                             ! THE BIGGIST TRACE VALUE WITHOUT OVERFLOW IN UNITS OF NIBS
      ADD=BIAS*DEF*DOTSX/100.
      ipos = 1
      DO i = ncolors, 1, -1                                             ! convert color deflections to dots
         IF( defs(i) .GE. 0. ) ipos = i
         defs(i) = defs(i) * rnibs
c         idefs(i) = NINT(defs(i))
      ENDDO
c**** gray scale plots only plot positive amplitudes
      IF( ngray .NE. 0 ) THEN
          ipos = 1
          IF( idir .LT. 0 ) THEN
              ipos = ngray+1
              ngray = -ngray
          ENDIF
      ENDIF
c      IF( ncolors .NE. 0 .AND. nsamps .GT. 8001 ) THEN
c        PRINT *,' ***  WARNING  ***  Plot may be bad due to input size.'
c        PRINT *,' Suggest using DISKIN parameter SET to limit the size.'
c        PRINT *,' Color and gray plots are limited to 8001 samps input.'
c      ENDIF
      NOFF=OFFSET*DOTSY
      apctfil = ABS(pctfil)
      FMAX=DEF*PCTFIL/100.*DOTSX                                        ! PERCENT FILL, IN DOTS
      fmaxabs = ABS(fmax)
c      IF(ABS(PCTFIL).GT.99.) FMAX=FMAX*10.
      WIGMAX=DEF*WIGGLE/100.*RNIBS                                      ! FIND WHEN THE WIGGLE TRACE SHOULD BE TURNED OFF
c      IF(ABS(WIGGLE).GT.99.) WIGMAX=WIGMAX*10.
      temp = SR*(VSCALE+.001)*DOTSY                                     ! INCREMENT IN DOTS BETWEEN DATA SAMPLES
      iinc = temp
      IF( temp - iinc .GT. .01 ) THEN
          PRINT *,
     *        ' ***  WARNING  ***  Uneven spacing between plot samples.'
          PRINT *,'                  Interpolation will be done.'
          temp = 1. / (sr*dotsy)
          PRINT *,'   Use vscale ',temp,' to avoid it.'
          uneven = 1
      ENDIF
      IF( iinc .LT. 1 ) iinc = 1
      N2DUMP=DOTSX/TRPIN                                                ! NUMBER OF LINES TO SEND TO THE VERSATEC PER TRACE
      IDENS=JDENS
      IF(IDENS.LE.0) IDENS=IINC
      KSAMPS=NSAMPS*IINC/IDENS-1                                        ! NUMBER OF SAMPLES AFTER RESAMPLING
      IF( uneven .EQ. 1 ) ksamps = nsamps * sr * vscale * dotsy
      itotsamp = ksamps + noff                                          ! needed for Sun rasterfile
      IF( idir .LT. 0 ) itotsamp = itotsamp + noff
      IF( iwrdcount .EQ. 0 ) iwrdcount = itotsamp / 32 + 8              ! needed for Sun rasterfile
c     trim starts with word 17
      IF( itrim .NE. 0 ) iwrdcount = iwrdcount - 16
      IF(KSAMPS+NOFF.GT.NWRDS*16) KSAMPS=NWRDS*16-NOFF
      MSAMPS=NSAMPS+1
      JINC=IINC/IDENS
      IF( JINC .LT. 1 ) uneven = 1
      IF(JINC.EQ.1) KSAMPS=NSAMPS
      KINC=JINC-1
      NDOTSC=.10*RNIBS                                                  ! NUMBER OF DOTS IN .1 INCH (A CHARACTER)
      TIMEL=SR*NSAMPS
      IF( nsecs .NE. 0 .AND. timel .GT. nsecs ) timel = nsecs
c  ALWAYS.    The background is bcolor.  The time lines are black.
c  Ploted made colors into the 3 bit (or 3 plane) index.  The colors are
c  0 = black, 1 = red, 2 = green, 3 = yellow, 4 = blue, 5 = magenta,
c  6 = cyan, 7 = white.  THINK BINARY!  Think RGB
c  Don't do the wiggle trace on color - fill or shade only
      IF( ncolors .NE. 0 ) THEN
          itemp = 0
          itemp2 = 0
          itemp3 = 0
          IF( rgb .EQ. 1 ) THEN
              IF( IAND(bcolor,1) .NE. 0 ) itemp = -1
              IF( IAND(bcolor,2) .NE. 0 ) itemp2 = -1
              IF( IAND(bcolor,4) .NE. 0 ) itemp3 = -1
          ELSE
              IF( bcolor .EQ. 1 ) THEN
                  itemp2 = -1
                  itemp3 = -1
              ELSEIF( bcolor .EQ. 2 ) THEN
                  itemp = -1
                  itemp3 = -1
              ELSEIF( bcolor .EQ. 3 ) THEN
                  itemp3 = -1
              ELSEIF( bcolor .EQ. 4 ) THEN
                  itemp = -1
                  itemp2 = -1
              ELSEIF( bcolor .EQ. 5 ) THEN
                  itemp2 = -1
              ELSEIF( bcolor .EQ. 6 ) THEN
                  itemp2 = -1
c              ELSEIF( bcolor .EQ. 7 ) THEN
c                  itemp = -1
c                  itemp2 = -1
c                  itemp3 = -1
              ENDIF
          ENDIF
          DO 8 i = 1, nwrds
             jbuf(i) = itemp
             jbuf2(i) = itemp2
             jbuf3(i) = itemp3
    8     CONTINUE
          wiggle = 0.
      ENDIF
c**** Must use 3 buffers because the background color might be colored!
      IF( itrim .EQ. 1 .OR. itrim .EQ. 3 ) THEN
          DO i = 1, 4
             tlines_trim(i) = 0
          ENDDO
          tlines_trim(1) = timel
          IF( uneven .EQ. 0 ) THEN
              CALL GENTL(JBUF,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
              CALL GENTL(JBUF2,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
              CALL GENTL(JBUF3,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
          ELSE
              CALL GENTL2(JBUF,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
              CALL GENTL2(JBUF2,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
              CALL GENTL2(JBUF3,tlines_trim,VSCALE,TIMEL,OFFSET,idir )
          ENDIF
      ENDIF
      IF( tlines(1) .NE. 0 ) THEN
          IF( uneven .EQ. 0 ) THEN
              CALL GENTL( JBUF, TLINES, VSCALE, TIMEL, OFFSET, idir )
              CALL GENTL( JBUF2, TLINES, VSCALE, TIMEL, OFFSET, idir )
              CALL GENTL( JBUF3, TLINES, VSCALE, TIMEL, OFFSET, idir )
          ELSE
              CALL GENTL2( JBUF, TLINES, VSCALE, TIMEL, OFFSET, idir )
              CALL GENTL2( JBUF2, TLINES, VSCALE, TIMEL, OFFSET, idir )
              CALL GENTL2( JBUF3, TLINES, VSCALE, TIMEL, OFFSET, idir )
          ENDIF
      ENDIF
      DO 20 I=1,NLINES                                                  ! SET UP THE J INDEXES
         LANN(I)=' '
         lann2(i) = ' '
         JTABLE(I)=I
         DO 10 J = 1, NWRDS
   10    IBUF(J,I) = JBUF(J)
         IF( ncolors .NE. 0 ) THEN
             DO 15 j = 1, nwrds
                IBUF2(J,I) = JBUF2(J)
                IBUF3(J,I) = JBUF3(J)
   15        CONTINUE
         ENDIF
   20 CONTINUE
c**** drop the decimal point in side ann if depth data
      idecimal = 1
      IF( ibinhdr(31).EQ. 6 ) idecimal = 0
c**** annotate the first set of tlines (ie tlines(1))
      IF( tlines(1) .NE. 0 .AND. itlan .NE. 0 ) THEN
          DO i = 1, 4
             IF( tlines(i) .NE. 0 ) temp = tlines(i)
          ENDDO
          itemp = 450
          IF( rnibs .GT. 250 ) itemp = 350
c          IF( rnibs .GT. 450 ) itemp = 50
          itemp1 = noff
          IF( idir .GE. 0 ) THEN
              IF( itrim .EQ. 1 .OR. itrim .EQ. 2 ) itemp1 = 17
              CALL tlann( ibuf(1,itemp), itemp1, mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
              IF( ncolors .NE. 0 ) THEN
                 CALL tlann( ibuf2(1,itemp), itemp1, mwrds,vscale,rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
                 CALL tlann( ibuf3(1,itemp), itemp1, mwrds,vscale,rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
              ENDIF
          ELSEIF( rnibs .GE. 200 ) THEN
              itemp = 350
c              IF( itrim .EQ. 1 .OR. itrim .EQ. 3 ) itemp1 = noff
              CALL tlann( ibuf(1,itemp), itemp1, mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
              IF( ncolors .NE. 0 ) THEN
                 CALL tlann( ibuf2(1,itemp), itemp1, mwrds,vscale,rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
                 CALL tlann( ibuf3(1,itemp), itemp1, mwrds,vscale,rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 0, idecimal )
              ENDIF
          ENDIF
      ENDIF
c**** if this is a DEC computer, move dtable on top of itable
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN                       ! DecStation or Dec Vax
          DO 40 i=1,16
   40     itable(i) = dvtable(i)
      ENDIF
c**** itable are the bits turned on, ioff are the bits turned off
      DO 50 i = 1, 16
   50 ioff(i) = NOT(itable(i))
C****
C  SCALE THE TRACE INTO DOTS RATHER THAN AMPLITUDES - THEREFORE EACH AMPLITUDE
C   OR SAMPLE WILL BE A MEASURE OF DOTS AWAY FROM THE ZERO AXIS.
C****
  100 CONTINUE
      IF(ISIG.EQ.1) GO TO 3000                                          ! FLUSH THE BUFFER IF ISIG=1
      nsamps = nsamps1
      KSAMPS=NSAMPS*IINC/IDENS                                          ! NUMBER OF SAMPLES AFTER RESAMPLING
      IF( uneven .EQ. 1 ) ksamps = nsamps * sr * vscale * dotsy
      IF(KSAMPS+NOFF.GT.NWRDS*16) KSAMPS=NWRDS*16-NOFF
      MSAMPS=NSAMPS
      IF( msamps*jinc .GT. ksamps ) msamps = ksamps/jinc
      IF(JINC.EQ.1 .AND. uneven .EQ. 0 ) KSAMPS=NSAMPS
      itotsamp = ksamps + noff                                          ! needed for Sun rasterfile
c****  OOPS, the sun rasterfile must have each raster line the same
c**** length.  This here will fall apart if the first trace is a space
c**** and the number of samples is not the same as the data!
      IF( iwrdcount .EQ. 0 ) iwrdcount = itotsamp / 32 + 7              ! needed for Sun rasterfile
      IF( IABS(itag) .GT. 1 ) GO TO 1100                                ! IS IT A NULL TRACE
      IF( relamp .GT. -99990. ) THEN
          SAVE=RELAMP                                                   ! SAVE RELAMP BECAUSE STATEMENT 130 CHANGES IT
          IF( IASGND .NE. 0 .AND. IUSEAP .EQ. 1 .AND. IN .NE. 0 ) THEN  ! IS THE AP ASSIGNED
              IOUT=0                                                    ! SIGNAL MAXSCA TO GET THE DATA OUT OF THE AP INTO TRACE!
              CALL MAXSCA(TRACE,DEF,NSAMPS,ITYPE,PEAK,TROUGH)
          ELSE
              CALL MAXSC(TRACE,DEF,NSAMPS,ITYPE,PEAK,TROUGH)            !  DO IT IN MEMORY
          ENDIF
          relamp = save
      ELSE
          CALL scalet( trace, nsamps, def, scalar, rnibs )
      ENDIF
      IF( BIAS .NE. 0 ) THEN
          DO 110 I=1,NSAMPS                                             !  APPLY THE BIAS AFTER EVERYTHING ELSE
  110     TRACE(I)=TRACE(I)+ADD
      ENDIF
c****
C****   Do the IMAGETOOL thing and return if no other type of plotting.
c****
      IF( lunimg .NE. 0 .AND. isig .NE. 1 ) THEN
          CALL wrdisc( lunimg, trace, nsamps )
          maxx = maxx + 1
          IF( nsamps .GT. maxy ) maxy = nsamps
      ENDIF
      IF( lunras + iunplt .EQ. 0 ) RETURN
      IF( clip .NE. 0.) THEN                                            ! LOOK OUT FOR OVERFLOW!!
          DO 130 I=1,NSAMPS
             IF( ABS(TRACE(I)) .GT. BIGIST ) THEN
                 TRACE(I)=SIGN(BIGIST,TRACE(I))                         ! CLIP THE DATA IF IT IS TOO BIG
             ENDIF
  130    CONTINUE
      ENDIF
C****
C    RESAMPLE THE DATA SO THAT EACH SAMPLE IS JDENS DOTS APART.
C****
c	print *,' ksamps=',ksamps,' msamps=',msamps,' kinc=',kinc
      IF( uneven .EQ. 0 .AND. jinc .NE. 1 .AND. ksamps .NE. msamps) THEN
c     Do a fast linear interpolation
          JSAMPS=KSAMPS
          DO 140 i = 1, kinc
             trace(jsamps) = 0.
             jsamps = jsamps - 1
  140     CONTINUE
          DO 200 i = 1, nsamps
             TRACE(JSAMPS)=TRACE(MSAMPS-I+1)
             IF( jsamps .EQ. 1 ) GOTO 201
             JSAMPS=JSAMPS-1
             T=(TRACE(MSAMPS-I+1)-TRACE(MSAMPS-I))/JINC
             DO 150 J=1,KINC
                TRACE(JSAMPS)=TRACE(JSAMPS+1)-T
                JSAMPS=JSAMPS-1
  150        CONTINUE
  200     CONTINUE
      ENDIF
  201 IF( uneven .EQ. 1 ) THEN
c   do a polynomial interpolation of order 4, as discussed in "Numerical
          iorder = 4
          index1 = nextad
          IF( index1 .LE. 1 ) index1 = 1
          index2 = index1 + nsamps + iorder
          index = index2
          index3 = index2 + ksamps
          injex = 1
c****   the old way was:
c          spacing = FLOAT(nsamps) / FLOAT(ksamps)
c         another way to do it is:
          spacing = (1./sr) / (dotsy*vscale)
          DO 210 i = 1, nsamps + 1
  210     a(index1+i-1) = FLOAT(i-1)
          DO 220 i = 1, iorder-1
  220     trace(nsamps+i) = 0.
c  i=  27 x=    77.0370 a=    75.0000 dex  0  75
c  i=  28 x=    80.0000 a=    78.0000 dex  0  78
c  i=  29 x=    82.9630 a=    81.0000 dex  0  81 **********
c  i=  30 x=    85.9259 a=    83.0000 dex  0  83
c  i=  31 x=    88.8889 a=    86.0000 dex  0  86
          DO 240 i = 1, ksamps
             xx = FLOAT(i-1) * spacing
             CALL polint( a(index1+injex-1), trace(injex), iorder, xx,
     &               a(index2+i-1), a(index3+i-1) )
c     given a(index1+injex-1) and trace(injex) and xx it returns
c     a point a(index2+i-1) and error estimate a(index3+i-1)
c****  If spacing < 1
             injex = NINT(FLOAT(i) * spacing)
  240     CONTINUE
  250     DO  i = 2, ksamps
              trace(i) = a(index2+i-2)
              IF( ABS(trace(i)) .GT. 300 ) THEN
                  IF( i .GT. iorder ) PRINT *,
     &               ' Plot polynomial interpolation failed.',i,trace(i)
                  trace(i) = 0.
              ENDIF
          ENDDO
          trace(1) = 0.
      ENDIF
C****
C   TAG THE TRACES (IF DESIRED) BY EXTENDING THE TRACE ALONG THE ZERO LINE
C   IN BOTH THE FRONT AND BACK OF THE TRACE.  TAGS WILL BE .1 IN.
C****
      nraster = ndone + midj
      IF( ITAG .NE. 0 .AND. itrim .LE. 0 ) THEN
          K=JTABLE(MIDJ)
          IHELP=K+7
          IF(IHELP.GT.NLINES) IHELP=IHELP-NLINES
          LANN(IHELP)(1:8)=LANNO(1:8)
          lann2(ihelp)(1:8) = lanno2(1:8)
          DO i = 1, 7
             IF( lann2(ihelp)(1:1) .EQ. ' ' ) THEN
                 lann2(ihelp)(1:7) = lann2(ihelp)(2:8)
                 lann2(ihelp)(8:8) = ' '
             ENDIF
          ENDDO
          DO 290 I=1,NDOTSC
             II=NOFF-I
             IWORD=(II-1)/16
             IBIT=II-IWORD*16
             IF( ncolors .EQ. 0 ) THEN
                 IBUF(IWORD,K) = IOR(IBUF(IWORD,K),ITABLE(IBIT))
             ELSE
                 IF( rgb .EQ. 1 ) THEN
                   IBUF(IWORD,K) = IAND( IBUF(IWORD,K), ioff(ibit))
                   IBUF2(IWORD,K) = IAND(IBUF2(IWORD,K),ioff(ibit))
                   IBUF3(IWORD,K) = IAND(IBUF3(IWORD,K),ioff(ibit))
                 ELSE
                   IBUF(IWORD,K) = IOR(IBUF(IWORD,K),ITABLE(IBIT))
                  IBUF2(IWORD,K) = IOR(IBUF2(IWORD,K),ITABLE(IBIT))
                  IBUF3(IWORD,K) = IOR(IBUF3(IWORD,K),ITABLE(IBIT))
                 ENDIF
             ENDIF
             II=KSAMPS+I+NOFF                                           ! TAG THE BACK SIDE
             IWORD=(II-1)/16
             IBIT=II-IWORD*16
             IF( ncolors .EQ. 0 ) THEN
                 IBUF(IWORD,K) = IOR(IBUF(IWORD,K),ITABLE(IBIT))
             ELSE
                 IF( rgb .EQ. 1 ) THEN
                   IBUF(IWORD,K) = IAND( IBUF(IWORD,K), ioff(ibit))
                   IBUF2(IWORD,K) = IAND(IBUF2(IWORD,K),ioff(ibit))
                   IBUF3(IWORD,K) = IAND(IBUF3(IWORD,K),ioff(ibit))
                 ELSE
                    IBUF(IWORD,K) = IOR(IBUF(IWORD,K),ITABLE(IBIT))
                  IBUF2(IWORD,K) = IOR(IBUF2(IWORD,K),ITABLE(IBIT))
                  IBUF3(IWORD,K) = IOR(IBUF3(IWORD,K),ITABLE(IBIT))
                 ENDIF
             ENDIF
  290     CONTINUE
          lastbit = (ii+ndotsc+ndotsc) / 16
      ENDIF
C****
C     FIGURE OUT WHAT BIT (AND WORD) NEEDS TO BE TURNED ON FOR EACH SAMPLE
C****
  300 DO 1000 I=1,KSAMPS                                                ! NOW FIND THE BIT POSITION WITHIN IBUF
c*****        if wiggle is 100%, everything is plotted
         IF( wiggle .LT. 0 .AND. trace(i) .LT. wigmax .AND.
     &       wiggle .GT. -100. ) GOTO 1000
         IF( wiggle .GT. 0 .AND. trace(i) .GT. wigmax .AND.
     &       wiggle .LT. 100. ) GOTO 1000
         II = NOFF+(I-1)*IDENS
         IF( itrim .NE. 0 ) THEN
             IF( idir .GE. 0 .AND. ( itrim .EQ. 1 .OR. itrim .EQ. 2) )
     &           ii = (i-1) * idens+17
c             IF( idir .LT. 0 .AND. ( itrim .EQ. 1 .OR. itrim .EQ. 3) )
c     &            ii = (i-1) * idens+1
         ENDIF
c**** If noff=0, then ii = 0, and iword becomes negative.
         IWORD = (II-1)/16
         IBIT = II-IWORD*16
         JJ = NINT(TRACE(I)) + MIDJ
         K = JTABLE(JJ)
C****    PUT THE WIGGLE TRACE DOWN
         IF( WIGGLE .NE. 0. .AND. ncolors .EQ. 0 )
     &       IBUF(IWORD,K)=IOR(IBUF(IWORD,K),ITABLE(IBIT))
C****    connect the dots by filling each sample halfway from the previous sample
         IF( ICDOTS .EQ. 1 .AND. i .NE. 1 .AND. wiggle .NE. 0 .AND.
     &       ncolors .EQ. 0 ) THEN                                      ! DON'T CONNECT THE DOTS UNLESS ASKED TO
             itemp = NINT(trace(i) - trace(i-1))
             K2 = ABS(itemp) / 2
             IF( K2 .GT. 1 ) THEN
                 IF( itemp .LT. 0 ) THEN
                     DO 322 I1=1,K2
                        JJJ=JJ+I1
                        K=JTABLE(JJJ)
                        IBUF(IWORD,K)=IOR(IBUF(IWORD,K),ITABLE(IBIT))
                        JJJ=JJSAVE-I1
                        K=JTABLE(JJJ)
                        IBUF(IWSAVE,K)=IOR(IBUF(IWSAVE,K),ITSAVE)
  322                CONTINUE
                 ELSEIF( itemp .GT. 0 ) THEN
                     DO 326 I1=1,K2
                        JJJ=JJ-I1
                        K=JTABLE(JJJ)
                        IBUF(IWORD,K)=IOR(IBUF(IWORD,K),ITABLE(IBIT))
                        JJJ=JJSAVE+I1
                        K=JTABLE(JJJ)
                        IBUF(IWSAVE,K)=IOR(IBUF(IWSAVE,K),ITSAVE)
  326                CONTINUE
                 ENDIF
             ENDIF
         ENDIF
         JJSAVE=JJ
         IWSAVE=IWORD
         ITSAVE=ITABLE(IBIT)
         icolor = bcolor
         itrace = NINT(trace(i))
c***     trace() and defs() have been converted to nibs
c****    ngray was made negative when doing ltr plots
         IF( (pctfil .LT. 0 .AND. itrace .LE. 0 .AND. ngray .EQ. 0 ).OR.
     &     ( ncolors .NE. 0 .AND. ngray .LE. 0 .AND. itrace .LT.0)) THEN
C****            ADD FILL TO THE NEGATIVE NUMBERS
                 IF( apctfil .EQ. 100. ) THEN
                     M = MIDJ  + itrace
                 ELSE
                     M = MIDJ +
     &                 NINT(SIGN(trace(i),AMIN1(ABS(TRACE(I)),FMAXABS)))
                 ENDIF
                 IF( ndptr .NE. 0 ) m = midj + ndptr - 1
c****  I give up, kludge for ltr, wiggle 0, trace=0
                 IF( m .EQ. midj .AND. ndptr+wiggle .EQ. 0) GOTO 1000
c****   see the discussion later about M .NE. vs M .GE.
c                 IF( ( M .NE. MIDJ .AND. ncolors .EQ. 0 ) .OR.
             IF( ( M .NE. MIDJ .AND. ncolors .EQ. 0 .AND. ndptr .NE. 0 )
     &           .OR. ( M .LE. MIDJ .AND. ncolors .EQ. 0.AND.ndptr.EQ.0)
     &           .OR. ( ncolors .GT. 0 ) ) THEN
                     IF( ncolors .GT. 0 ) THEN
                         IF( itrace .LE. 0 ) THEN
c                             DO j = 1, ncolors
c                                icolor = colors(j)
c                                IF( idefs(j) .GT. 0 ) GOTO 335
c                                IF( itrace .LE. idefs(j) ) GOTO 335
c                                IF( idefs(j+1) .GT. 0 ) GOTO 335
c                             DO j = ipos, ncolors
                             DO j = 1, ipos-1
                                icolor = colors(j)
                                IF( trace(i) .LT. defs(j) ) GOTO 335
                             ENDDO
                         ELSE
c****   kludge for gray scale ltr plots and neg (now pos amplitude)
                             IF( ngray.LT.0.AND.trace(i).GE.0)GOTO 1000
                             icolor = colors(ncolors)
                             DO j = ncolors, 1, -1
                                icolor = colors(j)
                                IF( trace(i) .GE. defs(j) ) GOTO 335
c   argh.  This assumes pos and neg defs, not the case with gray scale!
                                IF( j .NE. ncolors .AND.
     &                              defs(j-1) .LT. 0 .AND. ngray .EQ.0)
     &                              GOTO 335
                             ENDDO
                         ENDIF
  335                    CONTINUE
                         ired = 0
                         igreen = 0
                         iblue = 0
                         IF( rgb .EQ. 1 ) THEN
                             IF( IAND(icolor,1) .NE. 0 ) ired = 1
                             IF( IAND(icolor,2) .NE. 0 ) igreen = 1
                             IF( IAND(icolor,4) .NE. 0 ) iblue = 1
                         ELSE
                             IF( icolor .EQ. 1 ) THEN
                                 igreen = 1
                                 iblue = 1
                             ELSEIF( icolor .EQ. 2 ) THEN
                                 ired = 1
                                 iblue = 1
                             ELSEIF( icolor .EQ. 3 ) THEN
                                 iblue = 1
                             ELSEIF( icolor .EQ. 4 ) THEN
                                 ired = 1
                                 igreen = 1
                             ELSEIF( icolor .EQ. 5 ) THEN
                                 igreen = 1
                             ELSEIF( icolor .EQ. 6 ) THEN
                                 ired = 1
                             ENDIF
                         ENDIF
                     ENDIF
                     itemp = 1
                     IF( m .GT. midj ) itemp = -1                       ! 12 feb 96
c                     print *,' 340, i=',i,' trace(i)=',trace(i),
c     &                     ' DO',m,midj,itemp,
c     &                     ' icolor=',icolor,ired,igreen,iblue
                     DO 340 J = M, MIDJ, itemp
                        K = JTABLE(J)
                        IF( ncolors .EQ. 0 ) THEN
                           IBUF(IWORD,K)=IOR(IBUF(IWORD,K),ITABLE(IBIT))
                        ELSE
                            ibuf(iword,k)=IAND(ibuf(iword,k),ioff(ibit))
                          ibuf2(iword,k)=IAND(ibuf2(iword,k),ioff(ibit))
                          ibuf3(iword,k)=IAND(ibuf3(iword,k),ioff(ibit))
                            IF( ired .EQ. 1 ) ibuf(iword,k) =
     &                          IOR( ibuf(iword,k), itable(ibit) )
                            IF( igreen .EQ. 1 ) ibuf2(iword,k) =
     &                          IOR( ibuf2(iword,k), itable(ibit) )
                            IF( iblue .EQ. 1 ) ibuf3(iword,k) =
     &                          IOR( ibuf3(iword,k), itable(ibit) )
                         ENDIF
  340                CONTINUE
             ENDIF
         ELSE
C****        ADD FILL TO THE NON-NEGATIVE NUMBERS
             IF( apctfil .GT. 99. ) THEN
                 M = itrace + MIDJ
             ELSE
c****             g77 doesn't like the next statement
c                 M = MIN1(itrace,FMAX) + MIDJ
c                 The following prevented positive amplitudes from being
c                 plotted when idir=1 because pctfil is negated on dir ltr
c                 temp = MIN(FLOAT(itrace),fmax) + MIDJ
                 temp = MIN(FLOAT(itrace),fmaxabs) + MIDJ
                 m = NINT(temp)
             ENDIF
             IF( ndptr .GT. 0 ) m = midj + ndptr - 1
c****  was M .NE. MIDJ which means the thing must be at least 2 dots wide.
c****  True, but the "wiggle" was laid down earlier.  This is just fill.
c             IF( ( M .NE. MIDJ .AND. ncolors .EQ. 0 ) .OR.
c             IF( ( M .GE. MIDJ .AND. ncolors .EQ. 0 ) .OR.
c**** True, but M .NE. MIDJ caused negatives to be filled
c       print *,' trce=',trace(i),' midj=',midj,' m=',m,' pctfil=',pctfil
c	print *,' ndptr=',ndptr,' wiggle=',wiggle
             IF( ( M .NE. MIDJ .AND. ncolors .EQ. 0 .AND. ndptr .NE. 0 )
     &           .OR. ( M .GE. MIDJ .AND. ncolors.EQ.0 .AND. ndptr.EQ.0
     &                  .AND. pctfil .NE. 0 )
     &           .OR. ( ncolors .GT. 0 ) ) THEN
c****  Damn.  Now B&W, wiggle 0 doesn't work.
                 IF( m .EQ. midj .AND. ndptr+wiggle .EQ. 0 ) GOTO 1000
c****  Now. ltr (pctfil < 0) fills
                 IF( ncolors .EQ. 0 .AND. pctfil .LT. 0 .AND.
     &               trace(i) .GT. 0 ) GOTO 1000
                 IF( ncolors .GT. 0 ) THEN
                     IF( ngray .EQ. 0 ) THEN
                         icolor = colors(ipos)
                     ELSE
                         IF( ngray .NE. 0 ) THEN
c	 print *,ngray,pctfil,trace(i),defs(1)
                             IF( pctfil.LT.0 .AND. trace(i).GT.defs(1))
     &                          GOTO 1000
                             IF( pctfil.GT.0 .AND. trace(i).LT.defs(1))
     &                          GOTO 1000
                         ENDIF
                         icolor = 7
                     ENDIF
c                    we need to go through this loop once to get the color
                     DO 345 j = ipos, ncolors
c	 print *,' ipos=',ipos,' j=',j,' it ',itrace,' id ',
c     &   (defs(ii),ii=ipos,ncolors)
                        IF( trace(i) .LE. defs(j) ) GOTO 346
                        icolor = colors(j)
  345                CONTINUE
  346                CONTINUE
                     ired = 0
                     igreen = 0
                     iblue = 0
                     IF( rgb .EQ. 1 .OR. ngray .GT. 0 ) THEN
                         IF( IAND(icolor,1) .NE. 0 ) ired = 1
                         IF( IAND(icolor,2) .NE. 0 ) igreen = 1
                         IF( IAND(icolor,4) .NE. 0 ) iblue = 1
                     ELSE
                         IF( icolor .EQ. 1 ) THEN
                             igreen = 1
                             iblue = 1
                         ELSEIF( icolor .EQ. 2 ) THEN
                             ired = 1
                             iblue = 1
                         ELSEIF( icolor .EQ. 3 ) THEN
                             iblue = 1
                         ELSEIF( icolor .EQ. 4 ) THEN
                             ired = 1
                             igreen = 1
                         ELSEIF( icolor .EQ. 5 ) THEN
                             igreen = 1
                         ELSEIF( icolor .EQ. 6 ) THEN
                             ired = 1
                         ENDIF
                     ENDIF
                 ENDIF
c 	            print *,' 350, i=',i,' DO',midj,m,
c     &                 ' trace(i)=',trace(i),
c     &                ' icolor=',icolor,ired,igreen,iblue,
c     &                ' iword=',iword,' ibit=',ibit
                 itemp = 1
                 IF( m .LT. midj ) itemp = -1
                 DO 350 J = MIDJ, M, itemp
                    K = JTABLE(J)
                    IF( ncolors .EQ. 0 ) THEN
                        IBUF(IWORD,K) = IOR(IBUF(IWORD,K),ITABLE(IBIT))
                    ELSE
                        ibuf(iword,k) = IAND(ibuf(iword,k),ioff(ibit))
                        ibuf2(iword,k) = IAND(ibuf2(iword,k),ioff(ibit))
                        ibuf3(iword,k) = IAND(ibuf3(iword,k),ioff(ibit))
                        IF( ired .EQ. 1 ) ibuf(iword,k) =
     &                      IOR( ibuf(iword,k), itable(ibit) )
                        IF( igreen .EQ. 1 ) ibuf2(iword,k) =
     &                      IOR( ibuf2(iword,k), itable(ibit) )
                        IF( iblue .EQ. 1 ) ibuf3(iword,k) =
     &                      IOR( ibuf3(iword,k), itable(ibit) )
                        IF( rgb .EQ. 0 ) THEN
                          ibuf(iword,k) = IOR(ibuf(iword,k),jbuf(iword))
                         ibuf2(iword,k)=IOR(ibuf2(iword,k),jbuf2(iword))
                         ibuf3(iword,k)=IOR(ibuf3(iword,k),jbuf3(iword))
                        ENDIF
                     ENDIF
  350            CONTINUE
             ENDIF
         ENDIF
 1000 CONTINUE
C****
C****    NOW DUMP ENOUGH RASTERS TO PLOT 1 TRACE DISTANCE
C****
 1100 CONTINUE
      IF( IABS(itag) .GT. 2 ) GOTO 4000
      DO 2000 I=1,N2DUMP
         ndone = ndone + 1
         J=JTABLE(NLINES)
         IF( itrim .LE. 0 ) THEN
             IF( idir .GE. 0 ) THEN
                  CALL spp(lann(j),ibuf(1,j))                           ! OR the annotation in
                  CALL spp2(lann2(j),ibuf(lastbit-1,j))
             ELSE
                  CALL spp(lann(j),ibuf(lastbit,j))
                  CALL spp2(lann2(j),ibuf(4,j))
             ENDIF
             IF( ncolors .NE. 0 .AND. colors(1) .NE. 8 ) THEN
                 itemp = 16
                 IF( nibs .LT. 100 ) itemp = 8
                 DO k = 1, itemp
                    ibuf2(k,j) = ibuf(k,j)
                    ibuf3(k,j) = ibuf(k,j)
                    ibuf2(lastbit+k-2,j) = ibuf(lastbit+k-2,j)
                    ibuf3(lastbit+k-2,j) = ibuf(lastbit+k-2,j)
                 ENDDO
             ENDIF
         ENDIF
         IF( itrim .NE. 0 ) THEN
             IF( itrim .EQ. 1 .AND. ndone .LT. midj ) GOTO 1200
             IF(itrim.EQ.-2.AND.idir.GT.0.AND.ndone.LT.midj) GOTO 1200
             IF(itrim.EQ.-1.AND.idir.LT.0.AND.ndone.LT.midj) GOTO 1200
         ENDIF
         IF( lunras .NE. 0 .AND. ndone .GT. 40 ) THEN
             CALL WRDISC ( lunras, ibuf(1,j), iwrdcount)
             ilincount = ilincount + 1
         ENDIF
         IF( iunplt .NE. 0 ) THEN
             CALL wrdisc( iunplt, ibuf(1,j), nwrds/2 )
             nsiolines = nsiolines + 1
             IF( ncolors .NE. 0 ) THEN
                 CALL wrdisc( iunplt, ibuf2(1,j), nwrds/2 )
                 CALL wrdisc( iunplt, ibuf3(1,j), nwrds/2 )
             ENDIF
         ENDIF
 1200    CONTINUE
         DO KK = 1,NLINES                                           ! PRIME FORTRAN DOESN'T ALLOW BACKWARD INDEXING
            K = NLINES+1-KK
            JTABLE(K) = JTABLE(K-1)                                 ! MOVE THE BUFFER INDEXES AROUND
         ENDDO
         JTABLE(1)=J
         DO K = 1,NWRDS
             IBUF(K,J) = JBUF(K)                                    ! RESET THE LAST ONE TO THE TIME LINES
         ENDDO
         IF( ncolors .NE. 0 .AND. colors(1) .NE. 8 ) THEN
             DO k = 1, nwrds
                ibuf2(k,j) = jbuf2(k)
                ibuf3(k,j) = jbuf3(k)
             ENDDO
         ENDIF
         LANN(J)=' '
         lann2(j) = ' '
 2000 CONTINUE
      RETURN
C****
C****   PLOT  THE ENTIRE BUFFER
C****
 3000 CONTINUE
      DO 3100 M = 1, NLINES                                             ! REMEMBER THAT INDEX INC MUST BE >0
          I=NLINES+1-M
          J=JTABLE(I)
          IF( m .EQ. 400 .AND. nibs .GT. 250 ) THEN
              DO ii = 1, nlines
                 lann(ii) = ' '
                 lann2(ii) = ' '
                 DO jj = 1, nwrds
                    ibuf(jj,ii) = jbuf(jj)
                 ENDDO
                 IF( ncolors .NE. 0 ) THEN
                     DO jj = 1, nwrds
                        ibuf2(jj,ii) = jbuf2(jj)
                        ibuf3(jj,ii) = jbuf3(jj)
                     ENDDO
                 ENDIF
              ENDDO
              IF( tlines(1) .NE. 0 .AND. itlan.NE.0) THEN
                  DO ii = 1, 4
                     IF( tlines(ii) .NE. 0 ) temp = tlines(ii)
                  ENDDO
                  itemp1 = noff
                  IF( idir .GT. 0 .AND. (itrim.EQ.1.OR. itrim .EQ. 2 ))
     &            itemp1 = 17
                  CALL tlann( ibuf(1,1), itemp1, mwrds, vscale, rnibs,
     &            temp, stimel, timel, ncolors * rgb, idir, 0, idecimal)
                  IF( ncolors .NE. 0 ) THEN
                  CALL tlann( ibuf2(1,1), itemp1, mwrds, vscale, rnibs,
     &            temp, stimel, timel, ncolors * rgb, idir, 0, idecimal)
                  CALL tlann( ibuf3(1,1), itemp1, mwrds, vscale, rnibs,
     &            temp, stimel, timel, ncolors * rgb, idir, 0, idecimal)
                  ENDIF
              ENDIF
          ENDIF
          IF( m .GE. 400 .AND. nibs .GT. 200 ) j = nlines - m + 1
c         lack of +1 caused bad index (j=0) on last raster line
          IF( itrim .LE. 0 ) THEN
              IF( idir .GE. 0 ) THEN
                  CALL spp(lann(j),ibuf(1,j))                           ! OR the annotation in
                  CALL spp2(lann2(j),ibuf(lastbit-1,j))
              ELSE
                  CALL spp(lann(j),ibuf(lastbit,j))
                  CALL spp2(lann2(j),ibuf(4,j))
              ENDIF
              IF( ncolors .NE. 0 ) THEN
                  itemp = 16
                  IF( nibs .LT. 100 ) itemp = 8
                  DO k = 1, itemp
                     ibuf2(k,j) = ibuf(k,j)
                     ibuf3(k,j) = ibuf(k,j)
                     ibuf2(lastbit+k-2,j) = ibuf(lastbit+k-2,j)
                     ibuf3(lastbit+k-2,j) = ibuf(lastbit+k-2,j)
                  ENDDO
              ENDIF
          ENDIF
          ndone = ndone + 1
          IF( itrim .NE. 0 ) THEN
c****     trim to the zero line, cutting off wiggles
c              IF( itrim .EQ. 1 .AND. ndone .LT. midj-wigmax ) GOTO 3100
              IF( itrim .EQ. 1 .AND. ndone .LT. midj ) GOTO 3100
              IF(itrim.EQ.-2.AND.idir.GT.0.AND.ndone.LT.midj ) GOTO 3100
              IF(itrim.EQ.-1.AND.idir.LT.0.AND.ndone.LT.midj ) GOTO 3100
c              IF( itrim .EQ. 1 .AND. m .GT. midj+migmax ) GOTO 3100
              IF( itrim .EQ. 1 .AND. m .GT. midj ) GOTO 3100
              IF(itrim.EQ.-1.AND.idir.GT.0 .AND. m .GT. midj ) GOTO 3100
              IF(itrim.EQ.-2.AND.idir.LT.0 .AND. m .LT. midj ) GOTO 3100
          ENDIF
          IF( lunras .NE. 0 ) THEN
              CALL WRDISC (lunras, ibuf(1,j), iwrdcount)
              ilincount = ilincount + 1
          ENDIF
          IF( iunplt .NE. 0 ) THEN
              CALL wrdisc( iunplt, ibuf(1,j), nwrds/2 )
              nsiolines = nsiolines + 1
              IF( ncolors .NE. 0 ) THEN
                  CALL wrdisc( iunplt, ibuf2(1,j), nwrds/2 )
                  CALL wrdisc( iunplt, ibuf3(1,j), nwrds/2 )
              ENDIF
          ENDIF
 3100 CONTINUE
      IF( iunplt .NE. 0 ) THEN
c****     Put nibs and the size in columns 68-80 of header card 1
c          WRITE( line1, '(I4,I4,I6)' ) nibs, iwrdcount, ndone
          WRITE( line1, '(I4,I4,I6)' ) nibs, iwrdcount, nsiolines
          CALL podiscb( iunplt, 0, 66 )
          CALL wrdiscb( iunplt, line1, 14 )
      ENDIF
c**** Old color vplot program wants to know the plot size, so stuff it
c**** in the first 8 bytes of the header.
      IF( nibs .EQ. 3444 .AND. iunplt .NE. 0) THEN
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &        CALL swap32( iwrdcount, 2 )
          CALL podisc( iunplt, 1, 0 )
          CALL wrdisc( iunplt, iwrdcount, 2 )                           ! assume ilinecount is right after iwrdcount
      ENDIF
      IF( iunplt .NE. 0 ) CALL FREFIL(2,IUNPLT,ISTAT)                   ! CLOSE (EOF) AND FREE THE PLOT FILE
      RETURN
c
c
 4000 CONTINUE
      IF( tlines(1) .NE. 0 .AND. itlan .NE. 0 ) THEN
          DO i = 1, 4
             IF( tlines(i) .NE. 0 ) temp = tlines(i)
          ENDDO
          IF( rnibs .GT. 250 ) itemp = 350
          itemp = JTABLE(MIDJ)
c****     watch out for buffer overflow
          IF( itemp .GT. 400 ) RETURN
          itemp1 = noff
c****     this is nasty,  time line annotation causes buffer overflow
c****     if too close to the edge, so skip it!
          IF( idir .GE. 0 ) THEN
              IF( itrim .EQ. 1 .OR. itrim .EQ. 2 ) itemp1 = 17
              CALL tlann( ibuf(1,itemp), itemp1, mwrds, vscale, rnibs,
c              CALL tlann( ibuf(1,1), itemp1, mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
              IF( ncolors .NE. 0 ) THEN
                 CALL tlann( ibuf2(1,itemp),itemp1,mwrds, vscale, rnibs,
c                 CALL tlann( ibuf2(1,1),itemp1,mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
                 CALL tlann( ibuf3(1,itemp),itemp1,mwrds, vscale, rnibs,
c                 CALL tlann( ibuf3(1,1),itemp1,mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
              ENDIF
          ELSEIF( rnibs .GT. 250 ) THEN
c              IF( itrim .EQ. 1 .OR. itrim .EQ. 3 ) itemp1 = noff
c              CALL tlann( ibuf(1,itemp),itemp1,mwrds, vscale, rnibs,
              CALL tlann( ibuf(1,1),itemp1,mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
              IF( ncolors .NE. 0 ) THEN
c                 CALL tlann( ibuf2(1,itemp),itemp1,mwrds, vscale, rnibs,
                 CALL tlann( ibuf2(1,1),itemp1,mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
c                 CALL tlann( ibuf3(1,itemp),itemp1,mwrds, vscale, rnibs,
                 CALL tlann( ibuf3(1,1),itemp1,mwrds, vscale, rnibs,
     &           temp, stimel, timel, ncolors * rgb, idir, 1, idecimal )
              ENDIF
          ENDIF
 4010     CONTINUE
          IF( itag .GT. 2 ) THEN
              IF( lunras .NE. 0 .AND. ndone .GT. 40 ) THEN
                  CALL WRDISC ( lunras, ibuf(1,itemp), iwrdcount)
                  ilincount = ilincount + 1
              ENDIF
              IF( iunplt .NE. 0 ) THEN
                  CALL wrdisc( iunplt, ibuf(1,itemp), nwrds/2 )
                  nsiolines = nsiolines + 1
                  IF( ncolors .NE. 0 ) THEN
                      CALL wrdisc( iunplt, ibuf2(1,itemp), nwrds/2 )
                      CALL wrdisc( iunplt, ibuf3(1,itemp), nwrds/2 )
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
      RETURN
c
c
      END
