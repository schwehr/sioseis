      SUBROUTINE geomex( buf, lbuf, ibuf, scr, lscr, iscr, istop )
C     GEOMEX MODIFIES THE SEGY TRACE HEADER FOR THE SHOT AND LINE GEOMETRY.
C  THE SHOT-RECEIVER DISTANCE, CDP NUMBER, AND THE SHOT AND RECEIVER X-COORDINARES
C  ARE COMPUTED FROM THE USER'S PARAMETERS FROM DISC FILE ON FORTRAN UNIT IGUNIT.
C  THE SHOT NUMBER OF THE TRACE IN LBUF IS THE 3RD WORD OF THE HEADER.  THE SHOTS
C  ON TAPE MAY BE SENT TO GEOMEX IN ANY ORDER.
C    THE SHOT NUMBERS ON DISC ARE ASSUMED TO BE IN INCREASING ORDER AND ANY SHOT
C  WHOSE NUMBER IS LESS THAN THE FIRST ON DISC WILL GET THE PARAMETERS FOR
C  THE FIRST.  ANY SHOT WHOSE NUMBER IS GREATER THAN THE LAST ON DISC WILL GET
C  THE SAME AS THE LAST PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  LBUF   - THE TRACE HEADER AND TRACE ARRAY.  THE 32 BIT INTEGER TRACE ARRAY!
C
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1980
C  mod 29 March 1991 to add the type parameter
c  mod 2 Apr 92 by pch and gmk to add LDGO navigation files
c  mod 21 Oct 93 - increase max gxp to 200 (100 pairs)
c   mod 11 Sept 96 - Add bgp (bird trace pairs) and cgp (compass trace pairs)
c                  - add section 13 logic from Digicon trace 0
c                  - add arguments buf, ibuf, scr, lscr, iscr
c  mod 9 Mar 96 - Stop printing source/receiver coordinates
c  mod 30 Apr 97 - Allow negative TYPE
c  mod 22 May 97 - Add DECLIN
c  mod 21 Jul 97 - Add RPADD
c  mod 17 Sep 97 - Increase maxcomps from 20 to 50
c  mod 4 Dec 97 - typo on nbgps when error
c  mod 15 Apr 98 - Add navgeom, compute dfls from a file of fixes.
c  mod 21 Dec 98 - Add EPATH (source and receiver elevations)
c  mod March 1999 - Muck about to get the Digicourse stuff from the new LDEO
c                   Syntron system.  Streamer depth mostly.
c  mod 20 May 99 - Use real ggx AND change ldgogeom for longer
c                  streamer so roundoff doesn't matter.  GMK
c  mod 15 Dec 99 - Check for new LDEO streamer depth location due to rumored
c             Y2K compliance change (3 places with 2 digit year = add 6 bytes)
c  mod 20 Mar 00 - Add ukooain
c  mod 9 Sep 00 - make the water depths in cm rather than m.
c               - Also change getdep to warn rather than error if more
c                 birds are recorded than are defined.
c               - Change max bird depth from 50 to 99 - Dan did it!
c               - make lat/long REAL*8
c               - Add type 9 - compute dfls from long/lat (words 19 & 20)
c               - type -9 is lat/long (words 20 & 19)
c  mod Oct 00 - If GXP was not given get the range from the SEG-Y header.
c               This required NGXPS to be passed to CALCRP.
c             - Make lprint 512 print dfls
c  mod 21 May 01 - Add lunbinary to COMMON /RPCALC/ so calcrp will write
c               the x-coordinates into the SEGY header when elevations are given.
c  mod 23 Sep 02 - made pi real*8, but only used for print of heading
c  mod 29 Sep 02 - When type 9 and lat+long = 0, kill the trace so it doesn't get used.
c  mod 5 Oct 02 - Increase maxbirds to 100 (50 bird-group pairs)
c  mod 21 Mar 03 - Get LDEO stuff from LUNLDEO rather than luntr0.
c  mod 4 Jun 03 - Add TYPE 10 by calling UKOOAIN
c  mod Aug/Sep 03 - Add type 12 (Mladen's cross-offset) - merge the nav
c                  generated in geomed that's in the segy header file.
c  mod Jan 04 - Scrub Mladen's method and implement my "2-D feathered geom"
c  mod 28 Jan 04 - Had to add a status argument to UKOOAIN
c  mod 20 May 04 - save the lat/long in the receiver (words 21 & 22) on siofil.
c  mod 8 Mar 05 - The cross-line offset might be > 32768 decimeters, so make
c                 it meters.
c  mod 31 Mar 05 - Add parameters fbinx, fbiny, lbinx, lbiny, bin_off
c  mod 15 Jun 05 - Honor the (X/Y coordinates) scalar for words 19-22
c                - Add an argument to calcrp3d
c                - Do the feather angle and cross-line offset in REAL*8,
c  mod Aug 10 05 - Add types 14 & 15 for 2005 Healy cruise
c  mod 20 Aug 05 - If Healy returns -1, make sure dfls = 0
c  mod 11 Sep 05 - Type 9, 14 - do dfls correctly when going over the
c                  dateline (-180/180).
c  mod 25 Sep 05 - Add lprint 256
c  mod  1 May 06 - Change lprint 128 to include the range/offset.
c  mod 20 Jul 06 - Add healy06
c                - Add navfil2
c  mod 24 July 06 - Add type 17 - range = distance from origin (first shot)
c  mod 12 Oct 06 - Remove Healy geom - types 14, 15, 16
c                - On type 9, watch out for, and account for, bad fixes
c                - On type 9, watch out for missing fixes.
c  mod 16 Oct 06 - Bad fix logic failed when more than 1 trace because dfls=0
c  mod 16 Jan 07 - Add maxdfls
c  mod 22 Jan 07 - Add mindfls
c  mod 8 Feb 07  - Add setback
c  mod 12 Feb 07 - Add warning when type 9 and nav is not arcseconds.
c  mod 16 May 07 - Bad READ statement when multiple param lists are used.
c  mod 14 Aug 07 - g95 can't divide by a negative (a/-b)
c                - g95 gives error rather than eof on READ
c  mod 27 Nov 07 - Add type 18 - put nav into segy header.
c  mod 5 Mar 08 - Honor SEG-Y Rev 1 coordinates as decimal degrees
c  mod 9 Apr 08 - Initialize ncomps to 0
c  mod 17 Apr 08 - Increase depths, ranges, compasse to 1000 from 500
c  mod 23 Apr 08 - Redo dfls logic for Bad fixes (0,0) and type 9
c  mod 13 May 08 - Multiple lists didn't work.
c  mod 2 June 08 - If gxp(1) is 0, then use the number of traces 
c  mod 27 Aug 09 - Add cknav
c  mod 4 Sep 09 - refine cknav check 
c  mod 7 Sep 09 - add lprint 1024
c  mod ?? Nov 09 - cygwin didn't like an unreachable statement (podisc).
c  mod 14 Sep 10 - Add type 19 to read ukooa and write source lat/long as rael
c  mod 1 Dec 10 - Add type 20
c               - Set rpadd if not given and rp < 0
c  mod 8 Apr 16 - type 9 didn't work on MCS due to 2008 changes
c
c
c type 1 = Missing shots must be explicitly described 
c type 2 = Missing shot points are assumed to occur whenever a shot point
c           number is missing.
c type 3 = LDGO navigation method. CDP
c type 4 = LDGO navigation method. WAP
c type 5 = LDGO navigation method. ESP
c type 6 = SIOSEIS navigation file method
c type 7 = elevation insertion only.
c type 8 = UKOOA
c type 9 = calculates DFLS from the SEGY long/lat (x,y shot
c type -9 = lat/long or (y,x) 
c type 10 = write UKOOA coordinates in SEG-Y header
c type 11 = compute range and rp from SEG-Y header
c type 12 = Mladen's 3-D/cross-dip geom
c type 13 = Compute the feathering angle and cross-line offset
c type 14 = Healy 05 with Geometric's Geode and external SeaBeam depths.
c           Overwrite the SEGY time of shot with the one in the log file.
c type 15 = Healy 05 doing nav and depth only - SEGY time is ok.
c type 16 = Healy 06 - Get nav and depth from SeaBeam or Knudsen file.
c type 17 - Comp[ute the range as distance along the ship track
c type 18 = Read SIO nav file and insert the lat/long into the trace header.
c type 19 = Read UKOOA and insert the source lat/long into segy 19/20 and
c           source and receiver depths
c type 20 = Get gxp & ggx from the first shot in UKOOA , then get dfls from ldeo
c
C****   YOFFB IS NOT IMPLEMENTED!!!!
      EXTERNAL range
      PARAMETER (MAXGXP=200)                                             ! THE LARGEST NUMBER OF GXPS ALLOWED
      PARAMETER (MAXGX=MAXGXP+1)
      PARAMETER (maxbirds=100, maxcomps=50)
      DIMENSION GXP(MAXGX), bgp(maxbirds)
      DIMENSION cgp(maxcomps), icgrpno(maxcomps), creading(maxcomps)
      DIMENSION buf(60), lbuf(60), ibuf(120), 
     &          scr(111), lscr(111), iscr(111)
      COMMON /GEOM/IGUNIT, nglists
      COMMON /RPCALC/ DFLS,DBRPS,SMEAR,type, lunbinary, iwritexy
      INTEGER FS, type, lbuf, lscr, old19, old20
      COMMON /readt/ ilun, numhdr, numdat, iunhdr, ireel, intrcs
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo
      INTEGER*2 ibuf, iscr
      CHARACTER*1000 sect11, sect13
      INTEGER*2 isect13(2), isect11(2)
      EQUIVALENCE (sect13,isect13(1)), (sect11,isect11(1))
      DIMENSION depths(1000), ranges(1000), xbirds(1000), ebuf(2)
      COMMON /SIOLN1/ CBUF
      COMMON /sioln2/ ICHAR1, NCHARS, iprint, lunpo
      CHARACTER*200 CBUF, cbuf4
      COMMON /SIOLN3/ CBUF4
      COMMON /sioln4/ ICHAR4, NCHARS4, iprint4, lunpo4
      REAL lbinx, lbiny, maxdfls, mindfls
      REAL*8 ship_lat, ship_long, oldlat, oldlong, ddlat, ddlong
      REAL*8 deltax, deltay, dtemp, pi
      REAL*8 first_x, first_y, last_x, last_y, azimuth, az, 
     &       sx, sy, rx, ry
      REAL*8 rad, angle, x, y, sinaz, cosaz, mpx, mpy
      REAL*8 d19, d20, d21, d22
      CHARACTER*1 ns, ew
      SAVE
      DATA NTIMES/0/, lastshot/0/, oldlat/0/,oldlong/0/, ddlat/0./
      DATA pi/3.14159265358979/, rad/57.2957795131/, deg/.0174532925199/
C
C
   10 IF( NTIMES .EQ. 0 ) THEN
          NTIMES=1                                                      !  THE PARAMETER LIST NUMBER ON DISC
          nbad = 0
          olddfls = -99999.
          old19 = lbuf(19)
          old20 = lbuf(20)
          last_shot = -1
          last_tr = 9999
          ncomps = 0
          REWIND IGUNIT
          READ(IGUNIT,ERR=9000,END=9000) NGXPS, nbgps, ncgps
          IF( NGXPS .GT. MAXGXP) THEN
   15         PRINT *, ' PGM GEOMEX NEEDS MORE SPACE FOR GXP'
              STOP
          ENDIF
   30     READ(IGUNIT,ERR=9000,END=500) FS,LS,GGX,DFLS,DBRPS,SMEAR,
     &       YOFFA,YOFFB,LPRINT, type, navfil, offset, offset2, ntrcs,
     &       declin, lrpadd, lunbinary, bin_h, fbinx, fbiny, lbinx,
     &       lbiny, bin_off, lunascii, navfil2, maxdfls, mindfls,
     &       setback, cknav
          IF( ngxps .GT. 0 ) THEN
               READ(IGUNIT,ERR=9000,END=9000) (GXP(I),I=1,NGXPS)
               GXP(NGXPS+1)=-1.                                         ! CALCRP DEMANDS GXP TO BE TERMINATED WITH A NEGATIVE
c The following is nice if the user knows the leader length but not the streamer length
               IF( gxp(1) .EQ. 0 ) gxp(1) = intrcs
          ENDIF
          IF( nbgps .GT. 0 ) READ (igunit) (bgp(i),i=1,nbgps)
          IF( ncgps .GT. 0 ) READ (igunit) (cgp(i),i=1,ncgps)
          IF( lunbinary .NE. 0 ) CALL podisc( lunbinary, 0, 0 )
          IF( type .EQ. 13 ) THEN
              bin_h02 = bin_h / 2.
              REWIND navfil
              metric_grid = 1
   40         CONTINUE
c         Get the first shot the user specified or the first in the file
c         Get the X-Y of the first shot.
              CALL rline1( navfil )
              IF( nchars4 .LT. 0 ) GOTO 41
              IF( cbuf4(1:1) .EQ. 'H' ) THEN
                  IF( cbuf4(2:5) .EQ. '2000' )
     &                READ( cbuf4(33:33), '(I1)' ) metric_grid
                  GOTO 40
              ENDIF
              IF( cbuf4(1:1) .NE. 'S' ) GOTO 40
              READ( cbuf4(20:25), '(I6)' ) long_shotno
              IF( metric_grid .EQ. 1 ) THEN
                  READ( cbuf4(47:55), '(F9.1)' ) first_x
                  READ( cbuf4(56:64), '(F9.1)' ) first_y
              ELSE
                  READ( cbuf4(47:55), '(I9)' ) first_x
                  READ( cbuf4(56:64), '(I9)' ) first_y
              ENDIF
              IF( fs .NE. 1 .AND. fs .NE. long_shotno ) GO TO 40
              first_shot = long_shotno
c****         now find out how many traces each UKOOA shot has
c****         We just read one line of receivers.  Assume > 3 traces/shot
              ntrcs = 0
 4060         CONTINUE
              CALL rline1( navfil )
              IF( nchars4 .LT. 0 ) GOTO 41
              IF( cbuf4(1:1) .NE. 'R' ) GOTO 41
              IF( cbuf4(5:5) .NE. ' ' .AND. ICHAR(cbuf4(2:2)) .NE. 0 )
     &            ntrcs = ntrcs + 1
              IF( cbuf4(31:31) .NE. ' ' .AND.ICHAR(cbuf4(28:28)).NE.0)
     &            ntrcs = ntrcs + 1
              IF( cbuf4(57:57) .NE. ' ' .AND.ICHAR(cbuf4(54:54)).NE.0)
     &            ntrcs = ntrcs + 1
              GOTO 4060
   41         CONTINUE
c             Get the last shot specified by the user or the last shot
c             of the file.  Use the X-Y of that shot as the X-Y of the
c             end-of-line.
              IF( cbuf4(1:1) .EQ. 'S' ) THEN
                  READ( cbuf4(20:25), '(I6)' ) long_shotno
                  IF( metric_grid .EQ. 1 ) THEN
                      READ( cbuf4(47:55), '(F9.1)' ) last_x
                      READ( cbuf4(56:64), '(F9.1)' ) last_y
                  ELSE
                      READ( cbuf4(47:55), '(I9)' ) last_x
                      READ( cbuf4(56:64), '(I9)' ) last_y
                  ENDIF
                  IF( ls.NE.999999 .AND. ls.EQ.long_shotno ) GOTO 42
              ENDIF
              CALL rline1( navfil )
              IF( nchars4 .GE. 0 ) GOTO 41
   42         CONTINUE
              az = DATAN2((last_y-first_y),(last_x-first_x))
              azimuth = az * rad
              cosaz = DCOS(-az)
              sinaz = DSIN(-az)
              first_rp_x = 0.
              IF( smear .EQ. 999999. ) smear = dbrps
              IF( IAND(lprint,128) .NE. 0 ) THEN
                  PRINT *,' The print fields are:'
                  PRINT *,' 1.  Shot number or FFID'
                  PRINT *,' 2.  Shot trace number.'
                  PRINT *,' 3.  Range (shot to receiver distance).'
                  PRINT *,' 4.  CDP or bin number.'
                  PRINT *,' 5.  Feathering angle from the shot-line.'
                  PRINT *,' 6.  Midpoint distance from processing line.'
                  PRINT *,'     (midpoint cross-line offset)'
                  PRINT *,' 7.  Source X-coordinate.'
                  PRINT *,' 8.  Source Y-coordinate.'
                  PRINT *,' 9.  Receiver X-coordinate.'
                  PRINT *,' 10.  Receiver Y-coordinate.'
                  PRINT *,' 11.  Mid-point X-coordinate.'
                  PRINT *,' 12.  Mid-point Receiver Y-coordinate.'
                  PRINT *,
     &               ' 13.  Angle between processing line and receiver.'
                  PRINT 503
  503             FORMAT(8X,'1',4X,'2',6X,'3',6X,'4',7X,'5',6X,'6',
     &              11X,'7',11X,'8',11X,'9',10X,'10',10X,'11',
     &              10X,'12',6X,'13')
              ENDIF
          ENDIF
      ENDIF
      IF(LBUF(3).LT.FS.AND.NTIMES.GT.1) THEN    ! START LOOKING FROM THE START
         ntimes = 0
         GOTO 10
      ENDIF
      IF(LBUF(3).LE.LS .OR. (fs .EQ. ls .AND. nglists .EQ. 1)) GO TO 500  ! THE SHOT IS IN THIS LIST
  110 READ(IGUNIT,ERR=9000,END=500) NGXPS, nbgps, ncgps
      IF(NGXPS.GT.MAXGXP) THEN
         ntimes = 0
         GOTO 10
      ENDIF
      READ(IGUNIT,ERR=9000,END=9000) FS,LS,GGX,DFLS,DBRPS,SMEAR,
     &       YOFFA,YOFFB,LPRINT, type, navfil, offset, offset2, ntrcs,
     &       declin, lrpadd, lunbinary, bin_h, fbinx, fbiny, lbinx,
     &       lbiny, bin_off, lunascii, navfil2, maxdfls, mindfls,
     &       setback, cknav
      IF( ngxps .GT. 0 ) THEN
          READ(IGUNIT,ERR=9000,END=9000) (GXP(I),I=1,NGXPS)
          GXP(NGXPS+1)=-1.                                              ! CALCRP DEMANDS GXP TO BE TERMINATED WITH A NEGATIVE
          IF( gxp(1) .EQ. 0 ) gxp(1) = intrcs
      ENDIF
      IF( nbgps .GT. 0 ) READ (igunit) (bgp(i),i=1,nbgps)
      IF( ncgps .GT. 0 ) READ (igunit) (cgp(i),i=1,ncgps)
      NTIMES=NTIMES+1
      IF(LBUF(3).GT.LS) GO TO 110
  500 CONTINUE
      IF( type .EQ. 6 ) THEN
          CALL navgeom( navfil, ibuf, lbuf, dfls )
          lbuf(21) = lbuf(19)
          lbuf(22) = lbuf(20)
          IF( IAND(lprint,4) .NE. 0 ) THEN
              PRINT *,' navgeom dfls =',dfls,
     &            ' gmt ',ibuf(81),ibuf(82),ibuf(83)
c              PRINT *,' navscalar=',ibuf(36),' long=',lbuf(19),
c     &                ' lat=',lbuf(20)
          ENDIF
      ENDIF
      IF( IABS(type) .GE. 3 .AND. IABS(type) .LE. 5 ) THEN
          temp = ntrcs
          closest = range( gxp, ggx, temp )
          ioff = offset + closest
          xbin = dbrps
          ngpsp = IABS(NINT(ggx))
          gpsp = ABS(ggx)
c      print *,' type = ',type,' navfil=',navfil,'  xbin=',xbin,
c     &    ' ioff=',ioff,' xbin=',xbin
c      print *,' ngpsp=',ngpsp,' offs ',offset,offset2,' ntrcs=',ntrcs
          iyear = ibuf(79)
          IF( iyear .LT. 200 ) iyear = iyear + 1900
          IF( iyear .LT. 1999 ) THEN
              CALL ldgogeom( lbuf, lbuf, type, navfil, xbin, ioff,
     &                ntrcs, ngpsp, offset, offset2 )
          ELSE
              CALL ldeogeom( lbuf, lbuf, type, navfil, xbin, ioff,
     &               ntrcs, gpsp, offset, offset2 )
          ENDIF
      ENDIF
      IF( type .EQ. 8 .OR. type .EQ. 10 .OR. type .EQ. 19 .OR.
     &    type .EQ. 20 ) THEN
          IF( type .EQ. 20 ) lbuf(3) = -lbuf(3)  ! fake ukooain to use the first record regardless of shot number
          CALL ukooain( navfil, ibuf, lbuf, buf, istat )
c	print *,' buf=',lbuf(3),lbuf(4),lbuf(10)
          IF( istat .NE. 0 ) THEN
              ibuf(15) = 2                                              ! flag it as dead
              RETURN
          ENDIF
          IF( dbrps .EQ. 0 .OR. gxp(1) .EQ. 0 ) THEN
c             we only come here on the first trace of the job.
              lscr(3) = lbuf(3)
              lscr(4) = lbuf(4) + intrcs - 1   ! watch out for multiple streamers
c             find the trace closest to the source by getting the range at the other end
              CALL ukooain( navfil, iscr, lscr, scr, istat )
c	print *,' scr=',lscr(3),lscr(4),lscr(10)
              IF( IABS(lscr(10)) .GT. IABS(lbuf(10)) ) THEN
c                 trace 1 is closest to the source
                  IF( gxp (1) .EQ. 0 ) THEN
                      gxp(1) = lbuf(4)
                      gxp(2) = -lbuf(10)
                  ENDIF
              ELSE
                  IF( gxp (1) .EQ. 0 ) THEN
                      gxp(1) = lbuf(4) + intrcs -1
                      gxp(2) = -lscr(10)
                  ENDIF
              ENDIF
              IF( ggx .EQ. 0 ) THEN
                  r1 = lbuf(10)
                  dx = 0.
                  DO i = lbuf(4)+1, lbuf(4)+intrcs-1
                     lscr(4) = i
                     CALL ukooain( navfil, iscr, lscr, scr, istat )
                     r2 = lscr(10)
                     dx = dx + ABS(r2-r1)
                     r1 = r2
                  ENDDO
                  dx = dx / FLOAT(intrcs-1)
                  itemp = NINT(dx*1000/3125)
                  ggx = FLOAT(itemp) * 3.125
                  IF( lscr(10) .LT. lbuf(10) ) ggx = -ggx
              ENDIF
              IF( dbrps .EQ. 0 ) dbrps = ABS(ggx) / 2.
              IF( IAND(lprint,4) .NE. 0 ) 
     &            PRINT *,' ggx=',ggx,' dbrps=',dbrps,
     &                    ' gxp=',gxp(1),gxp(2)
              CALL ukooain( navfil, ibuf, lbuf, buf, istat )
          ENDIF
          IF( type .EQ. 20 ) THEN
c             We got the streamer layout from ukooa, now get the trace ranges
              lbuf(3) = -lbuf(3)  ! reset the shot number to the way it was
c             all nav will come from the ldeo header rather than ukooa
              type = 9
              gxp(3) = -1
              ngxps = 2
              DO i = 1, intrcs
                 ranges(i) = range( gxp, ggx, FLOAT(i) )
              ENDDO
          ENDIF
      ENDIF
c****
c****
      IF( ibuf(45) .EQ. 2 .AND. lbuf(19)+lbuf(20) .NE. 0 ) THEN
          d19 = DFLOAT(lbuf(19))
          d20 = DFLOAT(lbuf(20))
          d21 = DFLOAT(lbuf(21))
          d22 = DFLOAT(lbuf(22))
          ship_long = d19 / (60.D0 * 60.D0 )
          ship_lat = d20 / (60.D0 * 60.D0 )
      ENDIF
c****  ibuf(45) is SEG-Y Rev 2 addition of decimal degrees
      IF( ibuf(45) .EQ. 3 .AND. lbuf(19)+lbuf(20) .NE. 0 ) THEN
          ship_long = buf(19)
          ship_lat = buf(20)
      ENDIF
c****
c**** Honor the scalar in word 36 (negative means divisor)
c****
      IF( ibuf(36) .NE. 0 ) THEN
          dtemp = DFLOAT(ibuf(36))
          IF( ibuf(36) .LT. 0 ) dtemp = -1. / dtemp
          ship_lat = ship_lat * dtemp
          ship_long = ship_long * dtemp
      ENDIF
      IF( IAND(lprint,16) .NE. 0 ) PRINT *,' ship_lat = ',ship_lat,
     &    ' ship_long = ',ship_long
      IF( type .EQ. 14 .OR. type .EQ. 15 .OR. type .EQ. 16 ) THEN
          PRINT *,' *****      Healy 2005 and 2006 geom removed.  *****'
          CALL EXIT
c****   14, 16 = do nav and water bottom depth
c****   15 = water bottom only
c      IF( type .EQ. 14 .OR. type .EQ. 15 )
c     &    CALL healy05( type, navfil, lunascii, buf, ibuf, lbuf, istat )
c      IF( type .EQ. 16 )
c     &    CALL healy06( navfil, navfil2, buf, ibuf, lbuf, istat )
c          IF( istat .LT. 0 .OR. lbuf(3) .EQ. 0 ) THEN
cc              nbad = nbad + 1
cc              olddfls = dfls
c****       the next good nav will correct the shot spacing correctly
c              ibuf(15) = 2
c              dfls = 0
c              IF( lbuf(4) .EQ. 1 )
c     &        PRINT *,' ***  WARNING  ***  Bad GPS on shot ',lbuf(3),
c     &          ' FFID ',lbuf(5),', killing shot and set dfls to 0.'
c          ENDIF
      ENDIF
c****
c**** Types 9, 14, 16 GEOM calculate DFLS from the SEGY header lat/long
c**** The intention here is that the realtime GPS is good.  There's
c**** no need to do the lat/long work unless it is type 8 geom.
c**** ibuf(36) is the "scalar to be applied to the coordinates in
c**** bytes 73-88 (lbuf(19), lbuf(20), lbuf(21), lbuf(22))
c**** lbuf(19) is "longitude in seconds of arc"
c**** lbuf(20) is "latitude in seconds of arc"
c**** In the LDEO case, assume SEGDIN filled lat/long.
c**** Calcrp writes over bytes 73-88 (lbuf(19), lbuf(20), lbuf(21), lbuf(22))
c**** IF the lat/long = 0, we can't do anything, so give up and don't any
c****   more damage by calculating bogus geometry.
      IF( type .EQ. 9 .OR. type .EQ. -9 .OR. type .EQ. 14 .OR. 
     &    type .EQ. 16 .OR. cknav .NE. 99999. ) THEN
c****     Some GPS units give lat and long of 0 when they fail.
c****     Use the last dfls when that happens and keep track of how many
c****     consecutive are bad so when the next good fix we can subtract out
c****     the bad distances and place the good one in the right place.
          IF( lbuf(19)+lbuf(20) .EQ. 0 .AND. olddfls .NE. -99999. .AND.
     &      lbuf(3) .NE. last_shot .AND. lbuf(4) .LE. last_tr ) THEN
              nbad = nbad + 1
              PRINT *,' ***  WARNING  ***  Bad position, using DFLS ',
     &            dfls
              lbuf(19) = old19
              lbuf(20) = old20
              dfls = olddfls
              ship_lat = oldlat
              ship_long = oldlong
          ENDIF
c****     ibuf(45) is coordinates units; 2 means arcseconds
c****     ibuf(36) is coordinates scalar; < 0 means use as a divisor
          IF( ibuf(45) .NE. 2 .AND. ibuf(45) .NE. 3 .AND.
     &        lbuf(4) .LE. last_tr .AND. lbuf(19)+lbuf(20) .NE. 0 ) THEN
             PRINT *,' ***  WARNING  ***  GEOM TYPE 9 requires NAV to be
     & decimal degrees or in arcseconds.'
             PRINT *,' SEG-Y header word 45 must be 2 or 3 - it is ',
     &           ibuf(45)
             PRINT *,' Using DFLS ',dfls
          ENDIF
          CALL dlendeg( ship_lat,ddlat,ddlong)
          dfls = 1.
c****  calcrp looks to see if it's the same shot as the last, so when
c****  subsequent traces of the same shot have dfls 0, it's no big deal.
          IF( oldlat .NE. 0. .AND. lbuf(19)+lbuf(20) .NE. 0 .AND.
     &      lbuf(3) .NE. last_shot ) THEN
              deltay = (ship_lat-oldlat)*ddlat
              deltax = (ship_long-oldlong)*ddlong
              IF( SIGN(1.D0,ship_long) .NE. SIGN(1.D0,oldlong)) THEN
                  IF( ship_long .LT. 0. ) THEN
                      deltax = (ship_long-oldlong+360.D0)*ddlong
                  ELSE
                      deltax = (ship_long-oldlong-360.D0)*ddlong
                  ENDIF
              ENDIF
              dfls = DSQRT(deltax*deltax+deltay*deltay)
              IF( nbad .GT. 0 ) THEN
                  dfls = dfls - olddfls * nbad
                  nbad = 0
              ENDIF
              IF( deltax .NE. 0. .AND. deltay .NE. 0. ) THEN
c     &            heading = DATAN(deltay/deltax) * 180.D0 / pi
                  heading = DATAN2(deltay,deltax) * rad
                  IF( deltax .LT. 0 .AND. deltay .GE. 0 ) THEN
                      heading = 450.D0 - heading
                  ELSE
                      heading = 90.D0 - heading
                  ENDIF
              ENDIF
              IF( IAND(lprint,2) .NE. 0 ) THEN
                   print *,' ship_long ',ship_long,oldlong,deltax,ddlong
                   print *,' ship_lat ',ship_lat,oldlat,deltay,ddlat
                   temp = ship_lat * 60.D0 * 60.D0
                   CALL secsdms( 1, temp, itemp, itemp1, temp1 )
                   temp = FLOAT(itemp1) + temp1/60.
                   PRINT *,' SEG-Y header lat ',itemp, temp
                   temp = ship_long * 60.D0 * 60.D0
                   CALL secsdms( 1, temp, itemp, itemp1, temp1 )
                   temp = FLOAT(itemp1) + temp1/60.
                   PRINT *,' SEG-Y header long ',itemp, temp
              ENDIF
              IF( IAND(lprint,1024) .NE. 0 ) THEN
                   PRINT *,' dfls=',dfls,' heading=',heading
              ENDIF
              IF( cknav .NE. 99999. ) THEN
                  IF( dfls .GT. cknav ) THEN
                      PRINT *,' ***  WARNING  *** Bad nav fix of ',
     &                 ship_lat,ship_long,' using last good fix.  shot',
     &                 lbuf(3)
                      lbuf(19) = old19
                      lbuf(20) = old20
                      dfls = olddfls
                      ship_lat = oldlat
                      ship_long = oldlong
                  ENDIF
                  GOTO 501
              ENDIF
              IF( dfls .GT. 0 .AND. dfls .LT. 5. ) PRINT *,
     & ' ***  WARNING  ***  Unusually small distance from last shot of',
     &            dfls
              IF( ABS(dfls) .GT. maxdfls .OR.  
     &            ABS(dfls) .LT. mindfls ) THEN
                  nbad = nbad + 1
                  dfls = olddfls
                  PRINT *,
     &                ' ***  WARNING  ***  Bad position, using DFLS ',
     &            dfls
              ENDIF
  501         CONTINUE
              IF( lbuf(4) .LE. last_tr ) olddfls = dfls
          ENDIF
          oldlat = ship_lat
          oldlong = ship_long
          last_shot = lbuf(3)
          last_tr = lbuf(4)
          old19 = lbuf(19)
          old20 = lbuf(20)
      ENDIF
      IF( type .EQ. 13 ) THEN                                           ! feathering
c****     translate the axis so the first shot is the origin
           x = d19 - first_x
           y = d20 - first_y
c****      rotate the axis so the shot line is now the x axis
           sx = x*cosaz - y*sinaz
           sy =  x*sinaz + y*cosaz
c          now do the receiver
           x = d21 - first_x
           y = d22 - first_y
           rx = x*cosaz - y*sinaz
           ry =  x*sinaz + y*cosaz
           deltax = sx - rx
           deltay = sy - ry
c          feather angle is the receiver from the shot-line
           angle = DATAN2(deltay,deltax) * rad
           IF( deltay .LT. 0 .AND. deltax .GE. 0 ) THEN
               angle = 450.D0 - angle
           ELSE
               angle = 90.D0 - angle
           ENDIF
           ibuf(48) = NINT(angle*10.)                                   ! feathering angle in deci-degrees
           dtemp = DSQRT(deltax*deltax+deltay*deltay)
           ltemp = NINT(dtemp)
           IF( lbuf(10) .NE. 0 .AND. IABS(lbuf(10)-ltemp) .GT. 5 )
     &         print *,'  hum, new range=',ltemp,' old=',lbuf(10)
           lbuf(10) = ltemp
           IF( fbinx+fbiny+lbinx+lbiny .EQ. 0 ) THEN
c****          The bin-center-line is parallel to the shot-line.
               mpx = (sx + rx) / 2.
               mpy = (sy + ry) / 2.
c              make them use process header rather than write them here!
               IF( mpy .GT. bin_off + bin_ho2 ) ibuf(15) = 2
               IF( mpy .LT. bin_off - bin_ho2 ) ibuf(15) = 2
               IF( first_rp_x .EQ. 0. ) first_rp_x = mpx
               itemp = NINT((mpx-first_rp_x ) / dbrps)
               dtemp = itemp * dbrps
               lbuf(6) = 0
               IF( (mpx-first_rp_x) .GE. dtemp-smear/2. .AND. 
     &             (mpx-first_rp_x) .LE. dtemp+smear/2.)
     &             lbuf(6) = itemp + 1
               ibuf(49) = NINT(deltay)                                  ! crossline offset in meters
c****      watch out for ibuf being 16 bit integer
               IF( IAND(lprint,128) .NE. 0 ) THEN
                   dtemp = (d19+d21)/2.D0
                   dtemp1 = (d20+d22)/2.D0
                   PRINT 502, lbuf(3), lbuf(4), lbuf(10),lbuf(6)+lrpadd,
     &                angle, -mpy, d19, d20, d21, d22, dtemp, dtemp1,
     &                angle
  502              FORMAT
     &             (1X,I8,2X,I3,2(2X,I5),2X,F6.1,2X,F5.0,2X,
     &             6(1X,F11.1),2X,F6.1)
               ENDIF
           ELSE
               CALL calcrp3d( ibuf, lbuf, fbinx, fbiny, lbinx, lbiny,
     &              bin_h, dbrps, lprint, lrpadd )
           ENDIF
      ENDIF
      IF( type .EQ. 18 ) CALL sionav2segy( navfil, lbuf, ibuf )
      IF( type .EQ. 1 .OR. type .EQ. 2 .OR. type .EQ. 6 .OR.
     &    type .EQ. 9 .OR. type .EQ. 14 .OR. type .EQ. 16 .OR. 
     &    type .EQ. 17 .OR. type .EQ. 18 ) THEN
          IF( dfls .GT. 1000. .OR. dfls .LT. -1000. )
     &        PRINT *,' ***  WARNING  ***  Unusual DFLS of ',dfls
          IF( ngxps .GT. 0 ) CALL CALCRP(LBUF,GXP,GGX, ngxps )
          IF(YOFFA.NE.0.) LBUF(10)=SQRT(LBUF(10)*LBUF(10)+YOFFA*YOFFA)
          IF( setback .GE. 0 ) CALL rpxy( lbuf, ibuf, setback )
c****     512 + 256 = 768
          IF( IAND(lprint,768) .NE. 0 .AND. lbuf(4) .EQ. 1 ) THEN
c****  gfortran doesn't like FLOAT(integer*2), so don't do explicit float.
              scalar = (ibuf(36))
              temp = FLOAT(lbuf(20))
              IF( scalar .GT. 0 ) temp = temp * scalar
              IF( scalar .LT. 0 ) temp = temp / ABS(scalar)
              CALL secsdms( 1, temp, latdeg, latmin, seclat )
              temp = FLOAT(lbuf(19))
              IF( scalar .GT. 0 ) temp = temp * scalar
              IF( scalar .LT. 0 ) temp = temp / ABS(scalar)
              CALL secsdms( 1, temp, longdeg, longmin, seclong )
              IF( IAND(lprint,512) .NE. 0 ) PRINT 505, lbuf(3), dfls,
     &            latdeg, latmin, seclat, longdeg, longmin, seclong
  505         FORMAT(' Shot:',I8,' dfls: ',F8.2,
     &           ' lat: ', I4,1x,I2,1x,F8.5,' long: ',I4,1x,I2,1x,F8.5)
              IF( IAND(lprint,256) .NE. 0 ) THEN
                  IF( latdeg .LT. 0 ) THEN
                      ns = 'S'
                      itemp = -latdeg
                  ELSE
                      ns = 'N'
                      itemp = latdeg
                  ENDIF
                  IF( longdeg .LT. 0 ) THEN
                      ew = 'W'
                      itemp1 = -longdeg
                  ELSE
                      ew = 'E'
                      itemp1 = longdeg
                  ENDIF
                  IF( ibuf(79) .LT. 20 ) ibuf(79) = ibuf(79) + 2000
                  PRINT 506, lbuf(3), 
     &            ibuf(79), ibuf(80), ibuf(81), ibuf(82), ibuf(83), ns,
     &            itemp, latmin, seclat, ew, itemp1, longmin, seclong,
     &            lbuf(16)
  506         FORMAT(1x,I6.6,1x,I4,'+',I3.3,':',I2.2,':',I2.2,':',I2.2,
     &               1x,A1,1x,I2.2,1x,I2.2,1x,F6.3, 
     &               1x,A1,1x,I3.3,1x,I2.2,1x,F6.3,
     &               1x,I4 )
              ENDIF
          ENDIF
      ENDIF
      IF( type .EQ. 11 .OR. type .EQ. 17 ) THEN
          CALL segyxy( type, buf, lbuf, ibuf, lprint )
          IF( istop .LT. 0 ) lbuf(51) = -1
      ENDIF
      IF( lrpadd .EQ. 0 .AND. lbuf(6) .LE. 0 ) lrpadd = -lbuf(6) + 1001
      IF( lrpadd .NE. 0 ) lbuf(6) = lbuf(6) + lrpadd
      IF( IAND(LPRINT,2) .NE. 0 ) PRINT 510,LBUF(3),LBUF(4),
     *        LBUF(10),LBUF(6)
  510     FORMAT(' SHOT',I9,' TRACE',I9,' HAS RANGE',I9,', CDP ',I9)
      IF( lunldeo .NE. 0 .AND. lastshot .NE. lbuf(3) ) THEN
          lastshot = lbuf(3)
          DO i = 1, intrcs
             ranges(i) = range( gxp, ggx, FLOAT(i) )
          ENDDO
          DO i = 1, nbgps/2
             xbirds(i) = ranges(IFIX(bgp(i*2)))
          ENDDO
          IF( IAND(lprint,4) .NE. 0 ) THEN
              PRINT *,' ranges=',(ranges(i),i=1,5)
              PRINT *,' xbirds=',(xbirds(i),i=1,5)
              PRINT *,' nbirds=',nbgps/2,' intrcs=',intrcs
          ENDIF
c****     see if it's the old Digicon DMS or "new" Syntron
          CALL podiscb( lunldeo, 0, 0 )
          CALL rddiscb( lunldeo, isect11, 6, istat )
          IF( sect11(1:2) .EQ. '$1' ) THEN
c****      The ldeo external header was done in segddin
              READ( sect11, '(2x,I4)') nbytes
              CALL podiscb( lunldeo, 0, 0 )
              CALL rddiscb( lunldeo, isect11, nbytes, istat )
              CALL rddiscb( lunldeo, isect13, 500, istat )
              IF( nbgps .GT. 0 ) THEN
                  istat = getdep( isect13(1),
     &                xbirds, nbgps/2, ranges, intrcs, depths,
     &               ncomps, icgrpno, creading )
                  IF( ncgps .GT. 0 ) THEN
                      DO i = 1, ncomps
                         DO j = 1, ncgps, 2
                            IF( icgrpno(i) .EQ. NINT(cgp(j)) )
     &                          icgrpno(i) = NINT(cgp(j+1))
                         ENDDO
                      ENDDO
                  ENDIF
              ELSE
                  istat = -1
              ENDIF
              GOTO 12345
c*****              CALL podiscb( lunldeo, 0, 0 )
          ENDIF
c****     get the Digiscan/Digicouse  birds for streamer depth and compasses
c****     This is the old section 13 and now follows the LDEO ASCII record
          CALL podiscb( lunldeo, 0, 0 )
          CALL rddiscb( lunldeo, iscr, 3940, istat )
          index11 = 0
          index13 = 0
          j = 13
          DO i = 1, 15
             IF( iscr(j) .EQ. 11 .AND. iscr(j+1) .EQ. 214 ) index11 = j
             IF( iscr(j) .EQ. 13 ) index13 = j
             j = j + iscr(j+1)/2
          ENDDO
c****     If no section 11, then it's the Syntron which starts out $1,I4 length
          IF( index11 .EQ. 0 ) index11 = index11 + 50
c****         On the new LDEO Syntron, I put the LDEO ASCII stuff in luntr0
          IF( lastshot .NE. lbuf(3) ) THEN
              DO i = 1, 250
	            isect11(i) = iscr(index11+i-1)
	            isect13(i) = iscr(index+i-1)
              ENDDO
c             section 11 is LDEO ASCII header,  section 13 is the Digicourse birds
              IF( IAND(lprint,4) .NE. 0 .AND. index11 .NE. 0 ) THEN
                  PRINT *,' section 11= ',isect11(1),isect11(2),
     &               sect11(5:84)
                  PRINT *,' ',sect11(85:164)
                  PRINT *,' ',sect11(165:244)
                  PRINT *,' ',sect11(245:324)
                  PRINT *,' ',sect11(325:404)
                  PRINT *,' ',sect11(405:484)
              ENDIF
              IF( IAND(lprint,4) .NE. 0 .AND. index13 .NE. 0 ) THEN
                  PRINT *,' section 13= ',isect13(1),isect13(2),
     &                sect13(5:84)
                  PRINT *,' ',sect13(165:244)
                  PRINT *,' ',sect13(85:164)
              ENDIF
c****  sh!@#$%^.  the ldeo extended header stuff MIGHT change so that it's
c****  Y2K compliant.  There are three 2 digit dates which presumably will
c****  become 4 digits, so if we see one change, assume all three changed!
c****  ldgo_tr0 does it ok, but the location of the depths changes by 6 bytes
              IF( index13 .GT. 0 .AND. sect13(9:9) .EQ. ':' ) 
     &            index13 = index13 + 3
              IF( index13 .GT. 0 .AND. sect13(7:7) .EQ. ':' ) THEN
                  IF( nbgps .GT. 0 ) istat = getdep( iscr(index13),
     &                xbirds, nbgps/2, ranges, intrcs, depths, 
     &               ncomps, icgrpno, creading )
                  IF( ncgps .GT. 0 ) THEN
                      DO i = 1, ncomps
                         DO j = 1, ncgps, 2
                            IF( icgrpno(i) .EQ. NINT(cgp(j)) ) 
     &                          icgrpno(i) = NINT(cgp(j+1))
                         ENDDO
                      ENDDO
                  ENDIF
              ELSE
                  istat = -1
              ENDIF
c              PRINT *,' 13=', sect13(5:12), '11=',sect11(49:77)
c              IF( nbgps .GT. 0 .AND. istat .NE. 0 ) 
c     &            PRINT *,' Bird depth calculation failed on shot ',
c     &               lbuf(3)
          ENDIF
12345     CONTINUE
          IF( declin .NE. 0. ) THEN
              DO i = 1, ncomps
                 creading(i) = creading(i) + declin
              ENDDO
          ENDIF
          IF( IAND(lprint,8) .NE. 0 ) THEN
              PRINT *,' depths=',(depths(i),i=1,10)
              IF( ncomps .GT. 0 ) PRINT *,' compasses: ',
     &            (icgrpno(i),creading(i),i=1,ncomps)
          ENDIF
          IF( depths(lbuf(4)) .LT. 0 .OR. depths(lbuf(4)) .GT. 99 ) THEN
              PRINT *,' Bad bird depth of ',depths(lbuf(4)),' on shot ',
     &             lbuf(3),' trace ',lbuf(4)
          ENDIF
          lbuf(11) = -NINT(depths(lbuf(4))*100.)
      ENDIF
      IF( lunbinary .NE. 0 .AND. type .NE. 12 .AND. type .NE. 14 .AND.
     &    type .NE. 15 .AND. type .NE. 16 ) THEN
          ndones = 0
          ndoner = 0
          CALL podisc( lunbinary, 0, 0 )
          CALL rddisc( lunbinary, ebuf, 2, istat )
          location1 = ebuf(1)
          elevation1 = ebuf(2)
          lbuf(11) = elevation1
          lbuf(12) = elevation1
          sourcex = FLOAT(lbuf(19))
          receiverx = FLOAT(lbuf(21))
 5000     CALL rddisc( lunbinary, ebuf, 2, istat )
          IF( istat .LT. 0 ) THEN
              IF( ndones .EQ. 0 ) lbuf(12) = elevation1
              IF( ndoner .EQ. 0 ) lbuf(11) = elevation1
              GOTO 5050
          ENDIF
          location2 = ebuf(1)
          elevation2 = ebuf(2)
          IF( sourcex .LT. location1 .AND. 
     &        receiverx .LT. location1 ) RETURN
          IF( sourcex .GE. location1 .AND.
     &        sourcex .LT. location2 ) THEN
              temp = (sourcex - location1 ) /
     &               (location2 - location1 ) *
     &               (elevation2 - elevation1) + elevation1
              lbuf(12) = NINT(temp)
              ndones = 1
          ENDIF
          IF( receiverx .GE. location1 .AND.
     &        receiverx .LT. location2 ) THEN
              temp = (receiverx - location1 ) /
     &               (location2 - location1 ) *
     &               (elevation2 - elevation1) + elevation1
              lbuf(11) = NINT(temp)
              ndoner = 1
          ENDIF
          location1 = location2
          elevation1 = elevation2
          IF( ndones .EQ. 0 .OR. ndoner .EQ. 0 ) GOTO 5000
 5050     IF( lbuf(11) .EQ. 0 .OR. lbuf(12) .EQ. 0 ) THEN
              PRINT *,' ***  WARNING  ***  Possible bad elevation at ',
     &         lbuf(19),' or ',lbuf(21)
          ENDIF
          IF( IAND(lprint,32) .NE. 0 ) THEN
              PRINT *,' location: source ',lbuf(19),' receiver',lbuf(21)
             PRINT *,' elevation: source ',lbuf(12),' receiver',lbuf(11)
          ENDIF
      ENDIF
      RETURN
c****
 9000 PRINT *,' *** BOMB  - GEOM param file error.'
      STOP
      END
