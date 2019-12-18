      SUBROUTINE GEOMED(BUF,LBUF)
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c  mod 29 March 1991 by pch to add type 2 geometry
c  mod 22 Apr 92 by pch and gmk to add LDGO navigation file processing.
c         I.E. add navfil, offset and offset2
c  mod 9 Mar. 94 - Allow INTEGER user parameters to be super big
c                  (CALL lcode rather tha dcode on integer parameters).
c  mod 26 jul 94 - Change SMEAR preset to be dbrps
c  mod 11 Sept 96 - Add bgp (bird trace pairs) and cgp (compass trace pairs)
c  mod 30 Apr 97 - Allow negative TYPE.
c  mod 22 May 97 - Add parameter DECLIN, the magnetic declination
c  mod 21 Jul 97 - Add parameter RPADD
c  mod 12 Apr 98 - Add SIOSEIS navigation file for getting DFLS
c  mod 21 Dec 98 - Add EPATH (source and receiver elevations)
c  mod 16 Sep 99 - Convert the ASCII elevation file to binary.
c                - Add type 7 to be elevation insertion only.
c  mod 3 Mar 00 - Preset lunbinary to 0 (a scratch file for elevations).
c  mod 20 Mar 00 - Add type 8 - UKOOA navfil
c                - set dbrps to 0
c  mod 5 Apr 00 - LS preset was incorrect.  Now LS defaults to FS.
c  mod 24 Apr 00 - UKOOA navfil change blew opening of sionav on SGIs
c                  since the sionav files were double opened.
c  mod Sep 00 - Type 9 calculates DFLS from the SEGY long/lat (x,y shot coordinates)
c             - Allow -9 to be lat/long or (y,x) since I f**up in segdin for awhile
c  mod Oct 00 - Make GXP missing into a warning rather than an error, and
c               get the ranges from the SEG-Y header!
c  mod 21 May 03 - Add parameter WRITEXY
c  mod 4 Jun 03 - Add TYPE 10 (write UKOOA coordinates in SEG-Y header).
c  mod 5 Aug 03 - Add TYPE 11 (compute range and rp from SEG-Y header)
c  mod 26 Aug 03 - Add TYPE 12 (Mladen's 3-D/cross-dip geom).
c                - Add param bin_h
c  mod 27 Oct 03 - Add HPATH for SEG-Y header file.  When UKOOA is also
c                  given, create the file.  If no UKOOA, just use hpath.
c  mod 14 Jan 05 - Bad error check when navfil and types 3,4,5
c  mod 30 Mar 05 - Add parameters fbinx, fbiny, lbinx, lbiny, bin_off
c  mod August 05 - Add type 14 for Healy0503
c                - Use EPATH for Healy centerbeam file.
c  mod 20 Jul 06 - Add type 16 for hly0602
c               - Add navfil2
c  mod 24 Jul 06 - Add type 17 (range = word 10 = distance from origin
c  mod 16 Jan 07 - Add maxdfls
c  mod 22 Jan 07 - Add mindfls
c  mod 8 Feb 07  - Add setback
c  mod 16 May 07 - Use the same GXP & NGXPS on successive lists unless new
c                  one is given.
c  mod 27 Nov 07 - Allow type 18
c  mod 3 Jan 08  - Error gracefully if NAVFIL doesn't exist.
c  mod 5 Mar 08 - Above was badly done
c  mod 13 May 08 - geomex needs to know if multiple lists (add nglists)
c  mod 2 June 08 - Preset DBRPS to ggx/2
c  mod 27 Aug 09 - Add CKNAV
c  mod 1 Jun 10 - Add warning if DFLS not given and type 1, 2, or 6
c  mod 14 Sep 10 - Allow type 19
c  mod 29 Nov 10 - change ggx preset to 0
c                - Allow dbrps not given when type 10 (ukooa)
c                - Allow dfls not given when type 10 (ukooa)
c                - Set gxp to 0, yet given if navfil is given.
c                - Allow type 20
c  mod 23 Sep 11 - Don't print DFLS not given warning on types 9 or 19
c  mod 23 May 12 - Allow dfls not given when type 8 (ukooa)
C
      PARAMETER (NPARS=33)                                              ! THE NUMBER OF USER PARAMETERS
      PARAMETER (MULTI=31)                                              ! POINT TO THE FIRST MULTI-INPUT PARAMETER
      PARAMETER (MAXGXP=200)                                            ! THE LARGEST NUMBER OF GXPS ALLOWED
      PARAMETER (maxbirds=50)
      CHARACTER*8 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      COMMON /epath/epath
      CHARACTER*110 TOKEN, navpath, hpath, epath, navpath2
      DIMENSION VALS(NPARS),LVALS(NPARS)
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /GEOM/ IGUNIT, nglists
      COMMON /RPCALC/ dummy(5), iwritexy
      COMMON /SIOLN1/ CBUF
      COMMON /sioln2/ ICHAR1, NCHARS1, iprint, lunpo
      CHARACTER*200 CBUF
      COMMON /SIOLN3/ CBUF4
      COMMON /sioln4/ ICHAR4, NCHARS4, iprint4, lunpo4
      CHARACTER*200 CBUF4
      COMMON /SEGY/ lhead(60)
      INTEGER*4 lhead
      DIMENSION ihead(120)
      EQUIVALENCE (lhead(1),ihead(1))
      INTEGER*2 ihead
      DIMENSION BUF(111),LBUF(111)
      REAL lbinx, lbiny, maxdfls, mindfls
      INTEGER FS, type, rpadd
C
C
      EQUIVALENCE (FS,LVALS(1)),
     2            (LS,LVALS(2)),
     3            (GGX,VALS(3)),
     4            (DFLS,VALS(4)),
     5            (DBRPS,VALS(5)),
     6            (SMEAR,VALS(6)),
     7            (LPRINT,LVALS(7)),
     8            (YOFFA,VALS(8)),
     9            (YOFFB,VALS(9)),
     *            (type,lvals(10)),
     1            (navfil,lvals(11)),
     2            (offset,vals(12)),
     3            (offset2,vals(13)),
     4            (ntrcs,lvals(14)),
     5            (declin,vals(15)),
     6            (rpadd,lvals(16)),
c     7            (epath,vals(17)),
     8            (writexy,lvals(18)),
     9            (bin_h, vals(19)),
c    *            hpath is done differently
     1            (fbinx,vals(21)),
     2            (fbiny,vals(22)),
     3            (lbinx,vals(23)),
     4            (lbiny,vals(24)),
     5            (bin_off,vals(25)),
     6            (navfil2,lvals(26)),
     7            (maxdfls,vals(27)),
     8            (mindfls,vals(28)),
     9            (setback,vals(29)),
     *            (cknav,vals(30)),
     7            (bgp,vals(31)),
     8            (cgp,vals(32)),
     9            (GXP,VALS(NPARS))                                     ! GXP MUST BE THE LAST ONE!!!!!!
      DATA NAMES/'FS    ','LS    ','GGX   ','DFLS  ','DBRPS ',
     *           'SMEAR ','LPRINT','YOFFA ','YOFFB ','TYPE  ',
     &           'NAVFIL','OFFSET','OFFSET2','NTRCS ','DECLIN',
     &           'RPADD ','EPATH ','WRITEXY','BIN_H ','HPATH ',
     &           'FBINX ','FBINY ','LBINX ','LBINY ','BIN_OFF',
     &           'NAVFIL2','MAXDFLS','MINDFLS','SETBACK','CKNAV',
     &           'BGP   ','CGP   ','GXP   '/
      DATA LENGTH/2,2,3,4,5,5,6,5,5,4,6,6,7,5,6,5,5,7,5,5,5*5,4*7,
     &     5,3*3/
      DATA TYPES/'L','L',4*'F','L',2*'F','L','A','F','F','L','F','L',
     &         'A','A','F','A',4*'F','F','A',7*'F'/
      DATA pi/3.14159265358979/, rad/57.2957795131/, deg/.0174532925199/
C
C****      SET THE PRESETS
C****
      ns = 0
      nglists = 0
      FS=1
      LS=999999
      GGX = 0
      DFLS=150.
      DBRPS=0.
      SMEAR = 999999.
      YOFFA=0.
      YOFFB=0.
      LPRINT=0
      LLS=0
      NLISTS=0
      NGXPS=0
      ngxps1 = 0
      type = 2
      navfil = 0
      navfil2 = 0
      offset = 0.
      offset2 = 0.
      ntrcs = 0
      declin = 0.
      rpadd = 0
      lunascii = 0
      lunbinary = 0
      iwritexy = 0
      nbgps = 0
      ncgps = 0
      bin_h = 9999999.
      navpath = ' '
      navpath2 = ' '
      hpath = ' '
      fbinx = 0.
      fbiny = 0.
      lbinx = 0.
      lbiny = 0.
      bin_off = 0.
      maxdfls = 10000.
      mindfls = 0.
      setback = -99999.
      cknav = 99999.
      CALL GETFIL(2,IGUNIT,token,ISTAT)                                 ! GET A FILE UNIT
      IF(ICOMPT.EQ.1) IGUNIT=IGUNIT+4
      OPEN(UNIT=IGUNIT,STATUS='SCRATCH',FORM='UNFORMATTED',
     *     ACCESS='SEQUENTIAL')
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                         ! CONVERT THE TOKEN TO UPPERCASE
      IF( NCHARS .LE. 0 ) THEN
          IF( NOW .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GOTO 100
      ENDIF
  150 NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
         LEN=LENGTH(I)                                                  ! GET THE LEGAL PARAMETER NAME LENGTH
         IPARAM=I                                                       ! SAVE THE INDEX
         IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN)
     &       GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF( ns .NE. 0 ) GOTO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** GEOM DOES NOT HAVE A PARAMETER ',
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
      IF( NCHARS .LE. 0 ) THEN
          IF( NOW .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GOTO 210
      ENDIF
      ns = 0
  230 CONTINUE
      IF( TYPES(NPARAM) .EQ. 'A' ) THEN
          IF( names(nparam) .EQ. 'NAVFIL' ) navpath = token
          IF( names(nparam) .EQ. 'NAVFIL2' ) THEN
              navpath2 = token
              CALL getfil( 2, navfil2, token, istat )
              OPEN( UNIT=navfil2, FILE=navpath2, STATUS='OLD' )
              CLOSE (navfil2)
              CALL frefil( 1, navfil2, istat )
          ENDIF
          IF( names(nparam) .EQ. 'EPATH' ) THEN
              CALL getfil( 2, lunascii, token, istat )                  ! for the ASCII elevation file
              OPEN( UNIT=lunascii, FILE=token, STATUS='OLD' )
              epath = token
              CALL getfil( 1, lunbinary, token, istat )                  ! get a scratch file
          ENDIF
          IF( names(nparam) .EQ. 'HPATH' ) hpath = token
          IF( names(nparam) .EQ. 'WRITEXY' ) THEN
              CALL upcase( token, nchars )
              IF( token(1:3) .EQ. 'YES' ) iwritexy = 1
          ENDIF
          GOTO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
      IF( TYPES(NPARAM) .EQ. 'L' ) THEN
          CALL LCODE(TOKEN,NCHARS, LVALS(NPARAM), istat )
      ELSE
          IF( NPARAM .GE. MULTI ) THEN                                  !  IS IT A MULTIVALUED PARAMETER
              ns = ns + 1
              IF( names(nparam) .EQ. 'GXP' ) THEN
                  IF( ngxps1 .NE. 0 ) THEN
                      ngxps = 0
                      ngxps1 = 0
                  ENDIF
                  NGXPS = NGXPS + 1
                  CALL DCODE(TOKEN,NCHARS,BUF(NGXPS),ISTAT)
              ENDIF
              IF( names(nparam) .EQ. 'BGP' ) THEN
                  nbgps = nbgps + 1
                  CALL dcode( token, nchars, buf(maxgxp+nbgps), istat )
              ENDIF
              IF( names(nparam) .EQ. 'CGP' ) THEN
                  ncgps = ncgps + 1
                  CALL dcode( token, nchars,buf(maxgxp+maxbirds+ncgps),
     &                   istat)
              ENDIF
          ELSE
              CALL DCODE(TOKEN,NCHARS, VALS(NPARAM), istat )
          ENDIF
      ENDIF
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
      IF( ngxps .EQ. 0 ) THEN
          buf(1) = 0
c         type 17 is distance along the ship track
          IF( type .EQ. 17 ) buf(1) = 1
          buf(2) = 0
          IF( type .EQ. 17 .OR. navfil .NE. 0 ) ngxps = 2
      ENDIF
      N=NGXPS-2                                                         ! THE NUMBER OF ELEMENTS IN THE GXP ARRAY
      IF( NGXPS .EQ. 0 .AND. NLISTS .EQ. 0 .AND. type .NE. 7 .AND.
     &    type .NE. 8 .AND. type .NE. 10 .AND. type .NE. 11 .AND.
     &    type .NE. 12 .AND. type .NE. 13 .AND. type .NE. 17 .AND.
     &    type .NE. 18 .AND. type .NE. 19 .AND. type .NE. 20 .AND.
     &    cknav .EQ. 99999. ) THEN
          PRINT *,' ***  WARNING  ***  GXP not given.'
          iwarn = iwarn + 1
      ENDIF
 1020 IF(NGXPS.EQ.2) GO TO 1060
      DO 1050 I=1,N,2
      IF(BUF(I).LT.BUF(I+2)) GO TO 1050
      PRINT 1030
 1030 FORMAT(' ***  ERROR  ***  GROUP NUMBERS IN GXP MUST INCREASE.')
      IERROR=IERROR+1
 1050 CONTINUE                                                          !  MAKE SURE GXP IS TRUELY IN PAIRS
      IF(MOD(NGXPS,2).EQ.0) GO TO 1060
      PRINT 1055
 1055 FORMAT(' ***  ERROR  ***  GXP MUST BE IN PAIRS!')
      IERROR=IERROR+1
 1060 CONTINUE
 1070 IF( LS .EQ. 999999 ) LS=FS                                        ! SET THE DEFAULT PARAMETERS
      IF(LS.GE.FS) GO TO 1080                                           !  MAKE SURE THE USER'S SHOT NUMBERS INCREASE
      PRINT 1075
 1075 FORMAT(' ***  ERROR  ***  SHOT NUMBERS MUST INCREASE.')
      IERROR=IERROR+1
 1080 CONTINUE                                                          ! IS FS LARGER THAN THE LAST LS
      IF(FS.GT.LLS) GO TO 1090
      PRINT 1075
      IERROR=IERROR+1
 1090 LLS=FS
      IF( IABS(type) .LT. 1 .OR. IABS(type) .GT. 20 ) THEN
          PRINT *,' ***  ERROR  ***  Illegal TYPE.'
          ierror = ierror + 1
      ENDIF
      IF( type .EQ. 3 .OR. type .EQ. 4 .OR. type .EQ. 5 .OR. type .EQ. 6
     &    .OR. type .EQ. 8 .OR. type .EQ. 10 .OR. type .EQ. 12 .OR.
     &    type .EQ. 13 .OR. type .EQ. 14 .OR. type .EQ. 16 .OR.
     &    type .EQ. 18 .OR. type .EQ. 19 .OR. type .EQ. 20 ) THEN
          IF( navpath .EQ. ' ' ) THEN
              PRINT *,' ***  ERROR  ***  NAVFIL must be given.'
              ierror = ierror + 1
          ELSE
              CALL getfil( 4, navfil, navpath, istat )
              IF( istat .NE. 0 ) THEN
                  PRINT *,' ***  ERROR  ***  Can not open file ',
     &               navpath
                  ierror = ierror + 1
              ELSE
                  CALL frefil( -2, navfil, istat )
                  OPEN( UNIT=navfil, FILE=navpath, STATUS='OLD' )
              ENDIF
          ENDIF
          IF( navpath2 .NE. ' ' ) THEN
              CALL getfil( 2, navfil2, token, istat )
              OPEN( UNIT=navfil2, FILE=navpath2, STATUS='OLD' )
          ENDIF
          IF( ntrcs .EQ. 0 .AND. type .GE. 3 .AND. type .LT. 6 ) THEN
              PRINT *,' ***  ERROR  ***  NTRCS required for LDGO nav.'
              ierror = ierror +1
          ENDIF
          IF( ntrcs .LT. 0 ) THEN
              PRINT *,' ***  ERROR  ***  Illegal NTRCS '
              ierror = ierror +1
          ENDIF
      ENDIF
      IF( declin .LT. -90. .OR. declin .GT. 90. ) THEN
         PRINT *,' ***  WARNING  ***  DECLIN ',declin,' might be wrong.'
          iwarn = iwarn + 1
      ENDIF
      IF( dbrps .EQ. 0 ) dbrps = ABS(ggx)/2.
      IF( dbrps .EQ. 0 .AND. type .NE. 17 .AND. type .NE. 18 .AND.
     &    type .NE. 10 .AND. type .NE. 20 ) THEN
          PRINT *,' ***  ERROR  *** DBRPS required.'
          ierror = ierror + 1
      ENDIF
      IF( hpath .NE. ' ' ) THEN
          IF( navpath .NE. ' ' ) THEN
              CALL getfil( 3, lunbinary, hpath, istat )                 ! create file hpath
              IF( istat .NE. 0 ) THEN
                  ierror = ierror + 1
                  PRINT *,' ***  ERROR  ***  Can NOT create file:',
     &                    hpath
              ENDIF
          ELSE
              CALL getfil( 4, lunbinary, hpath, istat )                 ! open file hpath
              IF( istat .NE. 0 ) THEN
                  ierror = ierror + 1
                  PRINT *,' ***  ERROR  ***  Can NOT open file:', hpath
              ELSE
                  CALL podiscb( lunbinary, 3600, 0 )
              ENDIF
          ENDIF
      ENDIF
      IF( navfil .NE. 0 .AND. type .NE. 6 .AND. type .NE. 8 .AND.
     &    type .NE. 10 .AND. type .NE. 13 .AND. type .NE. 3 .AND.
     &    type .NE. 4 .AND. type .NE. 5 .AND. type .NE. 14 .AND.
     &    type .NE. 15 .AND. type .NE. 16 .AND. type .NE. 18 .AND.
     &    type .NE. 19 .AND. type .NE. 20 ) THEN
          PRINT *,' ***  ERROR  ***  Bad TYPE when NAVFIL is given.'
          ierror = ierror + 1
      ENDIF
      IF( maxdfls .LE. 0. ) THEN
          PRINT *,' ***  ERROR  ***  MAXDFLS must be greater than 0.'
          ierror = ierror + 1
      ENDIF
      IF( setback .NE. -99999. .AND. setback .LT. 0. ) THEN
          PRINT *,' ***  ERROR  ***  SETBACK must be non-negative.'
          ierror = ierror + 1
      ENDIF
      IF( dfls .EQ. 150. .AND. type .NE. 10 .AND. type .NE. 20 .AND.
     &    dfls .NE. 9 .AND. dfls .NE. 19 ) THEN
          IF( type .EQ. 1 .OR. type .EQ. 2 .OR. type .EQ. 6 ) THEN
              PRINT *,' ***  WARNING  ***  DFLS not set, using 150.'
              iwarn = iwarn + 1
          ENDIF
      ENDIF
c****
c****  I'd like to look at NAVFIL and see if it's UKOAA, but a call to rline
c****  would clobber the current line of parameters.
c****     ****  Use rline1 to get around that.
C****
C****   FINISHED A PARAMETER LIST.
C****   NOW WRITE THE LIST TO DISK AND THEN SEE IF THERE ARE ANY MORE GEOM LISTS
C****
      nglists = nglists + 1
      WRITE(IGUNIT) ngxps, nbgps, ncgps
      WRITE(IGUNIT) FS,LS,GGX,DFLS,DBRPS,SMEAR,YOFFA,YOFFB,
     *   LPRINT, type, navfil, offset, offset2, ntrcs, declin, rpadd,
     &  lunbinary, bin_h, fbinx, fbiny, lbinx, lbiny, bin_off, lunascii,
     &   navfil2, maxdfls, mindfls, setback, cknav
      IF( ngxps .GT. 0 ) WRITE (igunit) (BUF(I),I=1,ngxps)
      IF( nbgps .GT. 0 ) WRITE (igunit) (BUF(maxgxp+i),i=1,nbgps)
      IF( ncgps .GT. 0 ) WRITE (igunit)
     &    (BUF(maxgxp+maxbirds+i),i=1,ncgps)
      IF( IAND(LPRINT,1) .EQ. 1 ) THEN
          PRINT *, FS,LS,GGX,DFLS,DBRPS,SMEAR,YOFFA,YOFFB,LPRINT,type
          PRINT *, navfil, offset, offset2, ntrcs, declin, rpadd,
     &          lunbinary, navfil2, maxdfls, mindfls, setback, cknav
          PRINT *, bin_h, fbinx, fbiny, lbinx, lbiny, bin_off, lunascii
          PRINT *, ngxps, nbgps, ncgps
          IF( ngxps .GT. 0 ) PRINT *, ' gxp ',(BUF(I),I=1,NGXPS)
          IF( nbgps .GT. 0 ) PRINT *, ' bgp ',(buf(maxgxp+i),i=1,nbgps)
          IF( ncgps .GT. 0 ) PRINT *,' cgp ',(buf(maxgxp+maxbirds+i),
     &        i=1,ncgps)
      ENDIF
C****
      LS = 999999                                                       ! DEFAULT THE DEFAULTS
      NLISTS=NLISTS+1
      ngxps1 = 1
c      NGXPS=0
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN
          IF( NOW .EQ. 1 ) PRINT *,' <  ENTER PARAMETERS  >'
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GOTO 2020
      ENDIF
      IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
c****
c**** Convert the ACSII elevations to binary
c****
      IF( lunascii .NE. 0 .AND. type .LT. 14 ) THEN
 3000     CALL rline( lunascii )
          IF( nchars1 .GT. 0 ) THEN
              CALL getoke( token, nchars2 )
              CALL dcode( token, nchars2, areal, istat )
              CALL wrdisc( lunbinary, areal, 1 )
              CALL getoke( token, nchars2 )
              CALL dcode( token, nchars2, areal, istat )
              CALL wrdisc( lunbinary, areal, 1 )
              GOTO 3000
          ENDIF
c          CALL frefil( 2, lunascii, istat )
c          lunascii = lunbinary
      ENDIF
c****
      IF( smear .EQ. 999999. ) smear = dbrps                            ! calcrp and type 13 need a good value
c****
C
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
