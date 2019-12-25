      subroutine ldgogeom(lhead,ihead,itype,ildgounit,xbinp,ioff0p,
     &                    nfoldp,ngpspp,offsetp,offset2p)
c*GMK
c*** Modifications:
c*** Mod 23 Oct 1992 by pch to start at the beginning of the file on each
c***            shot.  Also revert back to checking for shot number and
c***            shot time being equal,  Sepr line 27 has duplicate shot
c***            numbers.  The line also has dropped shots.
c*** Mod 16 Dec 1992 by GMK (ESPs distance d is actually in 1/10's of meters
c*              d = d/10. Also kill ghost shots THISFLAGSHOT = 3. Also kill
c*               shots w/ bougus bearing if type = 5 (ESP).
c*   Mod 17 Dec 1992 Had a mix-up w/ OS bearing and MS bearing in code -- fixed.
c*   Mod 18 Dec 1992 Place LDGO line # into CDP slot in trace header and fake some CDP trace
c*              #'s to sort on such that the CDP (really ESP) can be gathered.
c*   Mod 15 Apr 1994 ifudgemin so as to allow 1 sec mismatches near minute mark
c*               Oh well, time to CHECK only SHOt #'s  for comparison, everything else is
c*               unreliable over an entire experiment.
c*   Mod 21 Dec 1994 Basically only shot # reliable so...check nav file for shot # only
c*               but will warn user if any mismatch occurs, Yikes!
c  mod July 95 by pch
c change ifudgehour = abs(tthr-ihr)  to ifudgehour = abs(tthr-ihour)
c and ifudgeminute to ifudgemin
c  18 Mar 1997 - add the line3 option so that we don't kill any traces
c                when line3 = 1.  Also set    thisshotflag = 1  when its 2!
c  30 Apr 97 - If type .EQ. -3, then force every shot in the nav file to
c              be 1 (thisshotflag = 1)
c  June 2000 - g77 didn't like line3 being a logical
c
c
c*    The following code was modified from a LDGO program that calculates offset(range)
c*    for various geometries, namely type = 3 (CDP), type = 4 (WAP) and type = 5 (ESP).
c*    These parameters can be calculated assuming one is given a LDGO log file and some
c*    other geometrical information (i.e. gun-antenna offset for both ships etc.....).
c*    To find the proper shot in the log file, it is necessary to know the field shot
c*    number (bytes 245-248 BCD) for trace 0 header which is read during the input process.
c*    The following variables are returned from the log file (binary) and are used to
c*    calculate offset and CDP number (type = 3 or 4):
c*    Navigation file:
c*        info header:   position (4byte words) total size = 1024 bytes
c*                       idirshot1 = 2        1st shot number
c*                       idirshotn = 3        last shot number
c*                       inumrec   = 7        total number of shots
c*                       idirangfact = 19     angle factor (i.e. angle = angle/idirangfact
c*                       ildgoline = 20       LDGO line number
c*
c*        shot headers:  position (4byte words) total size = 256 bytes * number of shots
c*                       msshot = 2            field shot number found in lbuf(3)
c*                       thisshotflag = 5      = 1 if nav ship (i.e Ewing), or = 2 if other ship
c*                       msdeadshot = 6        bad trace, nav ship
c*                       osdeadshot = 7        bad trace, other ship
c*                       ttjd = 9              julian day >
c*                       tthr = 10             hour        >  >  >
c*                       ttmn = 11             min          >  >  use to verify shot
c*                       ttsec = 12            sec           >      an additional check!
c*                       finalrange = 16       ship to ship range (tenths of meteres)
c*                       mscourse = 19         nav ship course
c*                       oscourse = 20         other ship course
c*                       bearostoms = 21       bearing other ship to nav ship
c*                       distcdp1 = 22         distance along nav. line from 5000 m reference point
c*                       mslat = 23            latitude of nav ship (MS)  deg*10**6
c*                       mslon = 24            longitude of nav ship (MS) deg*10**6
c*                       mscdpno = 25          cdp number corresponding to this shot MS
c*                       spcdpbin = 30         cdp bin size meters (*100)

      integer*4 logbuf(256)
      integer*4 ishot, ishotold, itrace, lhead(111),
     &idirshot1,idirshotn, inumrec, idirangfact, ishotpos, numtrys,
     &maxtrys,ildgounit, ishotmis, msshot, thisshotflag, msdeadshot,
     &osdeadshot, finalrange, mscourse, oscourse, bearostoms, distcdp1,
     &mslat, mslon, msmaxcdp, spcdpbin, ttjd, tthr, ttmn, ttsec, type,
     &ilogssh, ilogrsh, ilogstorb, ilogrange, ixsdist , ktrace, istrv,
     &ilogcdpdis, ioffset, icdpno, nfold, ildgoline

      real*4 xbin, ioff0, ngpsp, dag1, dag2, d, theta, phi, ymd,
     &xmd, yda, xda, xxmt, xat, yat, angfact, radeg, range, dist,
     &rsshp, refdist

      integer*2 iday, imin, ihour, isec, ihead(111)

      logical first, first1
c****  line3 on tera has dead trace flag on everything, don't kill any
c****  shots when line3 is set to 1, only kill when line3 is 0
      data first /.true./, maxtrys /10/, first1 /.true./, line3/0/

      save

      type = IABS(itype)
c*    Get variables from the lhead(lbuf) or ihead(ibuf) arrays regarding shot trace
      ishot  = lhead(3)
      itrace = lhead(4)
c*    If same shot, different trace - then skip past log file to savetime
      if (ishot.eq.ishotold) goto 99
      iday   = ihead(80)
      ihour  = ihead(81)
      imin   = ihead(82)
      isec   = ihead(83)

c*    check to see if shot number is zero, if so kill it and exit subroutine
      if (ishot.eq.0 .AND. line3 .EQ. 0 ) then
        ihead(15) = 2
        goto 999
      endif

c*    search through log file to find shot and compare the GMT to assure
c*    that the proper trace is identified

c*    get line variables
      if (first) then
         call podisc( ildgounit, 1, 0)                                  ! rewind
         call rddiscb( ildgounit, logbuf, 1024, istat)
         if (istat .NE. 1024 ) THEN
             PRINT *,' ***  ERROR  ***    Bad ldgo nav file.'
             stop
         ENDIF
c*       get variables form line log header 1024 bytes long
         idirshot1 = logbuf(2)
         idirshotn = logbuf(3)
         inumrec   = logbuf(7)
         idirangfact = logbuf(19)
         ildgoline = logbuf(20)
c*       disable if loop since we don't need info for each shot
c         first = .false.
      endif

c*    look for shot # in log file
c*    skip 1024 bytes (255 4byte words) and nshots*256 bytes (64 4 byte words)  --> ishotpos
c*    if shot = 0 i.e. lhead(3)=0, then go to mute trace end then exit - bad trace
   10 CONTINUE
c*    read nav shot values
      call rddiscb( ildgounit, logbuf, 256, istat )
      if (istat .NE. 256 ) THEN
         PRINT *,' ***  ERROR  *** Could not find shot ',lhead(3),
     &        ' trace ',lhead(4),' in the ldgo nav file.'
         ihead(15) = 2
         RETURN
      ENDIF
c*    set  match variables from nav header and compare to values read in from trace 0
      msshot = logbuf(2)                                                !nav file shot num
      ttjd   = logbuf(9)                                                !time julian day
      tthr   = logbuf(10)                                               !time hour
      ttmn   = logbuf(11)                                               !time minute
      ttsec  = logbuf(12)                                               !time sec
      ifudgesec = abs(ttsec-isec)                                       !sometimes shot tme trace 0 and logfile shots
                                                                        !are off by a second - hence its fudged to be within
                                                                        !1 second of shot time
      ifudgeday = abs(ttjd-iday)                                        !day are sometimes off by 1 too
      ifudgemin = abs(ttmn-imin)                                        ! minute will be off, if off by a sec. near min. mark
      ifudgehour = abs(tthr-ihour)                                        ! check hour mark also


c*    Did we find the proper shot in nav file

      if ( msshot.eq.lhead(3) ) then
c* Lets check for fun and see how off the DSS-240 values are:
c* Write values to screen for trace 1 only
      if ( itrace .eq. 1) then
       if (ifudgeday.ne.0) print*, '***Warning, day mismatch of ',
     &  ifudgeday, ' with navigation file for shot#', msshot
       if (ifudgehour.ne.0) print*, '***Warning, hour mismatch of ',
     &  ifudgehour, ' with navigation file for shot#', msshot
       if (ifudgemin.ne.0) print*, '***Warning, minute mismatch of ',
     &  ifudgemin, ' with navigation file for shot#', msshot
       if (ifudgesec.ne.0) print*, '***Warning, second mismatch of ',
     &  ifudgesec, ' with navigation file for shot#', msshot
      endif

c*   Lamont timing mark on DSS-240 sucks the big one -- not consistent
c*   we will risk for now, not using another check since its not robust enough
c*   will use fudge parameters for now
c*   if so, read variables for particular shot from 256 byte nav. header
         thisshotflag = logbuf(5)                                       !=1, nav; = 2, os
         IF( itype .EQ. -3 ) thisshotflag = 1
         msdeadshot = logbuf(6)                                         != -1 bad nav shot
         osdeadshot = logbuf(7)                                         != -1 bad os shot
         finalrange = logbuf(16)                                        !ship to ship range
         mscourse = logbuf(19)                                          !nav ship course
         oscourse = logbuf(20)                                          !other ship course
         bearostoms = logbuf(21)                                        !bearing other ship to mother ship
         distcdp1 = logbuf(22)                                          !distance to initial cdp nav point
         mslat = logbuf(23)                                             !lat of shot nav ship *10**6
         mslon = logbuf(24)                                             !long of shot nav ship *10**6
         msmaxcdp = logbuf(27)                                          !max num cdps this shot nav ship
         spcdpbin = logbuf(30)                                          !cdp bin size *100
c         print *,(ii,logbuf(ii),ii=1,30)
      else
c*     Basically looks at each value in the LDGO log file until a match is achieved,
c*     or end of file is encountered
         goto 10
      endif

c*    check if type = 3 (cdp) and os ship shots were recorded, then kill os shots or..
c*    check if type = 5 (esp) and ms ship shots were recorded, then kill ms shots
c*    and place bougus range in header (-999999)
99    if (type.eq.3 .and. thisshotflag.eq.2 ) then
         IF( line3 .EQ. 0 ) THEN
             ihead(15) = 2
             lhead(10) = -999999
             goto 999
         ELSE
             thisshotflag = 1
         ENDIF
      else if (type.eq.5 .and. thisshotflag.eq.1.AND. line3 .EQ. 0) then
         ihead(15) = 2
         lhead(10) = -999999
         goto 999
      endif

c*    check to see if bogus bearing is placed in either logbuf(19,20,21)
c*    if so, kill trace and place bougus range in header (-999999)
       if (type .ne. 3 .AND. line3 .EQ. 0 ) then
        if (mscourse.eq.-999999 .or. oscourse.eq.-999999 .or.
     &  bearostoms.eq.-999999) then
          ihead(15) = 2
          lhead(10) = -999999
          goto 999
        endif
       endif

c*    check to see if shot is really DSS - 240 trick shot to get the record length
c*    beyond 19.8 seconds - Will kill traces from ghost shot! If you want 0-40 sec.
c*    of data use SIOSEIS process CAT to concatenate shots (remember there will be
c*    a 2 second gap 19.8-20.0 and 39.8-40.0 DSS - 240 did not record then!
c*    will kill traces and place bogus range in header for now!
      if (type.eq.5 .and. thisshotflag.eq.3 .AND. line3 .EQ. 0 ) then
          ihead(15) = 2
          lhead(10) = -999999
          goto 999
      endif

c*    again check to see if there is a bad trace in log file
      if (thisshotflag.eq.1 .and. msdeadshot.eq.-1.AND.line3.EQ.0) then
         ihead(15) = 2
         goto 999
      elseif (thisshotflag.eq.2.and.osdeadshot.eq.-1.AND.line3.EQ.0)then
         ihead(15) = 2
         goto 999
      endif

c*    we are now going to do the offset and cdp number calculations for type = 3,4 and 5
c*    1st, we define the navigation and shooting parameters from the ldgo geometry file
c*    via sioseis (for both the shooting and navigation ships)

      if (first1) then
	xbin  =	abs(xbinp)                                                   !bin size in meters for cdp.
	ioff0 = abs(float(ioff0p))                                           !recording ship distance antenna to 1st receiver.)
        nfold = abs(nfoldp)                                                  !recording ship # of channels.
        ngpsp = abs(float(ngpspp))                                           !recording ship group spacing.
c*      I've hardwired the istrv value to be 0 for the DSS 240 system
        istrv =	0                                                            !recording ship streamer channel 1, 0-far, 1-near
        dag1 = abs(offsetp)                                                  !navigation ship distance antenna to guns.
        dag2 = abs(offset2p)                                                 !other ship ship distance antenna to guns.
        first1 = .false.
      endif

c*    define the following from appropriate nav log record knowing the shooting and
c*    recording ships.
        ilogssh   = oscourse                                            !shooting ship course
        ilogrsh   = mscourse                                            !recording ship course
        ilogstorb = bearostoms                                          !bearing os to ms.
        ilogrange = finalrange                                          !ship to ship range this shot

      ixsdist = 0
c*    ixdist used for explosive ESP shots - not done today
	if(type.eq.5) then
           ixsdist = 0                                                   !shooting ship extra source distance, set = 0
	endif

c*    reverse bearing to ms to os ship - used for two ship, two streamer work!
c*    recent experiments do not include this geometry - nevertheless is included for future work
	if(thisshotflag.eq.1) then
           ilogstorb=ilogstorb-180                                      !we really want bearing shooting to recording.
	endif

c*    distance from initial navigation point to shot location
          ilogcdpdis = distcdp1                                         !distance along line ms this shot.

c*    offset calculation.
        radeg=3.14159/180.
c*    angfact is used to retrieve more precise angle measurements (usually 100)
        angfact = float(idirangfact)
        if(angfact.eq.0) angfact=1.0
        angfact=1./angfact
        if (thisshotflag.eq.2) then
           d=float(ilogrange)/10.0                                      !ship ship distance (tenths of meters)
           theta=angfact*float(ilogstorb-ilogrsh)*radeg                 !bearing angle between shooting ship and nav. ship
           phi=angfact*float(ilogssh-ilogrsh)*radeg                     !feather angle of shooting ship w/ respect to nav. ship
           ymd=d*sin(theta)                                             !perpendicular projection w/ respect to nav. ship of s-s distance
           xmd=d*cos(theta)                                             !parallel projection w/ respect to nav. ship of s-s distance
           yda=(dag2+float(ixsdist))*sin(phi)                           !perpendicular projection w/ respect to nav. ship of source feathering
           xda=(dag2+float(ixsdist))*cos(phi)                           !parallel projection w/ respect to nav. ship of source feathering
       else
           ymd=0
           xmd=0
           yda=0
           xda=dag1                                                     !airgun to antenna distance nav ship = dag1
       endif
c*    calculate offset w/ respect to streamer channel
        ktrace=itrace
        if(istrv.eq.1) then                                             !streamer channel 1 is near trace, not the case however
           ktrace=nfold-ktrace+1
        endif
        xxmt=ioff0+float(nfold-ktrace)*ngpsp                            !distance from antenna to itrace channel on streamer
        xat=xxmt-xmd-xda
        yat=-ymd-yda
        range=sqrt(xat**2+yat**2)                                       !calculate range using X**2 + Y**2 = Z**2
        if(xat.gt.0) then
           range=-range
        endif
        if(type.ne.5) then                                              !ranges absolute for waps and cdps
           range=abs(range)
        endif

c*      place integer range in segy header
        ioffset= nint(range)                                            !make range an integer
        lhead(10) = ioffset

c*    calculate cdp number (LDGO line number) and pseudo cdp trace number for
c*    ESP binning of ranges i.e. 25 m range increments will have same trace #
c*    which is preferable for SIOSEIS process SORT

      if (type.eq.5) then
        lhead(6) = ildgoline
        lhead(7) = nint(10000 + (ioffset/25.0) )
      endif

c*    calculate cdp number corresponding to log (type 3 and 4)

      if (type.eq.3 .or. type.eq.4) then

	rsshp=0.0

c*    if the shooting ship is not the navigation ship, i.e. thisshotflag = 2
	if(thisshotflag.eq.2) then
	  rsshp= float(ilogrange)/10.0
	endif

c*    if the shooting ship is not the lead ship, i.e. not 1 (2 source, 2 receiver geometry)
	if(thisshotflag.eq.2) then
	  rsshp=-rsshp
	endif

c*    calculate half offset variable
	refdist=float(ioffset)*.5

c*    if the shooting and recording ships are the same or the shooting ship
c*    is the lead ship(1)
	if(thisshotflag.eq.1) then
	  refdist=-refdist
	endif
c	 print *,' refdist=',refdist,' ilogcdpdis=',ilogcdpdis,
c     &   ' rsshp=',rsshp,' dag1=',dag1

c*    midpoint distance away from navigational initial point
        if (thisshotflag.eq.1) then
          dist=float(ilogcdpdis)+rsshp-dag1+refdist
        else
          dist=float(ilogcdpdis)+rsshp-dag2+refdist
        endif

c*    calculate cdp number and place it in segy header
c	print *,' dist=',dist,' ilog=',ilogcdpdis
	icdpno = int((dist+xbin/2.0)/xbin + 1)
        lhead(6) = icdpno

c*    endif to calculate cdp
      endif

c*    put lat, lon, dead trace into segy header,
c*    and ready it to be passed onto next process

      lhead(19) = mslon                                                  !longitude*10**6
      lhead(20) = mslat                                                  !latitude*10**6
      ihead(15) = 1                                                      !good trace

999   ishotold = ishot

      return
      end

