      SUBROUTINE fdfmex( buf, lbuf, ibuf, scr, lscr, iscr, istop,
     *           nready )
c-------------------------------------------------------------------------------
c     FDIFEX is the SIOSEIS execution phase of process FDDIFF, finite
c  difference modelling of the diffraction process.
c-------------------------------------------------------------------------------
c
      INTEGER    ERROUT
      PARAMETER (ERROUT = 6)                                            ! error output for process
      PARAMETER ( MAXDTS = 10 )                                         ! the max. number of time-delta tau pairs
      PARAMETER ( MAXNTR = 4000 )                                       ! the max. number of traces
      PARAMETER ( MAXSMP = 4000 )                                       ! the max. number of taus
c
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      COMMON /fdmigr/ junit, nlists, nwrds
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad
      COMMON /readt/ ilun, numhdr, numdat
c
      INTEGER   fno, bpad, epad, opad, bclpad, eclpad
      REAL      maxdip, nrho, ldx
      DIMENSION buf(111), lbuf(111), scr(111), lscr(111)
      INTEGER*2 ibuf(111), iscr(111)
      INTEGER   dtpool(230,200), ttaustp(maxsmp), ttausiz(maxsmp)
      INTEGER   taustp(maxsmp), tausiz(maxsmp), idtaus(maxdts*2)
      LOGICAL   first, line3d, big
      CHARACTER*40 name
C
      INTEGER  inlen,    outlen,   xoutle,   youtle,   zoutle,
     +         insr,     outsr,    xoutsr,   youtsr,   zoutsr,
     +         inntr,    outntr,   xoutnt,   youtnt,   zoutnt,
     +         insamp,   outsam,   xoutsa,   youtsa,   zoutsa,
     +         cursam,   cursr,    crb,      tr,       endsh,    taper
      LOGICAL  gather,   stacke,   dead
      REAL     dist
      COMMON   /AVDATA/
     +         inlen,    outlen,   xoutle,   youtle,   zoutle,
     +         insr,     outsr,    xoutsr,   youtsr,   zoutsr,
     +         inntr,    outntr,   xoutnt,   youtnt,   zoutnt,
     +         insamp,   outsam,   xoutsa,   youtsa,   zoutsa,
     +         cursam,   cursr,    crb,      tr,       endsh,
     +         taper,    gather,   dist,     stacke,   dead
C
      INTEGER
     +adtrin,   adtrot,   adwin1,   adwin2,   adwin3,   adwin4,
     +adnliv,   adstak,   adscra
      COMMON /AVAP/
     +adtrin,   adtrot,   adwin1,   adwin2,   adwin3,   adwin4,
     +adnliv,   adstak,   adscra
C
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of ?K 32-bit floating point words.
C
      REAL    apdata(0:5000000)
      INTEGER iapdata(0:5000000)
      COMMON /apmem/ apdata                                             ! Veritas called this ap120bmd
      EQUIVALENCE (apdata,iapdata)
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
      INTEGER APSP(0:15)
      COMMON/AP120BSP/APSP
C
      PARAMETER (isize = 262144 )                                       ! 512x512
      COMMON /transp/ t(isize)                                          ! The transpose matrix t
c
      SAVE
c
      DATA first/.TRUE./, mlists/0/, line3d/.FALSE./, big/.FALSE./
      DATA ndone/0/, ihdradd/0/
c
c
      IF( .NOT. first ) GOTO 320
      first = .FALSE.
      CALL timer
      IF ( icompt .EQ. 5 ) ihdradd = 60                                 ! Cray short integer header additive
      CALL getfil( 1, jhdrun, name, istat)                              ! File for ALL the trace headers
      dtpool(1,1) = 3
      ncol        = 2                                                   ! the index of the column of the control point
      CALL podisc( junit, 1, 0 )
  100 CALL rddisc( junit, lscr, nwrds, istat )                          ! read first part of a parameter list
      mlists = mlists + 1
      lprint = lscr(1)
      fno    = lscr(2)
      IF( fno .EQ. 0 ) THEN                                             ! preset fno to the first shot/rp
          IF( lbuf(7) .EQ. 0 ) THEN
              fno = lbuf(4)
          ELSE
              fno = lbuf(6)
          ENDIF
      ENDIF
      lno = lscr(3)
      IF( lno .LE. 0 ) lno = fno
      dx     = scr(4)
      maxdip = scr(5)
      nvels  = lscr(6)
      npairs = nvels / 2                                                ! the number of velocity-time pairs
      ivtype = lscr(7)
      bpad   = lscr(8)
      epad   = lscr(9)
      opad   = lscr(10)
      velmul = scr(11)
      nrho   = scr(12)
      fcrho  = scr(13)
      rho    = scr(14)
      theta  = scr(15)
      gamma  = scr(16)
      ndtord = scr(17)                                                  ! the number of ndtaus to read from disk
      IF( mlists .EQ. 1 ) ndtaus = scr(17)                              ! use dtaus from the first list only
      ndtau  = ndtaus / 2
      ntrpl  = lscr(18)
      nlines = lscr(19)
      ldx    = scr(20)
      bclpad = lscr(21)
      eclpad = lscr(22)
      nx     = lscr(23)
      maxsam = lscr(24)
      lunt   = lscr(25)
      IF ( maxsam .EQ. 0 )
     *  maxsam = ibuf(58+ihdradd)+buf(46)/buf(49)+.5                    ! nsamps+delay/si
      maxmil = NINT( FLOAT(maxsam-1) * buf(49) * 1000.)                 ! Largest time in mils
      IF( maxsam*nx .GT. isize ) THEN
          big = .TRUE.
          IF( lunt .EQ. 0 ) CALL getfil( 1, lunt, name, istat )
      ENDIF
      IF( (IAND(lprint,2) .ne. 0).and. (mlists.eq.nlists) ) THEN
          PRINT *,' mlists=',mlists,' nlists=',nlists,' fno=',fno,
     *       ' lno=',lno,' dx=',dx,' maxdip=',maxdip
          PRINT *,' nvels=',nvels,' npairs=',npairs,' ivtype=',ivtype,
     *       ' bpad=',bpad,' epad=',epad,' opad=',opad
          PRINT *,' velmul=',velmul,' nrho=',nrho,' fcrho=',fcrho,
     *       ' rho=',rho,' theta=',theta,' gamma=',gamma
          PRINT *,' ndtaus=',ndtaus,' ndtau=',ndtau,' nlines=',nlines,
     *       ' ldx=',ldx,' bclpad=',bclpad,' eclpad=',eclpad
          PRINT *,' nx=',nx,' maxsam=',maxsam
      ENDIF
      IF ( nvels .GT. 0 ) THEN
          CALL rddisc( junit, scr, nvels, istat)
          IF( istat .NE. nvels ) THEN
              PRINT *,' rddisc error in fdmiex at 110, istat=',istat,
     *                ' nvels=',nvels
              STOP
          ENDIF
  110     ncol        = ncol + 1
          dtpool(2,1) = ncol                                            ! the index of the last control point
c****
c****   build an entry in dtpool for this control point
c****
          dtpool(1,ncol) = ivtype
c****     dtpool(2,x) contains a code indicating the type of control point
c****     1=start of a zone, 2=middle of a zone, 3=end of a zone
c****     this corresponds to Veritas 'S','C','E'
          IF ( mlists .EQ. 1) THEN
            dtpool(2,ncol) = 1                                          ! Type 1 ("S")  Start of Zone
          ELSE IF( mlists .EQ. nlists) THEN
            dtpool(2,ncol) = 3                                          ! Type 3 ("E")  End of Zone
          ELSE
            dtpool(2,ncol) = 2                                          ! Type 2 ("C") Center
          ENDIF
          dtpool(3,ncol) = fno
          ncontr = 1
          IF( lno .NE. 0 ) ncontr = lno - fno + 1
          dtpool(4,ncol) = ncontr
          dtpool(5,ncol) = npairs
          itemp1         = 5
          itemp2         = 117
          IF( scr(2) .ne. 0 ) THEN                                      ! make sure the first time is 0!
              dtpool(6,ncol)   = 0
              dtpool(118,ncol) = scr(1)                                 ! velocity
              itemp1 = 6
              itemp2 = 118
              dtpool(5,ncol) = dtpool(5,ncol) + 1
          ENDIF
          DO 120 i = 1, npairs
             dtpool(itemp1+i,ncol) = scr(i*2) * 1000. + .5              ! time in milliseconds!
             dtpool(itemp2+i,ncol) = scr(i*2-1)                         ! velocity
  120     CONTINUE
          IF ( lno .NE. 0 .AND. fno .NE. lno ) THEN                     ! Constant velocity from fno to lno (fno NE lno)
             fno = lno
             lno = 0
             GOTO 110
          ENDIF
          IF( IAND(lprint,2) .NE. 0 ) THEN
              npairs = dtpool(5,ncol)                                   ! In case npairs is out of date
              PRINT '(/A,I3)',' dtpool column ',ncol
              PRINT 605, (dtpool(i,ncol),i=1,5)
              PRINT *,'   Time     Velocity'
              PRINT '(2I10)', (dtpool(5+i,ncol),dtpool(117+i,ncol),
     $                           i = 1, npairs)
 605      FORMAT(' Type: ',I1,' Code: ',I1,' CDP No ',I5,' Distance ',
     $           I5,' Npairs ',I3)
          ENDIF
      ENDIF
c****
c****  get the tau size and tau step, even though they are constants
c****  for the entire job (thus the last one is used!)
c****
      IF( ndtord .GT. 0 ) THEN
          CALL rddisc( junit, scr, ndtord, istat)
          IF( istat .NE. ndtord ) THEN
              PRINT *,' rddisc error in fdmiex at 150, istat=',istat,
     *                ' ndtord=',ndtord
              STOP
          ENDIF
          DO 150 i = 1, ndtord                                          ! convert to integer milleseconds
  150     idtaus(i) = NINT( scr(i) * 1000.)
          IF( IAND(lprint,2) .NE. 0 )
     *        PRINT *,' idtaus=',(idtaus(i),i=1,ndtaus)
      ENDIF
c
      IF( mlists .LT. nlists ) GOTO 100
c****
c****    All the velocities are in array dtpool!
c****   get taustp and tausiz in shape
c****
  200 CONTINUE
      itime = 0                                                         ! Create an array of tau times - a time for each step
      ntau  = 0                                                         ! count the taus
c     Remember that dtaus are time-delta tau pairs
      IF (idtaus(1).eq.0) THEN                                          ! ignorea zero tau time
        istart = 3
      ELSE
        istart = 1
      ENDIF
      IF( idtaus(ndtaus-1) .GT. maxmil ) idtaus(ndtaus-1) = maxmil
      DO 250 i = istart, ndtaus-1, 2
  240    itime = itime + idtaus(i+1)
         ntau  = ntau + 1
         taustp(ntau) = itime
         tausiz(ntau) = idtaus(i+1)
         IF( itime .LT. idtaus(i) ) GOTO 240
  250 CONTINUE

c**** GMK necessary to have the first tau step(actually the last taustep calculated)
c**** GMK at time zero when doing the forward problem therefore the last taustep
c**** GMK (which is actually calculated first) is thrown out (contains only one sample)
      DO 251 i = 1, ntau-1
         ttaustp(i+1) = taustp(i)
         ttausiz(i+1) = tausiz(i)
 251  CONTINUE

         ttaustp(1) = 0
         ttausiz(1) = taustp(1)

      DO 252 i = 1, ntau
         taustp(i) = ttaustp(i)
         tausiz(i) = ttausiz(i)
 252  CONTINUE
C**** GMK if the last tausize is too large/small change it. This occurs
C**** GMK when the taustep is not a multiple of the number of samples
      IF (tausiz(ntau).ne.(maxmil-taustp(ntau)) ) THEN
         tausiz(ntau) = maxmil-taustp(ntau)
      ENDIF

      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' ntau=',ntau
          PRINT *,' taustp=',(taustp(i),i=1,ntau)
          PRINT *,' tausiz=',(tausiz(i),i=1,ntau)
      ENDIF
c****
c****  Generate the front pad traces.
c****  save the trace headers in a disk file
c****  add the delay to the data (so all data starts at time zero)
c****  get all the data until istop is nonzero
c****  ASSUME that bpad and epad are greater than zero
c****
      IF( ndone .EQ. 0 ) THEN
          DO 300 i = 1, maxsam
             scr(i) = 0.
  300     CONTINUE
          DO 310 i = 1, bpad
             IF( .NOT. big ) THEN
                 CALL trans1( scr, maxsam, nx, 0 )
             ELSE
                 CALL trans2( scr, maxsam, nx, lunt, 0 )
             ENDIF
  310     CONTINUE
      ENDIF
c****
c****   FDDVEL needs to know the first shot number
c****
      IF( lbuf(7) .EQ. 0 ) THEN
          istart = lbuf(3)
      ELSE
          istart = lbuf(6)
      ENDIF
c****
c****  Get all the data and transpose it
c****
  320 CONTINUE
      IF( istop .LT. 0 ) GOTO 340                                       ! Is there a trace in buf?
      IF( lbuf(7) .EQ. 0 ) THEN                                         ! is it a gathered trace?
          no = lbuf(3)
      ELSE
          no = lbuf(6)
      ENDIF
      si     = buf(49)                                                  ! sample interval in mils
      delay  = buf(46)                                                  ! the deep water delay
      cursr  = NINT( si * 1000.)                                        ! put sample interval in mils in common
      nsamps = ibuf(58+ihdradd)                                         ! number of samples
      inlen  = NINT( FLOAT(maxsam)*si*1000. + 1.)                       ! No. of mils - stick in common
      CALL wrdisc( jhdrun, buf(1), numhdr, istat )                      ! get rid of the trace header
      iout = 0                                                          ! force rlseap to cough up the data
      CALL rlseap( buf(numhdr+1), nsamps )
c****
c****   FDM requires all data to start from time zero, so remove the
c****  deep water delay.
c****    Data needs to be time reversed!
c****
      ndelay = 0
      indexs = 0                                                        ! index to the scratch array scr
      IF( delay .GT. 0. ) THEN
          ndelay = delay / si + .5
          DO 330 i = 1, ndelay
  330     scr(i) = 0.
          indexs = ndelay
      ENDIF
      DO 331 i = 1, nsamps
  331 scr(indexs+i) = buf(numhdr+i)
      IF( ndelay + nsamps .LT. maxsam ) THEN                            ! zero fill the back of the trace
          nfill = maxsam - (nsamps+ndelay)
          DO 332 i = 1, nfill
  332     scr(nsamps+ndelay+i) = 0.
      ENDIF

c**** GMK comment out the time reversal of data array scr because we need to propagate
c**** GMK in the opposite sense when doing the forward problem

c     DO 335 i = 1, maxsam
c335  buf(i) = scr(maxsam-i+1)

      DO 335 i = 1, maxsam                                              ! time reverse the data
 335  buf(i) = scr(i)


c****
c****   start to transpose the data into time slices
c****
      IF( .NOT. big ) THEN
          CALL trans1( buf, maxsam, nx, 0 )
      ELSE
          CALL trans2( buf, maxsam, nx, lunt, 0 )
      ENDIF
      ndone = ndone + 1
      IF( istop .EQ. 0 ) RETURN


c****
c****   ALL THE DATA IS HERE, do epad and finish the transposing
c****
  340 continue
      DO 350 i = 1, maxsam
         scr(i) = 0.
  350 CONTINUE
      iend = 0
      DO 360 i = 1, epad
         IF( i .EQ. epad ) iend = 1
         IF( .NOT. big ) THEN
             CALL trans1( scr, maxsam, nx, iend )
         ELSE
             CALL trans2( scr, maxsam, nx, lunt,  iend )
         ENDIF
  360 CONTINUE
      ntrcs = bpad + epad + ndone
      IF( ntrcs .GT. nx ) THEN
          PRINT *,' ***  ERROR  ***  The number of traces in the job (',
     *           ntrcs,') is larger than nx (',nx,')'
          STOP
      ENDIF
      call cmpslice(maxsam, nx, ntrcs, big, lunt)
c
      call second(timea)
      write(ERROUT,*) 'Finished input slice transfer at:',timea
c
c****
c****    Find the velocity function for every trace at ever tau step
c****   and transpose it
c****
      jndex  = 1
c
c..   subroutine avspintr (called by avintr called by fdmvel needs
c..   2 ap scratch arrays starting at adwin1 and adwin2
c
      adwin1 = maxsam + maxsam
      adwin3 = adwin1 + maxsam + maxsam
      CALL getfil( 1, lunvel, name, istat )                             ! keep the velocities on disc
      DO 370 i = 1, ntau                                                ! make sure the file is big enough for positioning
  370 CALL wrdisc( lunvel, scr, nx )
c
c****  fddvel clobbers the ap
c**** GMK subroutine fdmvel was changed to fddvel to average of newly defined tausteps
      CALL fddvel( istart, ndone, si, dx, dtpool, dtpool, ntau, taustp,
     *     nx, lunvel, maxmil, vpadss, vpadse, vpadgs, vpadge,
     *     line3d, ncrbln, nline, maxsam, no )
c
      call second(timea)
      write(ERROUT,*) ' Finished velocity multiplexing at:', timea
c
      IF( IAND( lprint,4) .NE. 0 ) THEN
          CALL podisc( lunvel, 1, 0 )
          ovgbase  =  (4.0 * dx**2) / si
           do 379 j = 1,ndone
 379        lscr(j) = 0
          print *,' Multiplexed velocity function - ntau: ', ntau,
     *            ' ntraces: ', ndone
          DO 380 i = 1, ntau
             CALL rddisc( lunvel, scr(ndone+1), nx, istat )
             IF( istat .NE. nx ) THEN
                PRINT *,' rddisc error in fdifex at 380, istat=',istat,
     *                  ' nx=',nx
                STOP
             ENDIF
             itemp = 0
             do 381 j = 1, ndone
               lscr(ndone+j) = nint (2 * sqrt( scr(ndone+j) * ovgbase) )
               if (iabs(lscr(ndone+j)-lscr(j)).gt.1 ) then
                 itemp = 1
               endif
  381        continue
             if ( (itemp.eq.1).or.(i.eq.ntau) ) then
               print *,' tau no: ', i-1,'tau: ',taustp(i-1)
               print '(10I6)', ( lscr(j),
     *                           j = 1, ndone)
               do 383 j = 1, ndone
  383            lscr(j) = lscr(ndone+j)
             endif
  380     CONTINUE
c          call exit
      ENDIF
c
c****    FORWARD MODEL DIFFRACTIONS
c****
      CALL fdmdif( gamma, rho, theta, si, ndone, bpad, epad, ntau,
     *  taustp, tausiz, maxsam, lunvel, nx, lunt, ntrcs, maxsam,
     *  nrho, fcrho, big )
c
      call second(timea)
      write(ERROUT,*) 'Finished Modelling Tau steps at: ',timea
c
      ndone   = 0
      ntxcall = 0
      nready  = ntrcs
      IF( opad .EQ. 0 ) nready = ntrcs - bpad -epad
      nreadyy = nready                                                  ! nready not defined for entry getntx
      CALL podisc( jhdrun, 1, 0 )

      RETURN



      ENTRY getntd( buf, lbuf, ibuf, scr )
c****
c****   Return one trace at a time
c****  transpose the data back to the TX domain
c****  The shot/rp numbers will be screwy if the pad traces are also output
c****
      ntxcall = ntxcall + 1                                             ! The number of times getntx is called
 1100 CONTINUE
      IF( .NOT. big ) THEN
          CALL trans3( scr, maxsam, nx )
      ELSE
          CALL trans4( scr, maxsam, nx, lunt )
      ENDIF
      ndone = ndone + 1                                                 ! The number of traces processed
      IF( opad .EQ. 0 .AND. ndone .LE. bpad ) GOTO 1100
      IF( opad .NE. 0 ) THEN
        IF ( ndone .LE. bpad + 1 )
     *    CALL podisc( jhdrun, 1, 0)                                    ! Use 1st trace header for bpad traces
        IF ( ndone .GT. ntrcs - epad )
     *    CALL podisc( jhdrun, 2, -numhdr)                              ! Repeat the last trace header
      ENDIF
      CALL rddisc( jhdrun, buf, numhdr, istat )
      IF (istat.ne.numhdr) THEN
        print *,' GETNTD Error Reading Trace header'
        STOP
      ENDIF
c
c**** Time reverse the data so the data starts with time zero!!
c**** Also reinstate the deep water delay, also remember that we changed
c**** the number of samples in the data so change the header entry
c****

c**** GMK we don`t need to flip the data back sense it was not done in the first
c**** GMK place(forward modelling) so it is commented out

c      DO 1200 i = 1, maxsam
c 1200 buf(numhdr+i) = scr(maxsam-i+1)

       DO 1200 i = 1, maxsam
 1200  buf(numhdr+i) = scr(i)
      delay  = buf(46)
      nsamps = maxsam
      IF( delay .GT. 0. ) THEN
          ndelay = delay / si + .5
          nsamps = maxsam - ndelay
          DO 1250 i = 1, nsamps
 1250     buf(numhdr+i) = buf(numhdr+ndelay+i)
      ENDIF
      ibuf(58+ihdradd) = nsamps
      numdat           = nsamps                                         ! some following process might use it from common
      ibuf(15+ihdradd) = 1                                              ! All traces are live after fdmigr
      in               = 0                                              ! Mark data as not in the ap

      IF ( ntxcall .GE. nreadyy) THEN
        IF ( lunt .NE. 0 ) CALL frefil( 3, lunt, istst )                ! release scratch file
      ENDIF
      RETURN
      END
