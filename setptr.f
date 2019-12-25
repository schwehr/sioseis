      SUBROUTINE setptr
c****    Set the pointers or indices of the SEGY trace header.
c****  The problem with using constants is that the SEGY header
c****  contains 16 and 32 bit integers, whereas ANSII Fortran 77
c****  only knows INTEGER, which happens to be INTEGER*8 on the
c****  Cray!  Most routines don't really need to know the
c****  difference.  On the Cray, the header is expanded into
c****  a 200 word header and the 16 bit integer values are moved.
c****
c****  when adding something to common, add it to the end so that all
c****  subroutines don't have to be recompiled!
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr, ilagaptr,
     *                 ilagbptr
      llsegptr = 1                                                      ! TRACE SEQUENCE NUMBER WITHIN THE LINE
      lrseqptr = 2                                                      ! TRACE SEQUENCE NUMBER WITHIN THE REEL
      lshotptr = 3                                                      ! SHOT NUMBER OR STACKED NUMBER
      lshtrptr = 4                                                      ! TRACE NUMBER WITHIN THE SHOT
      lespnptr = 5                                                      ! energy source point number
      lrpnptr = 6                                                        ! RP OR CDP NUMBER
      lrptrptr = 7                                                      ! TRACE NUMBER WITHIN THE CDP
      IF( icompt .NE. 5 ) THEN
          itridptr  = 15                                                ! TRACE ID:  1= LIVE, 2=DEAD
          ifoldptr = 17                                                 ! stack fold
      ELSE
          itridptr  = 75                                                ! TRACE ID:  1= LIVE, 2=DEAD
          ifoldptr = 77                                                 ! stack fold
      ENDIF
      ldisptr = 10                                                      ! SOURCE TO RECEIVER DISTANCE - RANGE
      lwbdptr = 16                                                      ! WATER DEPTH AT THE SOURCE
      lsxcoptr = 19                                                     ! SOURCE X-COORDINATE
      lrxcoptr  = 21                                                    ! RECEIVER X-COORDINATE
      IF( icompt .NE. 5 ) THEN
          icvelptr = 46                                                 ! constant velocity
          ilagaptr = 53                                                 ! Lag time A in ms of time zero
          ilagbptr = 54                                                 ! Lag time B in ms of time zero
          idelmptr = 55                                                 ! DEEP WATER DELAY IN MS.
          istmptr = 56                                                  ! start mute time in ms
          iendmptr = 57                                                 ! end mute time in ms
          isampptr = 58                                                 ! number of samples
          isiptr = 59                                                   ! sample interval in us
          iyrptr = 79                                                   ! year of data recording
          idayptr = 80
          ihrptr = 81
          iminptr = 82
          isecptr = 83
          igmtptr = 84                                                  ! 1= local, 2=gmt
      ELSE
          icvelptr = 106                                                ! constant velocity
          idelmptr = 115                                                !     DEEP WATER DELAY IN MS.
          istmptr = 116                                                 ! start mute time in ms
          iendmptr = 117                                                ! end mute time in ms
          isampptr = 118                                                ! number of samples
          isiptr = 119                                                  ! sample interval in us
          iyrptr  = 139                                                  ! year of data recording
          idayptr = 140
          ihrptr  = 141
          iminptr = 142
          isecptr = 143
          igmtptr = 144                                                  ! 1= local, 2=gmt
      ENDIF
      ldelsptr = 46                                                       ! DEEP WATER DELAY IN SECONDS
      lsmusptr = 47                                                      ! START MUTE TIME IN SECONDS
      lemusptr = 48                                                      ! END MUTE TIME IN SECONDS
      lsisptr = 49                                                       !  SAMPLE INTERVAL IN SECONDS
      lwbtsptr = 50                                                      !  WATER BOTTOM TIME IN SECONDS
      lgatptr = 51                                                       ! end of gather flag
      lssmsptr = 52                                                      ! SMUTE START TIME IN SECONDS.
      lesmsptr = 53                                                      ! SMUTE END TIME IN SECONDS.
      lsbptr = 54                                                        ! SeaBeam slant range (closest beam depth)
      RETURN
      END
