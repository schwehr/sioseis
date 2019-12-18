      SUBROUTINE T2DEX(BUF,LBUF,IBUF,SCR,LSCR,ISCR, istop )
C     T2DEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS T2D (NORMAL
C  MOVE OUT).  THE T2D PARAMETERS MUST BE ON DISC FILE NUNIT AND THE TRACE MUST
C  BE IN MEMORY LOCATION BUF.
C     T2DEX CALCULATES THE T2D IN THE AP.  THE OUTPUT OF THE AP IS ACTUALLY THE T0
C  INDEXES SO THAT THE T2D RESULT IS THE T0 ARRAY. I.E. THE EQUATION
C  REALLY BEING SOLVED HERE IS  TX=SQRT(T0+X**2/V**2).  T0 IS ANY ARRAY OF TIME
C  VALUES EACH SEPARATED BY THE SAMPLE INTERVAL, X IS THE SHOT-RECEIVER DISTANCE
C  OF THE TRACE, AND V IS THE USER'S VELOCITY FUNCTION.  THUS, TX IS WHERE
C  THE DATA IS BEFORE T2D.
C     SUBROUTINE T2DED CONTAINS THE EXPLAINATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE T2DED, INCLUDING THE TRACE HEADER.  THE FIRST
C           DATA SAMPLE MUST BE AT TIME DELAY.  THIS IS THE FLOATING
C           POINT (REAL) TRACE ARRAY.
C  LBUF   - THE LONG INTEGER TRACE ARRAY.  THIS IS REALLY THE SAME AS BUF, BUT
C           PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING ANYTHING TO AN ARGUMENT.
C  IBUF   - THE SHORT INTEGER TRACE ARRAY.  NEEDED FOR 16 BIT TRACE HEADER
C           ADDRESSES.
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.  THEREFORE, SCR MUST
C           BE AT LEAST 56 32BIT WORDS BIG.  SCR MAY BE DESTROYED BY THE CALLING
C           ROUTINE.
C  LSCR   - THE SAME SCRATCH ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C  ISCR   - THE SAME ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JULY 1983
c  mod 12 oct 89 to fake t2dex into thinking all vtp are average, which
c      they are now, t2ded was changed to convert interval vtps into
c      average vtps.
c mod Mar 90 by pch to redo the spatial variation stuff like filtex,
c         the old code doesn't work and I can't follow it!
c mod 12 Sept 90 by pch to:
c    a)  do the conversion in the ap simulator when it's supposed to
c    b)  spatial variation was bad in some situations because the vtp1
c        array wasn't set correctly
c    c)  do the interval velocity to average velocity conversion AFTER
c        spatial variation.
c mod 19 Nov 90 by pch to set delay to sdepth.
c    d)  Change spatial variation so that the velocities of the second
c        list are also in the first list.  This hopefully insures a
c        smooth interpolation.
c  mod 22 Dec 97 - Change MIN0 to MIN and MAX0 to MAX
c  mod 6 Oct 06 - If SDEPTH = -1, Make sdepth delay*vtp(1)
c               - IF EDEPTH = -1, make edepth sdepth + nsamps*vel*osi
c               - Print min_depth and max_depth on last trace of job.
c  mod 6 Aug 08 - convert the water bottom time to water bottom depth
c  mod 20 Aug 08 - in km
c  mod 7 Jan 09 - process dead traces so the header gets converted 
c  mod 19 Nov 09 - set binhdr(31) = 6 on every trace.
c   mod 17 May 10 - Comment out creating real mute times in header words 47 - 53
C
C
      PARAMETER (MAXVTP=50)                                              /* THE MAXIMUM NUMBER OF ELEMENTS THE USER ARRAY CAN BE
      PARAMETER (MULTIV=10)
      PARAMETER (nwrds = MAXVTP+MULTIV)                                    /* THE LENGTH OF EACH PARAMETER LIST
      DIMENSION list1(nwrds), list2(nwrds), rlist1(nwrds), rlist2(nwrds)
      EQUIVALENCE (list1(1), rlist1(1) ), (list2(1), rlist2(1))
      DIMENSION BUF(111),LBUF(111),IBUF(111),
     &          SCR(111),LSCR(111),ISCR(111)
      DIMENSION vtp1(maxvtp), vtp2(maxvtp), vtp(maxvtp), vtp3(maxvtp)
      COMMON /T2DCOM/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(32766)
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
     *   SECS,LRENUM,ISRCF,IDTYPE
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      INTEGER SDEPTH,EDEPTH, fno1, fno2
      INTEGER*2 ibuf,iscr
      SAVE
      DATA llnum/0/, odelay/-1/, lvtype/0/, mlists/0/, lastno/0/
      DATA ivelad/0/, min_depth/99999/, max_depth/0/, ididit/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF( istop .LT. 0 ) THEN
          IF( ididit .EQ. 0 ) PRINT *,' Minimum depth = ',min_depth,
     &            ' max_depth = ',max_depth
c**** t2d may get called several times at the end if there was a multi-output process
          ididit = 1
          RETURN
      ENDIF
c      IF(IBUF(15).EQ.2) RETURN                                          /* IS IT A DEAD TRACE
      NSAMPS=IBUF(58)                                                    /*  GET THE NUMBER OF DATA SAMPLES FROM THE TRACE HEADER
      si = buf(49)
      delay = buf(46)
      idelay = delay / si
      idtype = 5                                                        ! signal that the output is depth
      ibinhdr(31) = 6
   10 IF( mlists .EQ. 0 ) THEN
          IF(NEXTAD.EQ.0) NEXTAD=1                                          /* T2D NEEDS TO SAVE THE VELOCITIES IN THE AP
          IF( ivelad .EQ. 0 ) THEN
              ivelad = nextad
              nextad = nextad + nsamps
          ENDIF
          CALL podisc( munit, 1, 0 )                                    ! get the first parameter list from disk
          CALL rddisc( munit, list1, nwrds, istat )
          mlists = mlists + 1
          fno1 = list1(1)
          lno1 = list1(2)
          osi = rlist1(3)
          sdepth1 = list1(4)
          edepth1 = list1(5)
          iaddwb = list1(6)
          lprint = list1(7)
          nvtp1 = list1(8)
          lvtype = list1(10)
          DO 20 i = 1, nwrds
   20     list2(i) = list1(i)
          DO 22 i = 1, nvtp1
   22     vtp1(i) = rlist1(i+10)
          IF( mlists .LT. nlists ) THEN                                 ! get the next list if there is one
              CALL rddisc( munit, list2, nwrds, istat )
              mlists = mlists + 1
              fno2 = list2(1)
              lno2 = list2(2)
              nvtp2 = list2(8)
              DO 25 i = 1, nvtp2
   25         vtp2(i) = rlist2(i+10)
          ENDIF
      ENDIF
      lnum = lbuf(3)                                                    /*  IS THE DATA ON TAPE SORTED BY SHOT
      IF( lbuf(7) .NE. 0 ) lnum = lbuf(6)                               /*  OR BY RP
c      print *,' fno1=',fno1,' fno2=',fno2,' lno1=',lno1,' lno2=',lno2,
c     &     ' lnum=',lnum,' lastno=',lastno
      IF( lnum .EQ. lastno ) GOTO 1000                                  ! is this the same shot/rp as the last time?
      IF( nlists .EQ. 1 ) GOTO 100
   30 CONTINUE
      IF( lnum .LE. lno1 ) THEN                                         ! is this before the last of list1?
          IF( lnum .LT. fno1 .AND. mlists .GT. 2) THEN                   ! is it before fno of list1?
              mlists = 0
              GOTO 10                                                   ! start from the beginning
         ENDIF
         GOTO 100                                                       ! use list1
      ENDIF
      IF( lnum .LT. fno2 ) GOTO 200                                     ! spatially vary the sucker
      DO 70 i = 1, nwrds                                                ! move list2 to list1
         list1(i) = list2(i)
   70 CONTINUE
      fno1 = list1(1)
      lno1 = list1(2)
      DO 75 i = 1, nvtp1
   75 vtp1(i) = rlist1(i+multiv)
      IF( mlists .LT. nlists ) THEN                                     ! get the next list if there is one
          CALL rddisc( munit, list2, nwrds, istat )
          mlists = mlists + 1
          fno2 = list2(1)
          lno2 = list2(2)
          nvtp2 = list2(8)
          DO 80 i = 1, nvtp2
   80     vtp2(i) = rlist2(i+multiv)
          GOTO 30
      ENDIF
c****
c****   Use list1
c****
  100 CONTINUE                                                          /*  THE CURRENT SHOT (RP) IS >= LNO
      osi = rlist1(3)
      sdepth1 = list1(4)
      edepth1 = list1(5)
      iaddwb = list1(6)
      lprint = list1(7)
      nvtp1 = list1(8)
      lvtype = list1(10)
      DO 140 i = 1, maxvtp
         vtp1(i) = rlist1(multiv+i)
         vtp(i) = rlist1(multiv+i)
  140 CONTINUE
      nvtps = nvtp1
      GOTO 1000
c****
c****   Interval and Average velocity functions have different 
c****  interpolation schemes.  Interval is by intervals and average
c****  is by "iso-velocity".
c****
  200 CONTINUE
      IF( lvtype .NE. 1 ) GO TO 500
      IF( nvtp1 .NE. nvtp2 ) THEN
          PRINT *,' ***  ERROR  ***  Interval velocity spatial ',
     &      'interpolation requires the control points to have the ',
     &      'same number of intervals.'
          STOP
      ENDIF
      DO 300 i = 1, nvtp1
         vtp(i) = vtp1(i) + (vtp2(i)-vtp1(i)) * (lnum-lno1) /(fno2-lno1)
  300 CONTINUE
      nvtps = nvtp1
      GOTO 1000
c****
C****     THE VELOCITY VARIATION IS BY ISO-VELOCITY
C****      (FIND EQUAL VELOCITIES THEN INTERPOLATE THE TIMES),
c****     but first make sure all velocities are included
C****
  500 CONTINUE
      DO 510 i = 1, nvtp1
  510 vtp3(i) = vtp1(i)
      nvtp3 = nvtp1
      DO 600 i2 = 1, nvtp2,2
         DO 520 i3 = 1, nvtp3, 2
            IF( vtp2(i2) .EQ. vtp1(i3) ) GOTO 600
  520    CONTINUE
         DO 560 i3 = 1, nvtp3, 2
             IF( vtp2(i2) .GE. vtp3(i3) ) GOTO 560
             DO 530 i = 1, nvtp3
  530        scr(i) = vtp3(i)
             DO 540 i = 1, i3-1
  540        vtp3(i) = scr(i)
             vtp3(i3) = vtp2(i2)
             vtp3(i3+1) = -99999.
             DO 550 i = i3, nvtp3
  550        vtp3(i+2) = scr(i)
             nvtp3 = nvtp3 + 2
             GOTO 600
  560    CONTINUE
  600 CONTINUE
c**** now find what time the new velocity occurs in list 1
      DO 650 i = 3, nvtp3, 2
         IF( vtp3(i+1) .LT. 0. ) THEN
             DO 640 j = i+2, nvtp3, 2
                IF( vtp3(j+1) .LT. 0 ) GOTO 640
                vtp3(i+1) =
     &              ( vtp3(j+1)-vtp3(i-1) ) / ( vtp3(j)-vtp3(i-2) )
     &              * ( vtp3(i) - vtp3(i-2) ) + vtp3(i-1)
                GOTO 650
  640        CONTINUE
             nvtp3 = i - 1
             GOTO 660
         ENDIF
  650 CONTINUE
  660 CONTINUE
      DO 700 I = 1, NVTP3, 2                                            /*  FIND THE VELOCITY IN THE first LIST
         v1 = vtp3(i)
         t1 = vtp3(i+1)
         DO 670 j = 1, NVTP2, 2                                         /* FIND THE VELOCITY IN THE next LIST
            v2 = vtp2(j)
            t2 = vtp2(j+1)
c       print *,' i=',i,' v1=',v1,' t1=',t1,' j=',j,' v2=',v2,' t2=',t2
c      print *,' fno1=',fno1,' fno2=',fno2,' lno1=',lno1,' lno2=',lno2,
c     &     ' lnum=',lnum,' lastno=',lastno
            IF( v1 .EQ. v2 ) THEN
                v = v1
                t = (t2 - t1) / (fno2 - lno1) * (lnum - lno1) + t1
                GO TO 680
            ENDIF
            IF( v1 .LT. v2 ) THEN   
                IF( j .EQ. 1 ) THEN
                    v = v1
                    t = (t2 - t1) / (fno2 - lno1) * (lnum - lno1) + t1
                    GOTO 680
                ELSE
                    v = v1
c                   interpolate temporally to find t2
                    t2 = (vtp2(j+1) - vtp2(j-1)) * 
     &                   (v1 - vtp2(j-2)) / (vtp2(j) - vtp2(j-2)) +
     &                   vtp2(j-1)
c                   interpolate spatially
                    t = (t2 - t1) / (fno2 - lno1) * (lnum - lno1) + t1
                    GOTO 680
                ENDIF
            ENDIF
  670    CONTINUE
c        the last velocity of list1 is bigger than the last velocity of list2
         v = v1
         t = (t2 - t1) / (fno2 - lno1) * (lnum - lno1) + t1
  680    CONTINUE
         vtp(i) = v
         vtp(i+1) = t
  700 CONTINUE
      nvtps = nvtp3
c**** The first velocities may not be the same, so interpolate both the
c**** velocity and the time!
      vtp(1) = vtp1(1) + (vtp2(1)-vtp1(1)) * (lnum-lno1) / (fno2-lno1)
      vtp(2) = vtp1(2) + (vtp2(2)-vtp1(2)) * (lnum-lno1) / (fno2-lno1)
C****
C****      CONVERT TO DEPTH USING INTERVAL VELOCITIES
C****
 1000 CONTINUE
      IF( iaddwb .EQ. 1 ) THEN
          DO 1010 i = 2, nvtps, 2
 1010     vtp(i) = vtp(i) + buf(50)
      ENDIF
      ISCR1=1
      ISCR2=ISCR1+NSAMPS
      MSAMPS=NSAMPS                                                      /* SAVE A FEW THINGS TO CHECK AGAINST THE NEXT TRACE'S VELOCITY FUNCTION
      sdepth = sdepth1
      IF( sdepth1 .EQ. -1. ) sdepth = delay * vtp(1)
c**** convert the water bottom time to water bottom depth in km
      buf(50) = buf(50) * vtp(1) / 1000.
      edepth = edepth1
      IF( edepth1 .EQ. -1. ) 
     &    edepth = sdepth + REAL(nsamps) * si * vtp(1)
c**** change the delay and number of samples on every trace
      nout = NINT((edepth - sdepth ) / osi )
      odelay = sdepth
c**** keep track of the smallest and largest depth
      min_depth = MIN0(min_depth,sdepth)
      max_depth = MAX0(max_depth,edepth)
c     lvtype = 1 = interval velocity
c     lvtype = 2 = average velocity
      IF( lvtype .EQ. 1 ) THEN
          IF(IUSEAP.EQ.1) GO TO 3000                                    /* GO TO 3000 IF THERE IS AN AP
          IF( IAND(lprint,2) .NE. 0 ) THEN
              PRINT *,' nsamps=',nsamps,' delay=',delay,' si=',si,
     *                ' nout=',nout,' sdepth=',sdepth,' osi=',osi
     *                ,' iasgnd=',iasgnd,' in=',in,' nvtps=',nvtps
              PRINT *,' vtp =',(vtp(i),i=1,nvtps)
          ENDIF
          IF( in .EQ. 0 ) THEN                                          ! is the data in the ap simulator?
              CALL t2dint( BUF(NUMHDR+1), NSAMPS, DELAY, SI, SCR(ISCR2),
     &                     NOUT, SDEPTH, OSI, SCR(ISCR1), vtp, nvtps )
              ITEMP = ISCR2-1
              DO 1200 I=1,NOUT                                          /* MOVE THE DATA INTO THE OUTPUT BUFFER
 1200         BUF(NUMHDR+I)=SCR(ITEMP+I)
          ELSE
              CALL t2dint( a(in), NSAMPS, DELAY, SI, SCR(ISCR2),
     &                     NOUT, SDEPTH, OSI, SCR(ISCR1), vtp, nvtps )
              ITEMP = ISCR2-1
              DO 1210 I=1,NOUT                                          /* MOVE THE DATA INTO THE OUTPUT BUFFER
 1210         a(in+i-1) = SCR(ITEMP+I)
          ENDIF
          GO TO 9000
      ENDIF
C****
C****      CONVERT TO DEPTH USING AVERAGE VELOCITIES
C****
      IF(IUSEAP.EQ.1) GOTO 2500                                         /* GO TO 2500 IF THERE IS AN AP
      IF( nvtps .EQ. 2 ) THEN
          VTP(3) = VTP(1)
          VTP(4)=100.
          nvtps = 4
      ENDIF
      VTP(nvtps+1)=-1.                                                  /* IVELT WANTS VTP'S TO END WITH A -1
      VTP( nvtps+2 ) = -1.
      VTP( nvtps+3 ) = -1.
      CALL IVELT(VTP,A(IVELAD),DELAY,SI,NSAMPS)                          /* BUILD IT IN THE AP SIMULATOR
      IF(IAND(LPRINT,2).NE.0) PRINT *,LNUM,' vtp=',(VTP(I),I=1,NVTPS)
C****
C****      DO THE T2D, WITH THE ONE WAY VELOCITY IN THE AP SIMULATOR
C****
 2120 CONTINUE
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' nsamps=',nsamps,' delay=',delay,' si=',si,
     *    ' nout=',nout,' sdepth=',sdepth,' osi=',osi
     *  ,' iasgnd=',iasgnd,' in=',in
      ENDIF
      IF( in .EQ. 0 ) THEN                                              ! is the data in the ap simulator?
          CALL T2D(BUF(NUMHDR+1),NSAMPS,DELAY,SI,SCR(ISCR2),NOUT,SDEPTH,
     *         OSI,SCR(ISCR1),A(IVELAD))
          ITEMP=ISCR2-1
          DO 2200 I=1,NOUT                                              /* MOVE THE DATA INTO THE OUTPUT BUFFER
 2200     BUF(NUMHDR+I)=SCR(ITEMP+I)
      ELSE
          CALL T2D( a(in),NSAMPS,DELAY,SI,SCR(ISCR2),NOUT,SDEPTH,
     *         OSI,SCR(ISCR1),A(IVELAD))
          ITEMP=ISCR2-1
          DO 2210 I=1,NOUT                                              /* MOVE THE DATA INTO THE OUTPUT BUFFER
 2210     a(in+i-1) = SCR(ITEMP+I)
      ENDIF
      GO TO 9000
C****
C****   DO THE AVERAGE VELOCITY DEPTH CONVERSION IN THE AP
C****
 2500 CONTINUE
      CALL INAP(BUF(NUMHDR+1),NSAMPS)
      V1=VTP(1)
      DO 2510 I=2,NVTPS,2
      IF(VTP(I).GE.DELAY) GO TO 2510                                    /* FIND THE FIRST APPLICABLE VELOCITY
      VTP(I)=DELAY                                                      /* DON'T DO STUFF BEFORE THE DELAY!
      V1=VTP(I-1)                                                        /* FIND THE FIRST VELOCITY
 2510 VTP(I)=(VTP(I)-DELAY)/SI+1                                        /* CONVERT TIME TO AN INDEX
      IAPSCR=NEXTAD
      IV=IAPSCR+10
      IT=IV+NSAMPS
      CALL APPUT(V1,IV,1,2)                                              /* PUT THE INITIAL VELOCITY IN THE AP
      N=VTP(2)
      CALL APWD
      IF(N.GT.0) CALL VFILL(IV,IV,1,N)
      IAPADR=IV+N
      IF(NVTPS.EQ.2) GO TO 2600
      DO 2550 I=3,NVTPS,2
      VINC=(VTP(I)-VTP(I-2))/(VTP(I+1)-VTP(I-1))
      CALL APPUT(VINC,IAPSCR,1,2)
      N=VTP(I+1)-VTP(I-1)+1
      IF(N.LE.0) GO TO 2550
      CALL APWD
      CALL VRAMP(IAPADR-1,IAPSCR,IAPADR-1,1,N)
      IAPADR=IAPADR+N-1
 2550 CONTINUE
 2600 ITEMP=IV+NSAMPS
      IF(IAPADR.LT.ITEMP) CALL VFILL(IAPADR-1,IAPADR,1,ITEMP-IAPADR+1)
      SCR(1)=DELAY
      SCR(2)=SI
      SCR(3)=1./OSI
      SCR(4)=SDEPTH
      SCR(5)=EDEPTH
      CALL APPUT(SCR,IAPSCR,5,2)
      NOUT=(EDEPTH-SDEPTH)/OSI+.5
      CALL APWD
      CALL VRAMP(IAPSCR,IAPSCR+1,IT,1,NSAMPS)
      CALL VMUL(IV,1,IT,1,IV,1,NSAMPS)
      CALL VCLIP(IV,1,IAPSCR+3,IAPSCR+4,IV,1,NSAMPS)                    /* MAKE ALL DEPTHS BETWEEN SDEPTH AND EDEPTH
      CALL VSMUL(IV,1,IAPSCR+2,IV,1,NSAMPS)                              /* DIVIDE BY THE OUTPUT SAMPLE INTERVAL
      CALL VCLR(IT,1,NOUT)                                               /* CLEAR THE OUTPUT ARRAY IN CASE THERE ARE SOME HOLES
      ISTART=IT+(DELAY*V1-SDEPTH)/OSI                                    /* FIND THE FIRST INDEX TO THE OUTPUT
      CALL VINDEX(IN,IV,1,ISTART,1,NSAMPS)
      CALL VMOV(IT,1,IN,1,NOUT)
      GO TO 9000
C****
C****
C****   CONVERT TO DEPTH USING INTERVAL VELOCITIES IN THE AP
C****
C****
 3000 CONTINUE
      IF(ODELAY.NE.-1.) GO TO 3010                                      /* WHAT IS THE OUTPUT DELAY? THE FIRST SAMPLE?
      DO 3005 I=1,NVTPS,2
      IF(DELAY.GT.VTP(I+1)) GO TO 3005
      ODELAY=DELAY*VTP(I)
      GO TO 3010
 3005 CONTINUE
 3010 CONTINUE
      IF(VTP(NVTPS).LT.DELAY+NSAMPS*SI) 
     &   VTP(NVTPS) = DELAY + NSAMPS * SI                                  /* FORCE THE LAST TIME TO BE AS LONG AS THE DATA
      IF( IAND(LPRINT,2) .NE. 0 )
     &    PRINT 3001,LNUM,LTRCNO,(VTP(I),I=1,NVTPS)
 3001 FORMAT(' SHOT/RP ',I5,' TRACE ',I3,' VTP ',5(/,16(1X,F10.4)))
      CALL APPUT(VTP,NEXTAD,NVTPS,2)                                    /* PUT THE VTPS IN THE AP
      IV=NEXTAD                                                          /* THE AP ADDRESS OF THE VTP ARRAY
      IT=NEXTAD+1                                                        /* THE AP ADDRESS OF THE FIRST TIME IN THE VTP ARRAY
      SCR(1)=0.0                                                        /*  THE TIME BETWEEN THE TIME OF THE FIRST SAMPLE AND THE DELAY!
      ITO=NEXTAD+NVTPS                                                  /* THE AP ADDRESS OF THE TIME OF THE FIRST SAMPLE
      SCR(2)=SI
      ITSI=ITO+1                                                        /* THE AP ADDRESS OF THE TIME SAMPLE INTERVAL
      SCR(3)=-ODELAY                                                    /* NEGATIVE OF THE FIRST DEPTH TO OUTPUT
      IDO=ITSI+1                                                        /* THE AP ADDRESS OF THE OUTPUT DELAY
      SCR(4)=1./OSI                                                      /* THE RECIPROCAL OF THE OUTPUT SAMPLE INTERVAL
      IOSI=IDO+1                                                        /* THE AP ADDRESS
      SCR(5)=1.5                                                        /* HALF FOR ROUNDING AND 1. FOR ADDITIVE INDEX
      IHALF=IOSI+1                                                      /* THE AP ADDRESS OF A HALF
      SCR(6)=1                                                          /* THE LOW BOUND FOR AN INDEX TO THE TIME TRACE
      ILOW=IHALF+1                                                      /* THE AP ADDRESS
      SCR(7)=NSAMPS                                                      /* THE NUMBER OF TIME SAMPLES
      IHIGH=ILOW+1                                                      /* THE AP ADDRESS OF THE UPPER BOUND FOR AN INDEX TO THE TIME SERIES
      CALL APPUT(SCR,ITO,7,2)                                            /* PUT THESE CONSTANTS IN THE AP
      ITEMP=IHIGH+1                                                      /* NEED A SCRATCH WORD IN THE AP
      ID=ITEMP+1                                                        /* THIS AP ARRAY WILL CONTAIN THE THE DEPTHS
      IAPSCR=ID+NSAMPS                                                  /* ANOTHER AP SCRATCH ARRAY
      ISTART=ID                                                          /* THE AP ADDRESS OF THE START OF EACH INTERVAL (LAYER)
      N=(VTP(2)-DELAY)/SI+1                                              /* THE NUMBER OF TIME SAMPLES IN THE FIRST LAYER
c      N=MIN0(N,NSAMPS)                                                  /* DON'T DO TOO MANY!!
      N=MIN(N,NSAMPS)                                                   /* DON'T DO TOO MANY!!
      CALL APWD
      CALL VRAMP(ITO,ITSI,ID,1,N)                                        /* T0, T0+SI, TO+2*SI, TO+3*SI, ....
      CALL VSMUL(ISTART,1,IV,ISTART,1,N)                                /* MULTIPLY TIME BY VELOCITY FOR THE FIRST LAYER
      NTODO=NSAMPS-N
      IF(NVTPS.EQ.2) GO TO 3051
      DO 3050 I=3,NVTPS,2
      IF(NTODO.LE.0) GO TO 3050
      IV=IV+2                                                            /* POINT TO THE NEXT VELOCITY IN THE AP
      ISTART=ISTART+N                                                    /* THE START OF THE NEXT TIME LAYER
      ILAST=ISTART-1                                                    /* THE AP ADDRESS OF THE DEPTH OF THE LAST LAYER
      N=(VTP(I+1)-VTP(I-1))/SI+.5                                        /* THE NUMBER OF SAMPLES IN THIS LAYER
c      N=MIN0(N,NTODO)
      N=MIN(N,NTODO)
      CALL VRAMP(ITSI,ITSI,ISTART,1,N)                                  /* SI, 2*SI, 3*SI, ....
      CALL VSMUL(ISTART,1,IV,ISTART,1,N)                                 /* MULTIPLY BY THE VELOCITY OF THIS LAYER
      CALL VSADD(ISTART,1,ILAST,ISTART,1,N)                              /* ADD IN THE DEPTH OF THE PREVIOUS LAYER
      NTODO=NTODO-N
 3050 CONTINUE
      CALL VSMUL(ID,1,IOSI,ID,1,NSAMPS)                                  /* DIVIDE BY THE OUTPUT SAMPLE INTERVAL
 3051 CONTINUE
      CALL VSADD(ID,1,IHALF,ID,1,NSAMPS)                                /* ADD .5
      CALL VCLIP(ID,1,ILOW,IHIGH,ID,1,NSAMPS)                            /* MAKE SURE THE INDEXES ARE IN BOUNDS
      CALL VINDEX(IN,ID,1,IAPSCR,1,NSAMPS)                              /* MOVE THE TIME ARRAY TO THE DEPTH ARRAY
      NOUT=(EDEPTH-ODELAY)/OSI+.5+1                                      /* THE DEPTH TRACE LENGTH
      CALL VFIX32(ID,1,ID,1,NSAMPS)                                      /* CONVERT TO 32 BIT INTEGERS
      CALL APWR
      CALL APGET(LSCR(NSAMPS+1),ID,NSAMPS,0)                            /* GET THE 32 BIT INDEXES OUT OF THE AP
      DO 3060 I=1,NSAMPS                                                /* PRESET THE OUTPUT DEPTH ARRAY TO INTEGER -999999 SO THAT
 3060 LSCR(I)=-999999                                                    /* AFTER T2D CONVERSION WE CAN SEE IF ANY DEPTH SAMPLES WERE MISSED.
      CALL APWD
      DO 3070 I=1,NSAMPS
 3070 SCR(LSCR(NSAMPS+I))=BUF(NUMHDR+I)                                  /* MOVE THE TIME DATA TO DEPTH DATA
      ISINDX=1                                                          /* THIS ALWAYS INDEXES THE SCR ARRAY
      IBINDX=1                                                          /* THIS ALWAYS INDEXES THE BUF ARRAY
      N2MOVE=LSCR(NSAMPS+NSAMPS-1)                                      /* THE DEEPEST DEPTH
      IF(N2MOVE.GT.1) GO TO 3075
c      PRINT 3071,(VTP(I),I=1,NVTPS)
      ITEMP=(VTP(2)-DELAY)/SI+1
c      PRINT 3073, ITEMP,VTP(2),DELAY,SI
 3073 FORMAT(1X,I7,3F15.7)
 3071 FORMAT(10(1X,F10.3))
      N2MOVE=0
      DO 3072 I=1,NSAMPS
c 3072 N2MOVE=MAX0(N2MOVE,LSCR(NSAMPS+I))
 3072 N2MOVE=MAX(N2MOVE,LSCR(NSAMPS+I))
 3075 CONTINUE
      N=(VTP(1)*DELAY)/OSI-ODELAY                                        /* THE NUMBER OF POINTS BEFORE THE FIRST OUTPUT POINT
      IF(N.LE.0) GO TO 3090
      DO 3080 I=1,N                                                      /* ZERO OUT ANY DEPTH DATA THAT IS BEFORE THE FIRST TIME DATA
 3080 BUF(NUMHDR+I)=0.0
      IBINDX=IBINDX+N                                                    /* THESE COUNT IN THE NUMBER OF OUTPUT!
 3090 CONTINUE
      NDONE=0
 3095 IF(LSCR(ISINDX).NE.-999999) GO TO 4000                            /* IS THIS A WHOLE THE DEPTH DATA?
      BUF(NUMHDR+IBINDX)=0.0                                             /* ZERO THE DATA BEFORE THE FIRST GOOD DATA
      ISINDX=ISINDX+1
      IBINDX=IBINDX+1
      IF(IBINDX.LE.NOUT) GO TO 3095                                      /* ARE WE FINISHED?
 4000 CONTINUE
      IF(LSCR(ISINDX).NE.-999999) GO TO 4010
      BUF(NUMHDR+IBINDX)=BUF(NUMHDR+IBINDX-1)                            /* USE THE LAST SAMPLES RATHER THAT INTERPOLATE
      GO TO 4020
 4010 BUF(NUMHDR+IBINDX)=SCR(ISINDX)                                    /* JUST MOVE THE DATA
 4020 ISINDX=ISINDX+1
      IBINDX=IBINDX+1
      NDONE=NDONE+1
      IF(IBINDX.LE.NOUT.AND.ISINDX.LE.NSAMPS.AND.NDONE.LE.N2MOVE)
     *   GO TO 4000
      N=NOUT-IBINDX
      IF(N.LE.0) GO TO 4050
      DO 4040 I=1,N
      BUF(NUMHDR+IBINDX)=0.0                                            /* ZERO FILL THE END OF THE DEPTH TRACE
 4040 IBINDX=IBINDX+1
 4050 CONTINUE
      GO TO 9000
C****
C****  set the SEGY header.  Use meters instead of milliseconds and
c****  kilometers instead of seconds
C****
 9000 IBUF(58)=NOUT                                                     /* CHANGE THE TRACE HEADER INFO SINCE WE'VE CHANGED
      NUMDAT=NOUT                                                       /* THE OUTPUT NUMBER OF SAMPLES AND THE OUTPUT SAMPLE INTERVAL
      IBUF(59) = OSI * 1000.
      BUF(46) = ODELAY/1000.                                            /* THE DELAY in KM
      BUF(49) = OSI / 1000.
      IBUF(55) = ODELAY                                                 /* THE OUTPUT DEEP WATER DELAY IN MILS
      IBUF(56)=0                                                        /* ZERO THE MUTE TIMES
      IBUF(57)=0
c      BUF(47)=0.
c      BUF(48)=0.
c      BUF(52)=0.
c      BUF(53)=0.
      IDTYPE = 6
      IF( istop .NE. 0 ) THEN
          PRINT *,' Minimum depth = ',min_depth,
     &            ' max_depth = ',max_depth
          ididit = 1
          RETURN
      ENDIF
      RETURN
      END
