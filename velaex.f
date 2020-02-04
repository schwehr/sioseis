      SUBROUTINE VELAEX(BUF,LBUF,IBUF,SCR,LSCR,ISCR,NREADY,ISTOP,IINRW,
     *     IFIN)
C     VELAEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS VELAN, WHICH IS THE
C  VELOCITY ANALYSIS PROCESS.  THE VELAN PARAMETERS MUST BE ON DISC FILE MUNIT
C  AND THE TRACE (WITH HEADER) MUST BE IN MEMORY LOCATION BUF.
C     SUBROUTINE VELAED CONTAINS THE EXPLANATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.  VELAEX SAVES ALL THE INPUT TRACES ON
C  DISC (WITH HEADERS) BEFORE DOING ANY WORK (THIS IS COMMN TO ALL TYPES OF
C  VELOCITY ANALYSIS), THUS, NREADY=0 ON ALL TRACES INITIALLY.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE ANALYZED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 5 APRIL 1981
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
c  mod 14 oct 89 - some velocities were done twice in semblance (those
c                  at the velocity increment change)
c  modified by pch Aug 89 to put the velocity into ibuf(46)
c  modified by GMK December 94 to output semblance file as segy, see edit phase for info
c  mod Oct 95 - don't print the spectra if vtuple(1) is 0
c  mod 6 Nov. 95 - Truncate rather than round nvels on Matlab header.
c  mod 22 Dec 97 - Change MIN0 to MIN
c  mod 13 May 1998 - Subtract 1 from the number of semlances when
c     doing the spline.  Also check for exceeding the spline arrays
c  mod 11 Jan 99 - Change number of argument to nmoapp.  Some
c    undocumented change in nmoapp!
c  mod 17 Aug 05 - Increase semscr from 2500 to 4000 so output segy
c                  semblance file can be 4000 samples.
c  mod 5 Mar 08 - Take care of endianness for Matlab
c   mod 17 May 10 - Use mute times in mils (ibuf(56 & 57)) rather than
c                   real seconds (buf(47 & 48))
c  mod 31 Aug 10 - do byte swap for segy opath
c  mod 20 Oct 10 - finish the above
c  mod 13 Jul 11 - Increase semscr from 4000 to 10000
c  mod 9 Apr 13 - remove some print statements.
C
      PARAMETER (MAXVEL=21)                                             ! /* THE MAXIMUM NUMBER OF TRACES IN A VELAED OUTPUT
      PARAMETER (NPARS=14)                                              ! /* THE LENGTH OF EACH PARAMETER LIST
      PARAMETER (MAXBOU=30)                                             ! /* THE LENGTH OF THE BOUNDS
      PARAMETER (MWRDS=(NPARS+MAXVEL+MAXBOU+3))                         ! /* THE NUMBER OF 16 BIT WORDS IN EACH DISC PAR LIST
      PARAMETER (MAXSPLINE=1000)
      DIMENSION BUF(111),LBUF(111),IBUF(111),
     &          SCR(111),LSCR(111),ISCR(111)
      DIMENSION VTUPLE(3)
      DIMENSION VELS(MAXVEL)
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps,
     & nbperw, ireal
      COMMON /VELAN/ MUNIT,NLISTS,IVELU1,IVELU2, ivelu3
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(32766)
      COMMON /readt/ itunit, numhdr, numdat
      COMMON /VELDAT/ IA(900)                                           ! /* THIS HOLDS THE TIME VS VELOCITY PLOT
      CHARACTER*132 IA
      INTEGER NRP
      CHARACTER*1 BLANK
      CHARACTER*4 TYPE
      INTEGER*2 IBUF,ISCR
      REAL char1
      SAVE
      LOGICAL FIRST,FIRSTSEGY
      DATA FIRST /.TRUE./, JLIVE/0/, BLANK/' '/
C***GMK SEMBLANCE ARRAY
      DIMENSION SEMSCR(10000)
      REAL      SEMSCR
C***GMK SPLINE ARRAYS
      REAL Xdata(MAXSPLINE), Ydata(MAXSPLINE), Y2ND(MAXSPLINE), YP1, YPN
C***GMK BOOLEAN USED TO CHECK WHETHER EBCDIC & TAPE ID HEADER WAS WRITTEN
      FIRSTSEGY = .TRUE.
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      ISIG=0
      NREADY=0                                                          ! /* SIGNAL CONTRO THAT THERE IS NO OUTPUT
      IF(IFIN.EQ.1) GO TO 190                                           ! /* IFIN=1 MEANS VELAN HAS MORE VELS TO DO IN THIS ANALYSIS
      IF(IFIN.EQ.2) RETURN                                              ! /* IFIN=2 MEANS THERE ARE NO MORE VELS
      numrp = lbuf(6)
      si = buf(49)
      delay = buf(46)
      nsamps = numdat
      IF(.NOT.FIRST) GO TO 41
      FIRST=.FALSE.
   10 CONTINUE                                                          ! /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)                                            ! /* REWIND THE PARAMETER FILE
      CALL RDDISC(MUNIT,SCR,MWRDS,ISTAT)
      ISIG=1
      LFOR=1                                                            ! /* SET THE OUTPUT RP NUMBER
      NRP=LSCR(1)
      IF(LSCR(2).EQ.1) TYPE='CVEL'
      IF(LSCR(2).EQ.2) TYPE='SPEC'
      STRETC=SCR(3)
      NVELS=LSCR(4)
      LPRINT=LSCR(5)
      WINLEN=SCR(6)+.00001                                              ! /* WATCH OUT FOR FLOATING POINT TRUNCATION
      WINLENNOTRC=SCR(6)
      CHAR1 = SCR(7)
      REFRAC=SCR(8)
      NTAPER=LSCR(9)
      TTAPER=SCR(10)
      NSEGYFILE=LSCR(11)
      DO 20 I=1,NVELS
   20 VELS(I)=SCR(I+NPARS)
      DO 40 I=1,3
   40 VTUPLE(I)=SCR(I+NPARS+MAXVEL+MAXBOU)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' type ',type
          PRINT *, (lscr(i),i=1,5),winlen,char1,refrac,ntaper,ttaper,
     *             nsegyfile
          PRINT *, (vels(i),i=1,nvels)
          PRINT *, (vtuple(i),i=1,3)
          PRINT *,ivelu1,ivelu2,ivelu3
      ENDIF
      MINDEL=16000                                                      ! /* THE MINIMUM OR SMALLEST DEEP WATER DELAY
      MAXDEL=0                                                          ! /* THE LARGEST DEEP WATER DELAY ON ANY TRACE
      MAXSAM=0                                                          ! /* THE MAXIMUM NUMBER OF SAMPLES IN ANY TRACE
      ITRCS=0                                                           ! /* THE COUNT OF THE NUMBER OF TRACES IN THE ANALYSIS
      IRPS=0                                                            ! /* THE COUNT OF THE NUMBER OF RPS IN THE ANALYSIS
C****
C****  TAKE CARE OF THE TIME TAPERS
C****
   41 IF(TTAPER.LE.0) GO TO 50
      ITAPER=TTAPER/BUF(49)                                             ! /* THE INTEGER NUMBER OF SAMPLES IN THE TAPER
      TAPER=1./FLOAT(ITAPER)                                            ! /* THE INCREMENT BETWEEN TAPER VALUES
      INDX=0                                                            ! /* START THE TAPER FROM THE BEGINNING UNLESS MUTE WAS GIVEN
c      IF(BUF(48).NE.0.) INDX=(BUF(48)-BUF(46))/BUF(49)-1                ! /* (MUTE-DELAY)/SI
      IF( ibuf(57) .NE.  0 )
     &    indx = (FLOAT(ibuf(57))/1000. - buf(46)) / buf(49) -1
      IF(IASGND.EQ.0.OR.IUSEAP.EQ.0) GO TO 45                           ! /* IS THE AP ASSIGNED
      INDX=INDX+1
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! /* MAKE SURE THE AP IS ALLOCATED
      CALL APWAIT                                                       ! /* WAIT FOR THE AP
      CALL APPUT(TAPER,NEXTAD,1,2)                                      ! /* PUT THE TAPER INCREMENT IN AP LOCATION NEXTADD
      CALL APWD                                                         ! /* WAIT FOR I/O COMPLETION
      CALL VRAMP(NEXTAD,NEXTAD,NEXTAD,1,ITAPER)                         ! /* BUILD THE TAPER IN NEXTAD
      CALL VMUL(INDX,1,NEXTAD,1,INDX,1,ITAPER)
      CALL VMUL(NSAMPS,-1,NEXTAD,1,NSAMPS,-1,ITAPER)                    ! /* DO THE BACK END
      GO TO 50
   45 DO 46 I=1,ITAPER                                                  ! /* BUILD THE TAPER IN MEMORY SCR
   46 SCR(I)=FLOAT(I)*TAPER
      DO 47 I=1,ITAPER
      BUF(NUMHDR+INDX+I)=BUF(NUMHDR+INDX+I)*SCR(I)                      ! /* DO THE FRONT
   47 BUF(NUMHDR+NSAMPS-I+1)=BUF(NUMHDR+NSAMPS-I+1)*SCR(I)
C****
C****    COLLECT ALL THE INPUT TRACES FOR THE ANALYSIS INTO DISC FILE IVELU1.
C****  EACH TRACE WILL BE NUMHDR+NSAMPS+1 32 BIT WORDS LONG ON DISC FILE IVELU1.
C****  THE EXTRA WORD IS THE NUMBER OF 32 BIT WORDS IN THE HEADER PLUS
C****  THE NUMBER OF WORDS IN THE ACTUAL TRACE.  THIS EXTRA WORD APPEARS
C****   BEFORE EACH TRACE HEADER.
C****
   50 CONTINUE
C****
C****   IF THE DATA IS TO BE ANALYZED AS A REFRACTOR (TAU-P) REMOVE THE
C****   DELAY BY INSERTING ZEROES IN THE FRONT
C****
      IF(REFRAC.EQ.0.) GO TO 59
      IDELAY=BUF(46)/BUF(49)+.5
      IBUF(55)=0                                                        ! /*  THE DELAY IN MILS
      BUF(46)=0.                                                        ! /* THE DELAY IN SECONDS
      IBUF(58)=NSAMPS+IDELAY                                            ! /* THE LENGTH OF THE DATA AFTER INSERTING THE ZEROES.
      DO 58 I=1,1000                                                    ! /* MAKE A BUFFER OF ZEROES
   58 SCR(I)=0.
   59 CONTINUE
C****
C****
      NWRDS=NUMHDR+IBUF(58)                                             ! /* THE NUMBER OF 32 BIT WORDS
C**** NSAMPS IS THE LENGTH OF THE ORIGINAL TRACE, IBUF(58) HAS THE NEW NUMBER
      IOUT=0
      IF(IBUF(15).EQ.2.AND.TYPE.NE.'CVEL') GO TO 70
C**** WRITE THE HEADER LENGTH + DATA LENGTH IN FRONT OF EVERY TRACE
      CALL WRDISC(IVELU1,NWRDS,1)
      CALL WRDISC(IVELU1,BUF,NUMHDR)
   64 IF(IDELAY.LE.0.OR.REFRAC.EQ.0.) GO TO 65                          ! /* SHOULD WE PAD TH FRONT WITH ZEROES?
c      ITEMP=MIN0(IDELAY,1000)
      ITEMP=MIN(IDELAY,1000)
      IDELAY=IDELAY-ITEMP
      CALL WRDISC(IVELU1,SCR,ITEMP)                                     ! /* WRITE THE ZEROES
      GO TO 64
   65 CONTINUE
      CALL RLSEAP(BUF(NUMHDR+1),NSAMPS)                                 ! /* GET THE TRACE OUT OF THE AP
      CALL WRDISC(IVELU1,BUF(NUMHDR+1),NSAMPS)
      NSAMPS=IBUF(58)
      IDELAY=BUF(46)/BUF(49)
      IF(IDELAY.LT.MINDEL) MINDEL=IDELAY
      IF(IDELAY.GT.MAXDEL) MAXDEL=IDELAY
      IF(NSAMPS.GT.MAXSAM) MAXSAM=NSAMPS
      ITRCS=ITRCS+1
   70 IF(LBUF(51).GE.0) RETURN                                          ! /* IS IT THE END OF GATHER SIGNAL
      IRPS=IRPS+1                                                       ! /* INCREMENT THE NUMBER OF RPS
      IF(IRPS.LT.NRP) RETURN                                            ! /* ANY MORE RPS TO GET
      IF(ISTOP.EQ.0) GO TO 80                                           ! /* ANY MORE INPUT FROM TAPE
      IF(IINRW.NE.0) CALL FREETP(ILUN,0)                                ! /* UNASSIGN THE TAPE DRIVE
      IINRW=0                                                           ! /*  TELL SUBROUTINE CONTRO THAT THE TAPE IS NOT ASSIGNED
      IF(MINDEL.NE.MAXDEL) MAXSAM=MAXSAM+MAXDEL-MINDEL
   80 CONTINUE
C****
C****     APPLY THE TAPER TO THE FIRST AND LAST NTAPER TRACES OF THE GATHER
C****
      IF(NTAPER.EQ.0) GO TO 99
      WEIGHT=1./(NTAPER+1.)
      CALL PODISC(IVELU1,1,0)
      IOUT=0                                                            ! /* SET SIGNAL TO GET TRACE OUT OF AP ON CALLS TO APRLSE
      DO 85 I=1,NTAPER
      CALL RDDISC(IVELU1,NWRDS,1,ISTAT)
      CALL RDDISC(IVELU1,BUF,NWRDS,ISTAT)
      NSAMPS=IBUF(58)
      IF(IASGND.EQ.0.OR.IUSEAP.EQ.0) GO TO 82                           ! /* IS THE AP ASSIGNED TO US?
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! /* PUT THE TRACE IN THE AP
      SCR(1)=FLOAT(I)*WEIGHT
      CALL APPUT(SCR,NEXTAD,1,2)                                        ! /* PUT THE WEIGHT IN THE AP
      CALL APWD                                                         ! /* WAIT FOR THE DATA TRANSFER
      CALL VSMUL(IN,1,NEXTAD,IN,1,NSAMPS)
      IF(IAND(LPRINT,2).NE.0) PRINT 81,LBUF(6),LBUF(7),SCR(1)
   81 FORMAT(' RP ',I6,' TRACE ',I6,' IS WEIGHTED BY ',F7.5)
      CALL RLSEAP(BUF(NUMHDR+1),NSAMPS)                                 ! /* GET THE TRACE OUT OF THE AP
      GO TO 84
   82 TEMP=FLOAT(I)*WEIGHT
      IEND=NUMHDR+NSAMPS
      ITEMP=NUMHDR+1
      DO 83 J=ITEMP,IEND
   83 BUF(J)=BUF(J)*TEMP
      IF(IAND(LPRINT,2).NE.0) PRINT 81,LBUF(6),LBUF(7),TEMP
   84 CALL PODISC(IVELU1,2,-NWRDS)                                      ! /* BACK UP NWRDS!
   85 CONTINUE
C****  NOW SKIP THE NON WEIGHTED TRACES
      N=ITRCS-NTAPER-NTAPER
      IF(N.LE.1) GO TO 88888
      DO 86 I=1,N
      CALL RDDISC(IVELU1,NWRDS,1,ISTAT)
      CALL RDDISC(IVELU1,BUF,NWRDS,ISTAT)
   86 CONTINUE
88888 CONTINUE
      DO 90 I=1,NTAPER
      CALL RDDISC(IVELU1,NWRDS,1,ISTAT)
      CALL RDDISC(IVELU1,BUF,NWRDS,ISTAT)
      NSAMPS=IBUF(58)
      IF(IASGND.EQ.0.OR.IUSEAP.EQ.0) GO TO 87                           ! /* IS THE AP ASSIGNED TO US?
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! /* PUT THE TRACE IN THE AP
      SCR(1)=FLOAT(NTAPER-I+1)*WEIGHT
      CALL APPUT(SCR,NEXTAD,1,2)                                        ! /* PUT THE WEIGHT IN THE AP
      CALL APWD                                                         ! /* WAIT FOR THE DATA TRANSFER
      CALL VSMUL(IN,1,NEXTAD,IN,1,NSAMPS)
      IF(IAND(LPRINT,2).NE.0) PRINT 81,LBUF(6),LBUF(7),SCR(1)
      CALL RLSEAP(BUF(NUMHDR+1),NSAMPS)
      GO TO 89
   87 TEMP=FLOAT(NTAPER-I+1)*WEIGHT
      ITEMP=NUMHDR+1
      IEND=NUMHDR+NSAMPS
      DO 88 J=ITEMP,IEND
   88 BUF(J)=BUF(J)*TEMP
      IF(IAND(LPRINT,2).NE.0) PRINT 81,LBUF(6),LBUF(7),TEMP
   89 CALL PODISC(IVELU1,2,-NWRDS)
   90 CONTINUE
   99 IF(TYPE.NE.'CVEL') GO TO 1000
C****
C****    CONSTANT VELOCITY NMO
C****
      IF(REFRAC.EQ.0.) GO TO 110
      IRELSE=0                                                          ! /* ALLOW THE AP TO BE RELEASED BETWEEN VELOCITIES
      GO TO 170
  110 CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   ! /* MAKE SURE THE AP IS ASSIGNED ETC.
      IF(  iuseap .NE. 0 ) CALL VCLR(NEXTAD,1,LAPSIZ-NEXTAD)            ! /* ZERO OUT THE AP, EXCEPT THE TRACE
      IX2V2=NEXTAD                                                      ! /* THE AP ADDRESS OF THE RATIO X**2/V**2
      ISI=NEXTAD+1                                                      ! /* THE AP ADDRESS OF THE SAMPLE INTERVAL
      ISI2=NEXTAD+2                                                     ! /* THE AP ADDRESS OF THE SQUARE OF THE SAMPLE INTERVAL
      ISI3=NEXTAD+3                                                     ! /* THE AP ADDRESS OF HALF THE SAMPLE INTERVAL
      IT0=NEXTAD+4                                                      ! /* THE AP ADDRESS OF THE INITIAL T0 VALUE (THE DELAY)
  170 IVEL=1                                                            ! /* THE INDEX TO THE VEL ARRAY
      VEL=VELS(IVEL)
  190 CALL PODISC(IVELU1,1,0)
      CALL PODISC(IVELU2,1,0)                                           ! /* REWIND THE OUTPUT FILE
      NTRCS=0
  200 CONTINUE
      CALL RDDISC(IVELU1,NWRDS,1,ISTAT)
      CALL RDDISC(IVELU1,BUF,NWRDS,ISTAT)
      NREADY=NREADY+1
      NTRCS=NTRCS+1
      IF(IBUF(15).EQ.2) GO TO 220
      IF(VEL.EQ.VELS(1)) JLIVE=JLIVE+1                                  ! /* COUNT THE LIVE TRACES
      X=LBUF(10)                                                        ! /* GET THE SHOT-RECEIVER DISTANCE
      IF(REFRAC.EQ.0.) GO TO 208
C****
C****  THIS IS A REFRACTION VELOCITY ANALYSIS
C****  USE T=T-X/V RATHER THAN T=SQRT(T*T-X*X/V*V)
C****  THIS ALSO TRANSFORMS THE DATA TO THE TAU-P DOMAIN!!!
C****  RATHER THAN ACTUALLY SHIFTING THE DATA, CHANGE THE START TIME OF THE
C****  DATA (THE DELAY) SO THAT THE DATA IS NEVER CUT OFF!
C****
      SHIFT=ABS(X)/VEL                                                  ! /* -X/V IS MEARLY A TIME SHIFT - CONSTANT FOR THE WHOLE TRACE!
      NSHIFT=SHIFT/BUF(49)                                              ! /* THE NUMBER OF SAMPLES TO SHIFT THE DATA
      NSAMPS=IBUF(58)
      IF(SHIFT.EQ.0.) GO TO 207                                         ! /* WAS IT TAKEN CARE OF BY THE DELAY CHANGE?
      ISTART=NUMHDR+1
      IEND=NUMHDR+NSAMPS-NSHIFT+1
      IF(NSHIFT.LE.NSAMPS) GO TO 203
      IBUF(15)=2                                                        ! /* MAKE IT A DEAD TRACE
      IEND=NUMHDR
      GO TO 205
  203 DO 204 I=ISTART,IEND
  204 BUF(I)=BUF(I+NSHIFT)
  205 CONTINUE
c  205 BUF(47)=BUF(47)-SHIFT                                             ! /* THE MUTE START TIME
c      IF(BUF(47).LT.0.) BUF(47)=0.
c      IBUF(56)=BUF(47)*1000.+.5                                          ! /* CONVERT TO MILS
c      BUF(48)=BUF(48)-SHIFT                                              ! /* CHANGE THE MUTE TIME IN THE HEADER
c      IF(BUF(48).LT.0.) BUF(48)=0.
c      IBUF(57)=BUF(48)*1000.+.5
  207 NSAMPS=NSAMPS-NSHIFT
      IBUF(58)=NSAMPS                                                   ! /* MAKE THE DATA SHORTER BY THE SHIFT
      NWRDS=NUMHDR+NSAMPS
      GO TO 218
C****
  208 CONTINUE                                                          ! /* THIS IS HYPERBOLIC NMO
      X2V2=X*X/(VEL*VEL)
      ISTRET=STRETC/SI
      SI2=SI/2.
      CONST=-BUF(46)+SI
      IF(IUSEAP.EQ.0) GO TO 209
C****
C****  DO IT IN THE AP
      SCR(1)=X2V2
      SCR(2)=SI
      SCR(3)=SI
      SCR(4)=SI/2.
      SCR(5)=DELAY
      SCR(6)=CONST                                                      ! /* SUBTRACT OUT DELAY BEFORE CONVERTING TO INDEXES
      IF(IN.EQ.0) CALL INAP(BUF(NUMHDR+1),NSAMPS)                       ! /*  THE AP SOMETIMES GET RELEASED, SO GET IT IF IT HAS
      CALL APPUT(SCR,NEXTAD,6,2)                                        ! /* PUT THE VARIABLES IN THE AP
      IAPSCR=NEXTAD+6
      CALL APWD
      CALL CVNMO(IT0,ISI,IX2V2,NSAMPS,IAPSCR,ISI2,ISI3,NEXTAD+5)
      CALL APWR
      CALL APGET(SCR,IAPSCR,NSAMPS,2)                                   ! /* GET THE NMO FROM THE AP
      CALL APWD
      DO 99209 I=1,NSAMPS
      LTEMP = SCR(I)                                                    ! /* FIX THE FLOATING INDEX
          IF( LTEMP .GT. 0 .AND. LTEMP .LE. NSAMPS) THEN
               LSCR(I) = LTEMP
          ELSE
               LSCR(I) = NSAMPS
          ENDIF
99209 CONTINUE
      GO TO 215
C****
C****   DO IT IN HOST MEMORY
  209 T=DELAY
      B=0.
      DO 210 I=1,NSAMPS
      T=DELAY+SI*B                                                      ! /* WATCH OUT FOR TRUNCATION IF YOU ADD!
      B=B+1.
      T=T*T                                                             ! /* SQUARE THE TX TIME
      TNEW=SQRT(T+X2V2)+CONST
  210 LSCR(I)=TNEW/SI+SI2+1.                                            ! /* CONVERT TO AN INDEX
C****
C****    NOW APPLY THE NMO
  215 CALL NMOAPP(BUF(NUMHDR+1),BUF(NUMHDR+1),LSCR,1,NSAMPS,
     *    ISTRET,NEWMUT,1)
  218 IF(IAND(LPRINT,2).NE.0) PRINT 219, VEL,NREADY,NTRCS,LBUF(6),
     *    LBUF(7),NSAMPS
  219 FORMAT(' CONSTANT VELOCITY ',F10.3,' BEING APPLIED.',5I10)
  220 CALL WRDISC(IVELU2,NWRDS,1)
      LBUF(6)=LFOR                                                      ! /* MAKE THE OUTPUT RP NUMBER DIFFERENT!
      ibuf(46) = vel                                                    ! /* put the constant velocity in the SEGY "weathering velocity"
      IF(NEWMUT.LE.1) GO TO 230                                         ! /* UPDATE THE NEW MUTE TIMES INTO THE TRACE HEADER
c      BUF(48)=BUF(46)+(NEWMUT-1)*BUF(49)
      temp = BUF(46)+(NEWMUT-1)*BUF(49)
      IBUF(57)=temp*1000.
  230 CALL WRDISC(IVELU2,BUF,NWRDS)
      IF(NTRCS.LT.ITRCS) GO TO 200
C****
C****  GET THE NEXT VELOCITY
C****
      VEL=VEL+VELS(IVEL+1)
      LFOR=LFOR+1
      IFIN=1
      CALL PODISC(IVELU2,1,0)                                           ! /* REWIND THE OUTPUT FILE
      IF(VEL.LT.VELS(IVEL+2).AND.IVEL.LE.NVELS) RETURN
      IVEL=IVEL+2
      VEL=VELS(IVEL)
      IF(IVEL.LE.NVELS) RETURN                                          ! /* ANY MORE VELOCITIES?
      IFIN=2
      GO TO 1200
C****
C****   SEMBLANCE VELOCITY SPECTRA
C****
 1000 CONTINUE
      MAXSAM=MAXSAM+MAXDEL-MINDEL                                       ! /* THE LARGEST TRACE
      CALL INAP(BUF(NUMHDR+1),MAXSAM)                                   ! /* ASSIGN THE AP
      IAPNMO=NEXTAD                                                     ! /* THE AP ADDRESS OF THE NMO INDEXES
      IAPSEM=NEXTAD+MAXSAM                                              ! /* THE AP ADDRESS OF THE MEAN ENERGY
      IAPSTK=IAPSEM+MAXSAM                                              ! /* THE AP ADDRESS OF THE STACKED TRACE
      IF(IAPSTK.GT.LAPSIZ-4*MAXSAM) GO TO 1003
      IAPSCR=IAPSTK+MAXSAM*2                                            ! /* THE ADDRESS OF AN AP SCRATCH ARRAY
      IAPDIV=IAPSCR+8                                                   ! /* THE AP ADDRESS OF THE STACK DIVISOR
      ISCRAP=IAPDIV+MAXSAM                                              ! /* THE AP ADDRESS OF ANOTHER AP SCRATCH ARRAY
      IF(ISCRAP+MAXSAM.LE.LAPSIZ) GO TO 1005
 1003 PRINT 1004
 1004 FORMAT(' ***  ERROR  ***  TOO MUCH DATA IN VELAN FOR THE AP.')
      STOP
 1005 CONTINUE
      IVEL=1
      ILIVE=0
      ILEN=WINLEN/BUF(49)                                               ! /* THE WINDOW LENGTH IN SAMPLES
      ICENTR=ILEN/2                                                     ! /* THE DISTANCE TO MOVE THE WINDOW
 1010 VEL=VELS(IVEL)
C****  START AT THE BEGINNING OF THE INPUT FILE
 1020 CALL PODISC(IVELU1,1,0)
      IF(IAND(LPRINT,2).NE.0) PRINT 1021,VEL
 1021 FORMAT(' DOING VELOCITY ',F10.3)
      IF(IUSEAP.EQ.0) GO TO 1030
      CALL VCLR(IAPSEM,1,MAXSAM*6+8)                                    ! /* ZERO OUT THE STACKED TRACE AND THE SEMBLANCE
      GO TO 1039
 1030 IEND=IAPSEM+MAXSAM*6+8
      DO 1035 II=IAPSEM,IEND
 1035 A(II)=0.
 1039 NTRCS=0
 1040 CONTINUE
      CALL RDDISC(IVELU1,NWRDS,1,ISTAT)
      CALL RDDISC(IVELU1,BUF,NWRDS,ISTAT)
C****  INITIALIZE THE PLOT HERE BECAUSE IT NEEDS THE TRACE HEADER OF THE
C****  TRACE CLOSEST TO THE CENTER OF THE RP.
      IF(IBUF(15).EQ.2) GO TO 1110                                      ! /* IS IT A DEAD TRACE
      IF( vtuple(1) .NE. 0 .AND. VEL .EQ. VELS(1) .AND. JLIVE .EQ. 0 )
     *    CALL INVPLT(VTUPLE,ISCR,IBUF,LBUF,BLANK)
      IF(VEL.EQ.VELS(1)) JLIVE=JLIVE+1                                  ! /* COUNT THE LIVE TRACES
      X=LBUF(10)                                                        ! /* THE SHOT-RECEIVER DISTANCE
      SI=BUF(49)                                                        ! /* THE SAMPLE INTERVAL
      IDELAY=BUF(46)/SI                                                 ! /* THE DELAY IN SAMPLES
      NSAMPS=IBUF(58)                                                   ! /* THE NUMBER OF DATA SAMPLES IN THIS TRACE
      NSAMPS=NSAMPS+IDELAY-MINDEL
      ILEN=WINLEN/SI                                                    ! /* THE WINDOW LENGTH IN SAMPLES
      ICENTR=ILEN/2                                                     ! /* THE DISTANCE TO MOVE THE WINDOW
      T0=(MINDEL+ICENTR)*SI                                             ! /* THE CENTER OF THE FIRST WINDOW
      N=NSAMPS*2*SI/WINLEN                                              ! /* THE NUMBER OF WINDOWS IN THIS TRACE
      NZEROS=IDELAY-MINDEL                                              ! /* THE NUMBER OF SAMPLES BEFORE THE DATA STARTS
      IF(REFRAC.NE.0.) GO TO 1050
      SCR(1)=T0                                                         ! /* THE INITIAL T0 TIME
      SCR(2)=WINLEN/2.                                                  ! /* HALF THE WINDOW LENGTH IN SECONDS
      SCR(3)=(X*X)/(VEL*VEL)
      SCR(4)=SI                                                         ! /*  THE SAMPLE INTERVAL
      SCR(5)=SI/2.                                                      ! /* HALF THE SAMPLE INTERVAL
      SCR(6)=-MINDEL*SI+SI                                              ! /* THE SCALAR TO ADD AFTER NMO CALCULATION
      IF(IUSEAP.EQ.0) GO TO 1041
      SCR(7)=1.
      scr(8) = char1 - 1.
      IONE=IAPSCR+6
      NINE=IAPSCR+7
      CALL APPUT(SCR,IAPSCR,8,2)
      CALL APWD
C**** COMPUTE THE CONSTANT VELOCITY NMO ON THE REAL WINDOW CENTER TIMES
      CALL CVNMO(IAPSCR,IAPSCR+1,IAPSCR+2,N,IAPNMO,IAPSCR+3,IAPSCR+4,
     *   IAPSCR+5)
      CALL APWR
      CALL APGET(SCR,IAPNMO,N,2)                                        ! /* GET THE FLOATING INDEXES OUT OF THE AP
      CALL APWD
      DO 91040 I=1,N
      LTEMP = SCR(I)                                                    ! /* FIX THE FLOATING INDEX
          IF( LTEMP .GT. 0 .AND. LTEMP .LE. NSAMPS) THEN
               LSCR(I) = LTEMP - ICENTR
          ELSE
               LSCR(I) = NSAMPS
          ENDIF
91040 CONTINUE
      GO TO 1060
C****
C****    DO IT IN THE HOST
 1041 TINC=SCR(2)                                                       ! /* TIME INCREMENT
      X2V2=SCR(3)                                                       ! /* X**2/V**2
      CONST=SCR(6)                                                      ! /* -DELAY
      B=0.
      DO 1045 II=1,N                                                    ! /* MOVE OUT THE TIMES OF THE CENTERS OF THE WINDOWS
      T=T0+TINC*B
      B=B+1.
      T=T*T
      TNEW=SQRT(T+X2V2)+CONST
      LSCR(II)=TNEW/SI-ICENTR+1                                         ! /* CONVERT TIMES TO INDEXES
 1045 CONTINUE
      GO TO 1060
C****
C****   REFRACTION ANALYSIS
C****
 1050 TINC=WINLEN/2.
      ISHIFT=(X/VEL)/SI+SI2
      DO 1055 I=1,N                                                     ! /* BUILD AN ARRAY OF INDICES
 1055 LSCR(I)=ISHIFT*(I-1)
C****
C****
 1060 TEMP=BUF(46)                                                      ! /* FIND THE FIRST LIVE SAMPLE AFTER THE DELAY
c      IF(BUF(47).GT.BUF(46)) TEMP=BUF(47)
      IF( FLOAT(ibuf(57))/1000. .GT. temp ) temp = FLOAT(ibuf(57))/1000.
      ILIVE=TEMP/SI-IDELAY+NZEROS
      ISTKAD=IAPSTK
      IDIVAD=IAPDIV-1
      ISEMAD=IAPSEM-1
      IADD=0
      IF(IUSEAP.EQ.0) GO TO 1070
      CALL APWD
      NSAMPS=IBUF(58)
      CALL APPUT(BUF(NUMHDR+1),NZEROS+1,NSAMPS,2)                       ! /* START THE DATA TRACE TO THE AP
      IF(NZEROS.GT.0) CALL VCLR(1,1,NZEROS)                             ! /* ZERO OUT THE FRONT OF THE NEW TRACE
      GO TO 1079
 1070 ITEMP=IBUF(58)
      DO 1071 I=1,ITEMP
 1071 A(NZEROS+I)=BUF(NUMHDR+I)                                         ! /* PUT THE DATA IN APMEM
      IF(NZEROS.EQ.0) GO TO 1079
      DO 1072 I=1,NZEROS
 1072 A(I)=0.                                                           ! /* ZERO OUT THE FRONT OF THE TRACE
 1079 DO 1100 I=1,N
      NEW=LSCR(I)
      IF(NEW.LT.ILIVE) GO TO 1090                                       ! /* DON'T DO A DEAD WINDOW!
      IF(NEW+ILEN.GT.NSAMPS) GO TO 1090                                 ! /* IS THE WINDOW OFF THE END OF THE TRACE
C****   COMPUTE THE STACK TRACE AND THE MEAN ENERGY
      IF(IUSEAP.EQ.0) GO TO 1080
      CALL STKSEM(NEW,ISTKAD+IADD,ISCRAP,ISEMAD+I,ILEN,IONE,IDIVAD+I)
      GO TO 1090
 1080 CALL STKSE(NEW,ISTKAD+IADD,ISEMAD+I,ILEN,IDIVAD+I)
 1090 IADD=IADD+ILEN
 1100 CONTINUE
 1110 NTRCS=NTRCS+1
      IF(NTRCS.LT.ITRCS) GO TO 1040                                     ! /* ANY MORE TRACES IN THE ANALYSIS FOR THIS VELOCITY
      IF(JLIVE.GT.1) GO TO 1130
      PRINT 1120
 1120 FORMAT(' NOT ENOUGH LIVE TRACES.')
      GO TO 1200
 1130 CONTINUE
C****
C****  FINISH THE SEMBLANCE CALCULATIONS BY FINDING THE ENERGY OF THE STACKED
C****  TRACE AND DIVIDING IT BY THE MEAN ENERGY.
      IF( IUSEAP .NE. 0 ) THEN
          CALL SEMSTK(IAPSTK,IAPSEM,IAPNMO,ISCRAP,IAPDIV,ILEN,ICENTR,N,
     *       NINE,1)
          CALL APWR
          CALL APGET(SCR,IAPNMO,N,2)                                    ! BRING IN AS FLOATING POINT
          CALL APWD
      ELSE
          CALL SEMST(IAPSTK,IAPSEM,SCR,IAPDIV,ILEN,ICENTR,N,char1-1,1)
      ENDIF
C***GMK TEST TO DETERMINE WHAT TYPE OF FILE< SEGY OR ASCII NEEDS TO BE WRITTEN
      IF( ivelu3 .NE. 0 ) THEN
          IF( NSEGYFILE .EQ. 0) THEN
              WRITE( ivelu3, '(10H velocity ,F5.0)' ) vel
              DO 1160 i = 1, n
                 WRITE( ivelu3, '(F10.6)' ) scr(i)
 1160         CONTINUE
          ELSE
              nsamps = ibuf(58)
              nsamprate = ibuf(59)
              nsemsamprate = int((winlennotrc/2.0)*1.0E+06)
              nsemsamps = int(float(nsamps*2*nsamprate)
     *                  /(1.0E+06*winlennotrc)) - 1
              ndelay = ibuf(55)
              nrp    = lbuf(6)
              IF( FIRSTSEGY ) THEN
                  ITRACE = 1
                  FIRSTSEGY = .FALSE.
                  IF( nsemsamps+10 .GT. MAXSPLINE ) THEN
                      PRINT *,' ***  ERROR  ***  Too many semblances.',
     &                    MAXSPLINE,' maximum.'
                  ENDIF
                  IF( nsegyfile .EQ. 1 ) THEN
C*** GMK WRITE OUT 3200 BYTES INTO EBCDIC HEADER
C*** WE ARE USING THE LBUF/IBUF/BUF ARRAY TO FORMAT NEW SEMBLANCE TRACE HEADERS
C*** SOME GARBAGE (HOPEFULLY USELESS) WILL BE CARRIED INO NEW HEADER, CAN DO
C*** THIS SINCE PROCESS ENDS WITH VELAN AND DOESN'T NEED TO PRESERVE OLD HEADER
C*** VALUES.
                      CALL WRDISC(ivelu3, semscr, 800)
                      DO i = 1, 200
                         ibuf(i) = 0
                      ENDDO
                      ibuf(9)  = nsamprate
                      ibuf(10) = nsamprate
                      ibuf(11) = nsamps
                      ibuf(12) = nsamps
                      ibuf(13) = 5
                      ibuf(15) = 1
C***GMK WRITE OUT TAPE ID HEADER WITH APPROPRIATE VALUES FOR SEMBLANCE
                      IF( icompt .EQ. 2 .OR. icompt .EQ. 4)
     &                    CALL swap16( buf, 200 )
                      CALL WRDISC(ivelu3, buf, 100)
                      DO i = 9, 15
                         ibuf(i) = 0
                      ENDDO
                  ENDIF
                  IF( nsegyfile .EQ. 2 ) THEN                           ! A Matlab MAT file
c*   Matlab 4 header, word 1, is m0pt, where:
c*  m - thousands digit.  0 -> little endian (PC)
c*                        1 -> big endian
c*  0 - hundreds digit.
c*  p - tens digit.  0 -> 64 bit float.  1-> 32 bit float, 2 -> 32 bit int
c*           3 -> 16 bit int, 4 -> 16 bit unsigned int, 5 -> 8 bit unsigned.
                      lbuf(1) = 1010                                    ! Sun, column oriented, single precision, binary
                      IF( icompt .EQ. 2 .OR. icompt .EQ. 4) lbuf(1)=0010
                      lbuf(2) = nsamps                                  ! number of columns
c                      lbuf(2) = n                                       ! number of columns
                      lbuf(3) = (vels(3)-vels(1))/vels(2) + 1           ! number of rows
                      lbuf(4) = 0                                       ! imaginary flag (=1->complex)
                      lbuf(5) = 4                                       ! number of characters in variable name
                      CALL wrdiscb( ivelu3, buf, 20 )
                      ia(1)(1:3) = 'vel'
                      ia(1)(4:4) = CHAR(0)
                      CALL wrdiscb( ivelu3, ia(1), 4 )
                  ENDIF
              ENDIF
          ENDIF
          IF( nsegyfile .EQ. 1 ) THEN
              nsemrptr  = ITRACE
              nsemvel   = int(vel)
              lbuf(3)   = 0
              lbuf(4)   = 0
              lbuf(6)   = nrp
              lbuf(7)   = nsemrptr
              lbuf(10)  = nsemvel
              ibuf(46)  = vel                                           ! put the velocity in the SEGY "weathering velocity"
              ibuf(55)  = ndelay
              ibuf(58)  = nsamps
              ibuf(59)  = nsamprate
              IF( icompt .EQ. 2 .OR. icompt .EQ. 4) THEN
                  CALL swap32( buf, 10 )
                  CALL swap16( ibuf(46), 1 )
                  CALL swap16( ibuf(55), 1 )
                  CALL swap16( ibuf(58), 2 )
              ENDIF
              CALL WRDISC(ivelu3, buf, 60)
          ENDIF
          IF( nsegyfile .GT. 0 ) THEN
C***GMK Spline Interpolate Semblance values back to original time
C***    series length and deltat. Use subroutine splint and spline
C***    from process logsex.f
C***GMK Calculate time value of semblance
              DO 870 IX = 1,NSEMSAMPS
                Xdata(ix) = ndelay/1000.+(ix)*(nsemsamprate/1.0E+06)
                Ydata(ix) = scr(ix)
870           CONTINUE
              YP1=1.0E+31
              YPN=1.0E+31
C***GMK Calculate 2nd derivatives required for splining
              CALL SPLINE(Xdata,Ydata,NSEMSAMPS,YP1,YPN,Y2ND)
              IF( nsamps .GT. 10000 ) THEN
                  PRINT *,' Too many output samples.  Must be < 10000.'
                  PRINT *,' Reduce the number of input samples.'
                  STOP
              ENDIF
              DO 871 IS = 1, NSAMPS
                curxval =  ndelay/1000.+(is-1)*(nsamprate/1.0E+06)
C***GMK Calculate current semblance value (curyval) for given time (curxval).
                CALL SPLINT(Xdata,Ydata,Y2ND,NSEMSAMPS,curxval,curyval)
                semscr(is) = curyval
                IF( curyval .LT. 0 ) semscr(is) = 0.                     ! spline does weird stuff with the last sample
871           CONTINUE
              IF( itrace .EQ. 1 .AND. nsegyfile .EQ. 2 ) THEN
                  semscr(1) = numrp
                  semscr(2) = delay
                  semscr(3) = si
                  semscr(4) = vels(1)
                  semscr(5) = vels(2)
              ENDIF
              IF( nsegyfile .EQ. 1 ) THEN
                  IF( icompt .EQ. 2 .OR. icompt .EQ. 4)
     &                CALL swap32( semscr, nsamps )
              ENDIF
              CALL WRDISC(ivelu3, semscr, nsamps)
          ENDIF
c            IF( nsegyfile .EQ. 2 ) CALL wrdisc( ivelu3, scr, n )
          ITRACE = ITRACE + 1
      ENDIF
      DO 1170 II=1,N
          IF( scr(ii) .LT. 0. .OR. scr(ii) .GT. char1-1. ) THEN         ! /* there might be 0 windows adding
               lscr(ii) = 0                                             ! /* because the nmo took it off the end of the trace
          ELSE
               lscr(ii) = scr(ii)                                       ! /* just fix the value
          ENDIF
 1170 CONTINUE
      IF( vtuple(1) .NE. 0 ) CALL VELPLT(LSCR,N,VEL,VTUPLE)
C****
C****  GET THE NEXT VELOCITY
      VEL=VEL+VELS(IVEL+1)
      IF(VEL.LE.VELS(IVEL+2).AND.IVEL.LE.NVELS) GO TO 1020
      IVEL=IVEL+2
      IF( IVEL .LT. NVELS ) THEN                                        ! /* ANY MORE VELOCITIES
          VEL = VELS(ivel) + VELS(IVEL+1)
          GOTO 1020
      ENDIF
      IF( vtuple(1) .NE. 0 ) THEN
          CALL PLOTVS(T0,WINLEN/2.+.00001,N,VTUPLE)
          CALL CLVPLT(LSCR,VTUPLE)
      ENDIF
 1200 CONTINUE
      IF(IUSEAP.NE.0) CALL APRLSE                                       ! /* RELEASE THE AP SINCE WE ARE PROBABLY FINISHED WITH IT
      IN=0                                                              ! /*  TELL SUBROUTINE INAP THAT THE AP IS NOT ASSIGNED
      IRELSE=0
      IASGND=0
      PRINT 1210,JLIVE
 1210 FORMAT(' THERE WERE ',I4,' LIVE TRACES IN THE ANALYSIS.')
      JLIVE=0
      CALL PODISC(IVELU1,1,0)                                           ! /* REWIND THE INPUT FILE
      ITRCS=0
      IRPS=0
      RETURN
      END
