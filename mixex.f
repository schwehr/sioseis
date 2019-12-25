      SUBROUTINE MIXEX(BUF,LBUF,IBUF,SCR,LSCR, nready)
C     MIXEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS MIX, WHICH MIXES
C  SEVERAL ADJACENT TRACES.  THE MIX PARAMETERS MUST BE ON DISC FILE MUNIT
C  AND THE TRACE (WITH HEADER) MUST BE IN MEMORY LOCATION BUF.
C     SUBROUTINE MIXED CONTAINS THE EXPLANATION OF THE USER PARAMETERS AND THE
C  ORDER OF THE USER PARAMETERS ON DISC.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE MIXED, INCLUDING THE TRACE HEADER.  THE FIRST
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
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 12 MARCH 1981
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c  MODS:
c  22 March 1991 by pch to add the Type parameter and record mix
c      and add argument nready
c  7 July 1992 by pch to honor lno
c  13 July 93 by pch to ignore the mute times if they are less than the
c       delay (which will screw up the buffer indices, but can only
c       happen if the mute times are in error anyway!)
c  mod 22 Dec 97 - Change MIN0 to MIN and MAX0 to MAX
c  mod 20 Mar 98 - Allow very large delay (in samples) by preseting
c                  mindel to 1000000 from 10000
c  mod 27 Jun 00 - Add HDR, LHDR, IHDR
c  mod 16 Nov 00 - Above change caused ndone to be incremented at
c                  the wrong place, which then ended up causing
c                  an index to go bad and segmentation fault
C
C
      PARAMETER (MAXMIX=10)                                             ! THE MAXIMUM NUMBER OF TRACES IN A MIXED OUTPUT
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      INTEGER*4 lhdrs(100), ihdrs(100)
      REAL hdrs(100)
      DIMENSION ITABLE(MAXMIX,7),WEIGHT(100)
C      ITABLE IS COMPOSED OF 10 ROWS, ONE FOR EACH TRACE TO BE MIXED, AND
C      6 COLUMNS, WHERE
C      COLUMN 1 = THE TRACE NUMBER WITHIN THE MIX.
C      COLUMN 2 = THE AP ADDRESS OF THE TRACE. A NEGATIVE ADDRESS MEANS THERE IS
C                 NOT A TRACE IN THE AP AT THE ADDRESS.
C      COLUMN 3 = THE TRACE ID.  =2 MEANS A DEAD TRACE.
C      COLUMN 4 = THE DELAY, IN SAMPLES, OF THE FIRST DATA SAMPLE.
C      COLUMN 5 = THE NUMBER OF SAMPLES IN THE TRACE.
C      COLUMN 6 = THE MUTE, IN SAMPLES (FROM ZERO TIME)
      COMMON /MIXR/ MUNIT,NLISTS, nwrds
      COMMON /WRITET/ MOUNIT,MSAMPS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(32766)
      COMMON /READT/ ILUN,NUMHDR,numdat
      INTEGER FNO, type, hdr, ihdr, lhdr
      SAVE
      LOGICAL FIRST
      DATA FIRST /.TRUE./, NDONE/0/, last/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      nready = 1
      ISIG=0
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          ! GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)
      CALL RDDISC(MUNIT,SCR,nwrds,ISTAT)
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      JDIP=SCR(3)/BUF(49)                                               ! CONVERT SECONDS TO SAMPLES
      NWEIGS=LSCR(4)
      LPRINT=LSCR(5)
      type = lscr(7)
      MAXSAM = NINT(SCR(6)/BUF(49))                                     ! CONVERT TO SAMPLES
      hdr = lscr(8)
      lhdr = lscr(9)
      ihdr = lscr(10)
      DO I=1,NWEIGS
         WEIGHT(I)=SCR(I+10)
      ENDDO
      MLISTS=1
      IFREE=IFREE+1                                                     ! SIGNAL THAT WE SAVE THINGS IN THE AP FOR THE NEXT CALL
      NSAMPS = numdat
      IF( scr(6) .EQ. 0 ) THEN                                          ! did the user give maxlen?
          maxsam = nsamps
      ELSE
          nsamps = maxsam
      ENDIF
      CALL INAP(BUF(NUMHDR+1),NSAMPS)
      LTEMP=NEXTAD+MAXSAM*NWEIGS+2                                      ! IT MAY EXCEED 16 BITS!!!
      IF(LTEMP.GE.LAPSIZ) GO TO 35
      IF( type .NE. 4 ) THEN
          DO I=1,NWEIGS                                                 ! PRESET THE AP ADDRESSES ETC IN THE ITABLE ARRAY
             ITABLE(I,2)=-NEXTAD
             NEXTAD=NEXTAD+MAXSAM
          ENDDO
      ENDIF
      IF(NEXTAD+2*MAXSAM.LE.LAPSIZ) GO TO 50
   35 PRINT 40
   40 FORMAT(' ***  ERROR  ***  NOT ENOUGH AP DATA MEMORY TO PERFORM ',
     *   'MIX.')
      PRINT 41
   41 FORMAT(' DECREASE THE NUMBER OF WEIGHTS OR DECREASE THE ',
     *   'DATA LENGTH PROCESSED OR DECIMATE THE DATA.')
      STOP
   50 CONTINUE
      LNUM=LBUF(3)                                                      !  IS THE DATA ON TAPE SORTED BY SHOT
      TRNO=LBUF(4)                                                      ! THE TRACE NUMBER WITHIN THE SHOT
      IF(LBUF(7).NE.0) TRNO=LBUF(7)                                     ! THE TRACE NUMBER WITH AN RP
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                     !  OR BY RP
   70 IF( LNUM .LT. FNO ) THEN                                          ! IS THIS SHOT BEFORE THIS PARAMETER LIST
          IF( MLISTS .EQ. 1 ) RETURN                                    ! IS IT BEFORE THE FIRST LIST
          IF( LNUM .LE. LASTNO ) GOTO 10                                ! IS IT IN OR BEFORE THE LAST LIST
          RETURN                                                        ! IF THE SHOT IS NOT SPECIFIED, LEAVE IT ALONE
      ENDIF
      IF( LNUM .LE. LNO ) GOTO 500                                      ! USE THE PARAMETERS OF THIS LIST
      IF( MLISTS .GE. NLISTS ) RETURN                                   ! ANY MORE USER PARAM LISTS ON DISC
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
      LASTNO=LNO
      CALL RDDISC(MUNIT,SCR,nwrds,ISTAT)
      ISIG=1
      MLISTS=MLISTS+1
      FNO=LSCR(1)
      LNO=LSCR(2)
      JDIP=SCR(3)/BUF(49)                                               ! CONVERT SECONDS TO SAMPLES
      NWEIGS=LSCR(4)
      LPRINT=LSCR(5)
      type = lscr(7)
      MAXSAM = NINT(SCR(6)/BUF(49))                                     /* CONVERT TO SAMPLES
      hdr = lscr(8)
      lhdr = lscr(9)
      ihdr = lscr(10)
      DO I=1,NWEIGS
         WEIGHT(I)=SCR(I+10)
      ENDDO
      GO TO 70
C****
C****   DO THE MIX
C****
  500 CONTINUE
      IF( type .EQ. 2 ) THEN                                            ! a running record mix
          IF( lbuf(7) .EQ. 0 .AND. last .NE. lbuf(3) ) THEN
              ndone = 0
              DO 505 i = 1, maxmix
                 IF( itable(i,2) .GT. 0 ) itable(i,2) = -itable(i,2)
  505         CONTINUE
          ENDIF
      ENDIF
      IF( type .EQ. 4 ) GOTO 1000
      last = lbuf(3)
      CALL INAP(BUF(NUMHDR+1),NSAMPS)                                   /* GET THE DATA TO THE AP
      IF( NDONE .LT. NWEIGS ) THEN                                      /* HAS THE MIX TAPER BEEN DONE
          IF( ndone .LT. nweigs ) ndone = ndone + 1
          ITABLE(NDONE,1)=NDONE
          IPUT=NDONE
          GOTO 550
      ENDIF
  510 DO 520 I = 1, NWEIGS                                               /* FIND A PLACE IN THE AP TO SAVE THE TRACE
         IPUT=I
         IF(ITABLE(I,2).LT.0) GO TO 550
  520 CONTINUE
      DO 530 I=1,NWEIGS                                                  /* NO OPENINGS IN THE TABLE, OPEN A SLOT BY REMOVING THE
         ITABLE(I,1)=ITABLE(I,1)-1                                       /* FIRST TRACE OF THE PREVIOUS MIX AND MOVING ALL THE
         IF(ITABLE(I,1).NE.0) GO TO 530                                  /* THE OTHER TRACES DOWN THE MIX LIST
         ITABLE(I,1)=NWEIGS                                              /* PUT THE NEW TRACE IN THE LAST MIX TRACE NUMBER
         ITABLE(I,2)=-ITABLE(I,2)                                        /* SET THE SLOT FREE FLAG
  530 CONTINUE
      GO TO 510
  550 CONTINUE
      ITABLE(IPUT,2)=-ITABLE(IPUT,2)
      IF(IAND(LPRINT,2).NE.0) PRINT 551,LNUM,TRNO,ITABLE(IPUT,2)
  551 FORMAT(' RECORD ',I6,' TRACE ',F4.1,' IN AP AT ',I10)
      IF( IUSEAP .NE. 0 ) THEN
          CALL VMOV(IN,1,ITABLE(IPUT,2),1,NSAMPS)
          GOTO 555
      ELSE
          J=IN-1
          K=ITABLE(IPUT,2)-1
          DO 553 I=1,NSAMPS
  553     A(K+I)=A(J+I)
      ENDIF
  555 SI=BUF(49)                                                         /* SAMPLE INTERVAL IN SECONDS
      ITABLE(IPUT,3)=IBUF(15)                                            /* DEAD TRACE FLAG
      itable(iput,4) = NINT(buf(46)/si)
      ITABLE(IPUT,5)=NSAMPS                                              /* NUMBER OF SAMPLES IN THE TRACE FROM THE DELAY
      ITABLE(IPUT,6) = NINT(BUF(48)/SI)                                  /*  THE MUTE, IN SAMPLES
      IDIVIS=NEXTAD+MAXSAM                                               /* AP ADDRESS OF THE DIVISOR ARRAY
      ISCRAD=IDIVIS+MAXSAM                                               /* THE AP ADDRESS OF THE LIST OF MIX WEIGHTS
      IF( IUSEAP .NE. 0 ) THEN
          CALL VCLR(NEXTAD,1,2*MAXSAM)                                   /* CLEAR THE OUTPUT AND DIVISOR ARRAYS
          CALL APPUT(WEIGHT,ISCRAD,NWEIGS,2)                             /* PUT THE WEIGHTS IN THE AP
      ELSE
          N = MAXSAM*2
          J=NEXTAD-1
          DO 558 I=1,N
  558     A(J+I)=0.
      ENDIF
      MINDEL=1000000
      MAXSA=0
      DO 560 I=1,NWEIGS                                                  /* THE MIN AND MAX DELAYS AND THE MAX NSAMPS OF ALL TRACES
         IF( ITABLE(I,2) .GE. 0 ) THEN
c             MINDEL=MIN0(MINDEL,ITABLE(I,4))
             MINDEL=MIN(MINDEL,ITABLE(I,4))
c             MAXSA=MAX0(MAXSA,ITABLE(I,5))
             MAXSA=MAX(MAXSA,ITABLE(I,5))
         ENDIF
  560 CONTINUE
      IF(IUSEAP.NE.0) CALL APWD                                          /* WAIT FOR THE WEIGHTS
      DO 600 I=1,NWEIGS
         IF(ITABLE(I,2).LT.0) GOTO 600
         IF(ITABLE(I,3).EQ.2) GOTO 600                                   /* IS IT A DEAD TRACE
         IDIP=(ITABLE(I,1)-1)*JDIP                                       /* HOW MUCH DIP ON THIS TRACE
         IMUTE=0
         JMUTE=0
         IF( ITABLE(I,6) .NE. 0 .AND. itable(i,6) .GT. itable(i,4)) THEN
             IMUTE=ITABLE(I,6)-ITABLE(I,4)                               /* THE MUTE RELATIVE TO THE OLD DELAY
             JMUTE=ITABLE(I,6)-MINDEL                                   /* THE MUTE RELATIVE TO THE NEW DELAY
          ENDIF
          IADDR=ITABLE(I,2)+IMUTE+IDIP
          N=ITABLE(I,5)-IMUTE                                            /* NUMBER OF LIVE SAMPLES IN THE TRACE
          IOUT=NEXTAD+JMUTE                                              /*  THE AP ADDRESS OF THE FIRST LIVE OUTPUT
          IDIV=IDIVIS+JMUTE                                              /* THE AP ADDRESS OF THE FIRST LIVE DIVISOR
          IWAD=ISCRAD+ITABLE(I,1)-1                                      /* THE AP ADDRESS OF THE WEIGHT FOR THIS TRACE
          IF( IUSEAP .NE. 0 ) THEN
              IF( IAND(LPRINT,2) .NE. 0 )
     &            PRINT 580,IDIP,IADDR,IOUT,IDIV,IWAD
  580         FORMAT(' MIX AP PARAMS:',6I10)
              CALL VSMA(IADDR,1,IWAD,IOUT,1,iout,1,n  )                  /* VECTOR SCALAR MULTIPLY AND ADD
              CALL VSADD(IDIV,1,IWAD,IDIV,1,N)                           /* PUT THE WEIGHT INTO THE DIVISOR
          ELSE
              J=IOUT-1                                                   /* THIS WHERE THE SUM IS
              K=IADDR-1                                                  /* THIS IS WHERE THE BEGINNING OF THE INPUT IS
              M=IDIV-1                                                   /* THIS IS WHERE THE DIVISOR IS
              WEIGH=WEIGHT(ITABLE(I,1))                                  /* THIS IS THE WEIGHT
              IF( IAND(lprint,2) .NE. 0 ) PRINT *,' mixing: ',
     &            i,j,k,itable(i,1),itable(i,2),weigh
              DO 595 II=1,N
                 A(J+II)=A(J+II)+A(K+II)*WEIGH                           /* ADD THE TRACES TOGETHER AFTER WEIGHTING
                 A(M+II)=A(M+II)+WEIGH                                   /* ADD THE WEIGHT TO THE DIVISOR
  595         CONTINUE
          ENDIF
  600 CONTINUE
      IF( IUSEAP .NE. 0 ) THEN
          CALL VDIV(IDIVIS,1,NEXTAD,1,IN,1,NSAMPS)                       /* DIVIDE BY THE SUM OF THE WEIGHTS
      ELSE
          J=NEXTAD-1                                                     /* THIS IS WHERE THE SUM IS
          K=IDIVIS-1                                                     /* THIS IS WHERE THE DIVISOR IS
          M=IN-1                                                         /* THIS IS WHERE TO LEAVE THE RESULTS
          DO 620 I=1,NSAMPS
  620     IF( a(k+i) .NE. 0. ) A(M+I)=A(J+I)/A(K+I)
      ENDIF
      IF( type .EQ. 2 ) THEN                                            ! a running record mix
          IF( lbuf(7) .NE. 0 .AND. lbuf(51) .LT. 0 ) THEN               ! start new at the end of a gather
              ndone = 0
              DO 625 i = 1, maxmix
                 IF( itable(i,2) .GT. 0 ) itable(i,2) = -itable(i,2)
  625         CONTINUE
          ENDIF
      ENDIF
      IF( type .EQ. 3 ) THEN
          IF( ndone .LT. nweigs ) THEN
              nready = 0
              RETURN
          ELSE
              ndone = 0
              DO 630 i = 1, maxmix
                 IF( itable(i,2) .GT. 0 ) itable(i,2) = -itable(i,2)
  630         CONTINUE
          ENDIF
      ENDIF
      IDELAY = NINT(BUF(46)/SI)                                          /* THE DELAY IN SAMPLES)
      IF(MINDEL.EQ.IDELAY) GO TO 700
      BUF(46)=FLOAT(MINDEL)*SI                                           /* THE NEW DELAY IN SECONDS
      IBUF(55)=BUF(46)*1000.+.5                                          /* THE NEW DELAY IN MILS
      ibuf(58) = nsamps                                                  /* we may have changed the length by using maxlen !
      numdat = nsamps
      IF(IAND(LPRINT,3).NE.0) PRINT 650,IBUF(55),BUF(46)
  650 FORMAT(' NEW DELAYS:',I10,1X,G10.4)
  700 CONTINUE
      IBUF(15)=1                                                         /* MAKE IT A LIVE TRACE
c****
c****  Do the header mix
c****
 1000 CONTINUE
      IF( hdr+lhdr+ihdr .NE. 0 ) THEN
          IF( ndone .LT. nweigs ) ndone = ndone + 1
          IF( hdr .NE. 0 ) THEN
              hdrs(ndone) = buf(hdr)
              sum = 0.
              divisor = 0.
              DO i = 1, ndone
                 sum = sum + hdrs(i)*weight(i)
                 divisor = divisor + weight(i)
              ENDDO
              buf(hdr) = sum / divisor
              IF( ndone .GE. nweigs ) THEN
                  DO i = 1, nweigs-1
                     hdrs(i) = hdrs(i+1)
                  ENDDO
              ENDIF
          ENDIF
          IF( lhdr .NE. 0 ) THEN
              lhdrs(ndone) = lbuf(lhdr)
              sum = 0.
              divisor = 0.
              DO i = 1, ndone
                 sum = sum + FLOAT(lhdrs(i))*weight(i)
                 divisor = divisor + weight(i)
              ENDDO
              lbuf(lhdr) = NINT(sum / divisor)
              IF( ndone .GE. nweigs ) THEN
                  DO i = 1, nweigs-1
                     lhdrs(i) = lhdrs(i+1)
                  ENDDO
              ENDIF
          ENDIF
          IF( ihdr .NE. 0 ) THEN
              ihdrs(ndone) = ibuf(ihdr)
              sum = 0.
              divisor = 0.
              DO i = 1, ndone
                 sum = sum + FLOAT(ihdrs(i))*weight(i)
                 divisor = divisor + weight(i)
              ENDDO
              ibuf(ihdr) = NINT(sum / divisor)
              IF( ndone .GE. nweigs ) THEN
                  DO i = 1, nweigs-1
                     ihdrs(i) = ihdrs(i+1)
                  ENDDO
              ENDIF
          ENDIF
      ENDIF

      RETURN
      END
