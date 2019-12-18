      SUBROUTINE T2FEX(BUF,LBUF,IBUF,SCR,LSCR)
C     T2FEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS T2F, TIME TO FREQUENCY
C  TRANSFORMATION.  THE TRANSFORMATION IS DONE BY THE FFT IN EITHER THE AP
C  OR HOST MEMORY, DEPENDING ON THE IUSEAP SWITCH.  IN BOTH CASES, THE DATA IS
C  FIRST WINDOWED, THEN FILLED WITH ZEROES TO A POWER OF 2, THEN THE FFT, THEN
C  SCALED DOWN BY 1./FFTLEN.  THE SEGY TAPE HEADER SIGNAL FOR DATA TYPE IS SET
C  TO 'F' FOR RECTANGULAR COORDINATES OR 'W' FOR POLAR COORDINATES.
C
C
C  ARGUMENTS:
C    BUF,LBUF,IBUF   - THE TRACE HEADER FOLLOWED BY THE TRACE
C    SCR,LSCR,ISCR   - A SCRATCH ARRAY
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 14 JANUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
c  mod 25 Aug 93 - Increase maxdow from 100 to 1000
c  mod 13 Oct 93 - Save the input (time domain) sample interval.
c  mod 22 Dec 97 - Change MAX0 to MAX
c  mod 6 Sept 05 - Work on overflow messages.
c  mod 25 Jul 06 - Use numdat rather than ibuf(58) so we get > 32K samples
c  mod 12 Jan 07 - Index for zero pad was off by 1 (didn't zero nsamps + 1)
c                - The window was not created correctly (bad arg to window)
c  mod 2 Oct. 2008 - Add coords AMPL
c
c
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr, ilagaptr,
     *                 ilagbptr
      COMMON /T2F/ MUNIT,NLISTS
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *     IDECIM
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
     *   SECS,LRENUM,ISRCF,IDTYPE,
     *               nfskip, jform, itxsi, itxdel, nfktrc, origntr
      COMMON /APMEM/A(500000)                                            /* THE AP SIMULATOR DATA ARRAY
      PARAMETER (MAXDOW=1000)                                            /* THE MAXIMUM NUMBER OF POINTS THAT CAN BE SAVED IN WINDO
      PARAMETER (NWRDS=12)                                              /* THE NUMBER OF WORDS IN EVERY PARAMETER LIST
      DIMENSION BUF(111),IBUF(111),LBUF(111),SCR(111),LSCR(111)
      DIMENSION WINDO(MAXDOW)
      INTEGER FNO,FTR
      INTEGER*2 IBUF
      LOGICAL FIRST
      SAVE
      DATA FIRST/.TRUE./, NW/0/, IPRINT/1/
C
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          /* IS IT A DEAD TRACE
      ISIG=0
      IF(.NOT.FIRST) GO TO 50
      FIRST=.FALSE.
   10 CONTINUE                                                          /* GET THE FIRST PARAMETER LIST INT0 MEMORY ARRAY SCR
      CALL PODISC(MUNIT,1,0)
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      MLISTS=1
   50 CONTINUE
      LNUM=LBUF(3)                                                      /*  IS THE DATA ON TAPE SORTED BY SHOT
      IF(LBUF(7).NE.0) LNUM=LBUF(6)                                      /*  OR BY RP
   70 IF(LNUM.GE.FNO) GO TO 100                                           /* IS THIS SHOT BEFORE THIS PARAMTER LIST
      IF(MLISTS.EQ.1) GO TO 500                                          /* IS IT BEFORE THE FIRST LIST
      IF(LNUM.LE.LASTNO) GO TO 10                                        /* IS IT IN OR BEFORE THE LAST LIST
      GO TO 500                                                          /* IT MUST BE BETWEEN THE 2 LISTS
  100 CONTINUE                                                          /*  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                          /* USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.GE.NLISTS) GO TO 500                                    /* ANY MORE USER PARAM LISTS ON DISC
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
      LASTNO=LNO
      CALL RDDISC(MUNIT,SCR,NWRDS,ISTAT)
      MLISTS=MLISTS+1
      ISIG=1
      FNO=LSCR(1)
      LNO=LSCR(2)
      GO TO 70
C****
C****    NOW FIND THE PARAMETERS FOR THE  TRACE NUMBER
C****
  500 IF(ISIG.EQ.0) GO TO 1000                                          /* IS A LIST IN SCR
      FTR=LSCR(3)
      LTR=LSCR(4)
      IWINDO=LSCR(5)
      WINLEN=SCR(6)
      LFFT=LSCR(7)
      LCOORD=LSCR(8)                                                    /* 1=RECTANGULAR, 2=POLAR
      LPRINT=LSCR(9)
      IADDWB=LSCR(10)
      stime = scr(11)
      stime1 = stime
      etime = scr(12)
      etime1 = etime
C
 1000 CONTINUE
      IF( stime1 .LT. 0. ) stime = buf(46)                                /* use the delay if no stime
      IF( etime1 .LT. 0. ) etime = buf(46) + numdat * buf(49)           /* delay*nsamps*si
      delay = buf(46)
      IF( stime1 .GE. 0. ) THEN
          buf(46) = 0.                                                   /* set delay to 0 if stime is given
          ibuf(55) = 0
      ENDIF
      itxsi = ibuf(ISIPTR)                                              ! save the sample interval (in microsecs)
      SI=BUF(49)
      nsamps = (etime - stime) / si
      NWINDO=WINLEN/SI
      IF(IWINDO.EQ.5 .OR. NWINDO .EQ. 0) GO TO 1005                     /* DON'T WINDOW IF IT IS A RECTANGULAR WINDOW!
      IF(NWINDO.GT.MAXDOW) GO TO 1005                                    /* IS THE WINDOW LARGER THAN THE WINDO ARRAY?
      IF(NW.NE.NWINDO) CALL WINDOW(-IWINDO,WINDO,nwindo,DUMMY)
      NW=NWINDO                                                          /* NW IS THE NUMBER OF POINTS SAVED IN THE WINDO ARRAY
C
 1005 CONTINUE
      nfft=LFFT                                                          /* THE NUMBER OF POINTS IN THE FFT
      IF(LFFT.LE.0) THEN                                                /* DID THE USER GIVE FFTLEN?
         nfft = 4
 1010    CONTINUE
         IF( nfft .LT. nsamps ) THEN
             nfft = nfft + nfft
             GOTO 1010
         ENDIF
      ENDIF
      mfft = 4                                                          /* make sure the fft is a power of 2 long
      DO 1030 i = 1, 15
         IF( nfft .EQ. mfft ) GOTO 1040
         mfft = mfft + mfft
 1030 CONTINUE
      PRINT *,' ***  ERROR  ***  The fft length of ',nfft,' is not ',
     *    'a power of 2.'
      STOP
 1040 NZEROS=nfft-NSAMPS                                                /* NOW SEE IF WE NEED TO PAD WITH ZEROES
      IPOWR2 = 2
      ITEMP = 4
 1050 CONTINUE
      IF( nfft .GT. itemp ) THEN                                        /* find the power of 2
          ipowr2 = ipowr2 + 1
          itemp = itemp + itemp
          GOTO 1050
      ENDIF
      n = MAX(numdat,nfft*2)
      CALL INAP(BUF(NUMHDR+1),n)                                        /* PUT THE DATA IN THE AP OR ARRAY A
      in = (stime - delay) / si + 1
C****  AND MAKE SURE THE AP ADDRESSING ALLOWS FOR LARGER DATA ARRAYS
      IF(IUSEAP.EQ.1) GO TO 1500                                        /* IS THERE AN AP?
C****
C****    DO THE TRANSFORMATION IN THE HOST
C****
      IF(IWINDO.EQ.5.OR.NWINDO.LE.0) GO TO 1200                          /* DON'T DO THE WINDOW IF IT IS A RECTANGULAR WINDOW
      IF(NW.NE.NWINDO) CALL WINDOW(-IWINDO,WINDO,nwindo,DUMMY)
      DO 1110 I=1,NW                                                    /* WINDOW RETURNED EITHER THE LEFT HALF OF THE WINDOW OR ALL OF IT
 1110 A(IN+I-1)=A(IN+I-1)*WINDO(I)                                      /* APPLY THE WINDOW!
c      IF(IWINDO.GT.0) GO TO 1200                                        /* WAS THAT THE WHOLE WINDOW WE JUST DID?
      ISTART=IN+NSAMPS-NW-1                                              /* FIND THE BACK END OF THE TRACE TO BE WINDOWED
      DO I = 1, nw                                                       /* APPLY THE RIGHT HALF OF THE WINDOW TO THE END OF THE TRACE
         A(ISTART+I)=A(ISTART+I)*WINDO(NW-I+1)
      ENDDO
c      GO TO 1200
c 1150 CONTINUE
c      IF( nw .LT. 10 .OR. nw .GT. maxdow ) THEN
c          PRINT *,' ***  ERROR  ***  Bad window length of ',nw
c          STOP
c      ENDIF
c      CALL WINDOW(-IWINDO,SCR,NW,DUMMY)                                  /* THE WINDOW COULDN'T FIT IN THE WINDO ARRAY
c      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL window(',iwindo,nw
c      DO 1160 I=1,NW                                                    /* NOW APPLY THE NEWLY CREATED WINDOW
c 1160 A(IN+I-1)=A(IN+I-1)*SCR(I)
c      IF(IWINDO.GT.0) GO TO 1200                                        /* DID WE DO THE WHOLE WINDOW?
c      ISTART=NSAMPS+IN-NW-1                                              /* FIND THE BACK OF THE TRACE TO WINDOW
c      DO 1170 I=1,NW
c 1170 A(ISTART+I)=A(ISTART+I)*SCR(NW-I+1)
c 1200 IF(NWINDO.GT.0) NSAMPS=NWINDO                                      /* IF THE WHOLE WINDOW, CHANGE THE NUMBER OF SAMPLES
 1200 IF( NZEROS .GT. 0 ) THEN                                          /* IS THERE ANYTHING TO PAD?
          DO I=1,NZEROS
             A(IN+NSAMPS+I-1) = 0.                                      /* PAD THE END OF THE WINDOWED DATA WITH ZEROES
          ENDDO
      ENDIF
      J=NEXTAD                                                          /* NOW MAKE THE DATA COMPLEX - FFT CAN'T DO REAL DATA
      DO 1270 I=1,nfft                                                   /* GET A BETTER AND FASTER FFT ROUTINE THAT HANDLES REAL DATA
      A(J)=A(IN+I-1)
      A(J+1)=0.                                                         /* ZERO OUT THE IMAGINARY
      J=J+2
 1270 CONTINUE
      nyqust = 1./si/2. + .5
      deltaf = FLOAT(nyqust) / FLOAT(nfft) * 2.
      CALL FFTFWD(A(NEXTAD),IPOWR2)
      IF( IAND(lprint,2).NE.0) PRINT *, 'CALL fftwd(',nextad,ipowr2
      nsamps = nfft + nfft
      IF( iprint .EQ. 1 ) THEN
          PRINT *,' The new (output) sample interval (deltaf) is ',
     *       deltaf,' hz.'
          PRINT *,' The output length is ',nsamps,' samples long.'
          PRINT *,' There are ',nfft,' frequencies (',-nyqust,
     &            ' to 0 to ',nyqust-deltaf,')'
          IF( nsamps .GT. 32767 ) PRINT *,
     &        ' Will write the SEG-Y number of samples as unsigned int.'
          IF( nsamps .GT. maxsamps ) PRINT *,
     &      ' Writing past data buffer into scratch area (might be ok!)'
      ENDIF
      iprint = iprint + 1
      SCALE=1./FLOAT(nfft)
      DO 1310 I=1,NSAMPS
 1310 A(IN+I-1)=A(NEXTAD+I-1)*SCALE                                      /* SCALE THE DATA WHILE MOVING IT BACK TO IN
      IF( lcoord .EQ. 2 .OR. lcoord .EQ. 3 ) THEN
          DO 1400 i = 1, nsamps                                         /* move the data to nextad
 1400     a(nextad+i-1) = a(in+i-1)
C****  NOTE THAT THE AMPLITUDE SPECTRUM PRECEDES THE PHASE SPECTRUM
          IF(IAND(lprint,2).NE.0) PRINT *,' call polarc(',nfft,nextad,in
          CALL POLARC(nfft,A(NEXTAD),A(IN),A(IN+nfft))                  /* CONVERT TO POLAR AND MOVE
      ENDIF
      GO TO 5000
C****
C****    WE HAVE AN AP!!
C****
 1500 CONTINUE
      IF(IWINDO.EQ.5.OR.NWINDO.LE.0) GO TO 1600                          /* SKIP THE WINDOWING IF IT IS A BOX CAR
      IF(NW.NE.NWINDO) GO TO 1550                                        /* IS THE WINDOW IN THE WINDO ARRAY?
      CALL APWR                                                          /* WAIT FOR THE AP TO FINISH, SO WE DON'T CLOBBER ANYTHING IN PROGRESS
      CALL APPUT(WINDO,NEXTAD,NW,2)                                        /* PUT THE WINDOW IN THE AP
 1520 CALL APWD                                                          /* WAIT FOR THE WINDOW TO GET TO THE Ap
      CALL VMUL(IN,1,NEXTAD,1,IN,1,NW)                                      /* MULTIPLY THE DATA BY THE WINDOW
      IF(IWINDO.GT.0) GO TO 1600                                        /* IS THE WINDOW THE WHOLE TRACE?
      CALL VMUL(IN+NSAMPS-1,-1,NEXTAD,1,IN+NSAMPS-1,-1,NW)                 /* DO THE BACK END WINDOW
      GO TO 1600
 1550 CALL WINDOW(-IWINDO,SCR,NWINDO,DUMMY)                              /* GENERATE THE WINDOW IN THE SCR ARRAY
      IF( IAND(lprint,2) .NE. 0) PRINT *,' CALL window(',iwindo,nw
      CALL APWR
      CALL APPUT(SCR,NEXTAD,NWINDO,2)
      CALL APPUT(SCR,NEXTAD,NWINDO,2)
      GO TO 1520
 1600 IF(NWINDO.GT.0) NSAMPS=NWINDO
      IF(NZEROS.GT.0) CALL VCLR(IN+NSAMPS,1,NZEROS)                      /* PAD THE DATA WITH ZEROES
      CALL vmov(in,1,nextad,2,nfft)                                      /* make it into complex data
      CALL vclr(nextad+1,2,nfft)                                         /* there are now nfft complexes
      IF(in+nfft+nfft-1 .GT. nextad) THEN
          IF( iprint .EQ. 1 ) THEN
              iprint = 2
              PRINT *,' The data after T2F extends into the ap scratch',
     *                ' area.  Post T2F processes that use the ap '
              PRINT *,' might destroy part of the frequency data.'
              PRINT *,' NEXTAD=',NEXTAD
          ENDIF
      ENDIF
      nsamps = nfft + nfft                                              /* there are nfft complexes which is nfft+nfft reals
      CALL cfft(nextad,nfft,+1)
      CALL cfftsc(nextad,nfft)                                          /* scale by 1./nfft
      IF( lcoord .EQ. 1 ) THEN                                          /* is the data to be left in rectangular coordinates?
          CALL vmov(nextad,1,in,1,nsamps)                               /* move nfft complexes
      ELSE
          CALL POLAR(NEXTAD,2,NEXTAD,2,nfft)                            /* CONVERT TO POLAR COORDINATES
          CALL VMOV(NEXTAD,2,IN,1,nfft)                                 /* MOVE THE AMPLITUDE SPECTRUM TO THE FIRST HALF OFTHE TRACE
          CALL VMOV(NEXTAD+1,2,IN+nfft,1,nfft)                          /* MOVE THE PHASES TO THE SECOND HALF
      ENDIF
C*****
C*****
C*****
 5000 CONTINUE
      NUMDAT=NSAMPS
c      IBUF(58)=NSAMPS
c      IF( ibuf(57) .EQ. 0 ) lbuf(29) = nsamps
      IDTYPE = LCOORD + 3                                                /* 1=TIME, 2=FK RECT, 3=FK POLAR, 4=F RECT, 5=F POLAR
      IF( lcoord .EQ. 3 ) THEN
          buf(numhdr+nsamps+1) = buf(numhdr+1)
          numdat = numdat / 4 + 1
          DO i = 1, numdat
             buf(numhdr+i) = buf(numhdr+numdat+i-1)
          ENDDO
          ibuf(59) = NINT( deltaf * 1000. )
          buf(49) = deltaf / 1000.
          ibuf(55) = 0
          buf(46) = 0.
      ENDIF
      CALL long2ushort( numdat, ibuf(58) )
      RETURN
      END
