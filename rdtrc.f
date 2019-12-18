       SUBROUTINE RDTRC(BUF,IBUF,LBUF,JSTAT,ISIG)
C      RDTRC READS A SEISMIC TRACE AND HEADER FROM TAPE INTO ARRAY BUF.  THE
C  TAPE MUST BE IN SEGY OR MODIFIED SEGY FORMAT.  MODIFIED SEGY IS THE STANDARD
C  SCRIPPS TAPE FORMAT.  THE TRACE HEADER WILL ALWAYS BEGIN IN BUF(1) AND THE
C  FIRST DATA SAMPLE WILL BE IN BUF(NUMHDR+1), WHERE NUMHDR IS THE HEADER LENGTH
C  (SEE BELOW) AND WILL PROBABLY ALWAYS BE 240 BYTES.  ( THUS, THE FIRST 32 BIT
C  DATA SAMPLE IS IN BUF(61).   IF THE DATA IS 16 BIT INTEGER ON TAPE, RDTRC WIL
C  WILL CONVERT IT TO 32 BIT FLOATING POINT.)
C     THE TRACE RETURNED TO THE CALLING PROGRAM WILL ALWAYS BE IN FLOATING POINT.
C     THE TAPE IS ASSIGNED BY SUBROUTINE RDTRC, THUS RELIEVING THE USER.
C
C  SEGY FORMAT CALLS FOR ALL TRACES ON A TAPE TO BE CONSECUTIVE ON TAPE WITHOUT
C  ANY EOFS.  REEL CHANGES ARE PERFORMED BY RDTRC WITHOUT RETURNING TO THE
C  CALLING PROGRAM.  THE OPERATOR CAN HANDLE THIS THRU FILE WETALK.  SEE
C  SUBROUTINE TPCHNG.
C     THE WAY TO USE THIS PROGRAM IS TO JUST KEEP CALL RDTRC UNTIL THE USER
C  WANTS TO STOP, OR THE OPERATOR STOPS THE JOB THRU TPCHNG.  THE CALLING
C  PROGRAM SHOULD CHECK EACH TRACE HEADER TO SEE IF THE TRACE IS LIVE OR DEAD, A
C  DATA TRACE OR AN AUXILLARY ETC.  SEE THE ARTICLE IN GEOPHYSICS FOR THE
C  DEFINITION OF SUCH.
C      DOUBLE BUFFERING MAY BE ACCOMPLISHED BY THE CALLING ROUTINE BY
C  INITIATING THE TAPE READ BY CALLING MAGTAP WITH A FUNCTION OF 1 (READ
C  WITHOUT WAIT)AND THEN CALLING RDTRC WITH ARGUMENT ISIG SET TO 2 WHEN ACTUALLY
C  READY FOR THE TRACE.
C
C  ARGUMENTS:
C  BUF    - AN ARRAY TO RECEIVE THE INTEGER HEADER AND THE FLOATING POINT DATA.
C           BUF MUST BE LONG ENOUGH TO HOLD THE DATA PLUS THE 240 BYTES OF HEADER.
C  IBUF   - THE BUF ARRAY, BUT THE INTEGER EQUIVALENT.  BUF AND IBUF MUST
C           BE THE SAME ARRAY.  THIS IS NEEDED SINCE THE FORTRAN COMPILER
C           DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C  LBUF   - THE BUF ARRAY, BUT THE LONG INTEGER EQUIVALENT.  BUF AND LBUF MUST
C           BE THE SAME ARRAY.  THIS IS NEEDED SINCE THE FORTRAN COMPILER
C           DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C  JSTAT  - INTEGER STATUS WORD RETURNED BY RDTRC.
C         =0,  EVERYTHING OK.
C         =-1,  END OF SEISMIC LINE
C  ISIG   - A SIGNAL OR SWITCH TO RDTRC.  DOUBLING BUFFERING MAY NOT BE DONE
C           ON THE FIRST TRACE READ>
C         =0,  THE TAPE WILL BE REWOUND AND THE TAPE HEADERS WILL BE READ
C              AND PUT ON DISC. ONLY HONORED ON THE FIRST CALL TO RDTRC.
C         =1,  THE TAPE WILL NOT BE REWOUND.  THE TAPE HEADERS MUST BE IN
C              DISC FILE UNIT IUNHDR. ONLY HONORED ON THE FIRST CALL TO RDTRC.
C         =2,  THE TRACE IS ALREADY IN BUF VIA DOUBLE BUFFERING.  RDTRC WILL NOT
C              READ THE TAPE, BUT WILL DO EVERYTHING ELSE.
C         =3,  FOR DOUBLE BUFFERING - RDTRC WILL WAIT FOR THE I/O COMPLETION AND
C              CHECK THE STATUS, BUT RETURNS TO THE CALLING ROUTINE WITHOUT
C              UNPACKING THE DATA.
C         =4,  FOR DOUBLE BUFFERING - UNPACKS THE DATA.
C         =5,  REWINDS AND TAKES CARE OF TAPE HEADERS, LIKE ISIG=0, BUT DOES
C              NOT READ THE FIRST TRACE
C   DOUBLE BUFFERING MAY BE ACCOMPLISHED BY 1) INITIATING THE I/O, 2) CALLING
C   RDTRC WITH ISIG=3, 3) DOING SOMEHING ELSE, 4) CALLLING RDTRC WITH ISIG=4
C
C  COMMON DEFINITION:
C     RDTRC MUST BE INITIALIZED BY SETTING VARIOUS VARIABLES IN COMMON.
C      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
C     *   SECS,LRENUM,ISRCF,IDTYPE
C   ILUN   - THE CURRENT UNIT NUMBER TO READ THE TAPE FROM.  THIS MUST BE SET
C            INITIALLY BY THE CALLING ROUTINE, BUT WILL BE CHANGED BY RDTRC ON
C            REEL CHANGES.
C   NUMHDR - THE NUMBER OF 32 BIT WORDS CONTAINED IN THE TRACE HEADER.  THIS IS
C            SET BY RDTRC.
C   NUMDAT - THE MAXIMUM NUMBER OF 32 BIT DATA SAMPLES TO READ.  ARRAY BUF WILL
C            HAVE NUMDAT+NUMHDR 32 BIT WORDS IN IT AFTER EACH TRACE IS READ.
C          =0,  THE NUMBER OF DATA SAMPLES READ FROM TAPE WILL BE THE NUMBER ON
C               TAPE ITSELF.  NUMDAT AFTER THE READ  WILL BE THE NUMBER OF DATA
C               SAMPLES ACTUALLY READ.  NUMDAT MUST BE SET TO 0 FOR EACH TRACE
C               FOR THIS TO WORK RIGHT.
C   IUNHDR - THE PRIMOS UNIT NUMBER OF THE MASS STORAGE FILE CONTAINING THE 2
C            INPUT TAPE HEADERS.  THIS IS SET BY RDTRC.  THE TAPE HEADERS MAY BE
C            ACCESSED BY THE CALLING ROUTINE, BUT ARE PRIMARILY FOR THE OUTPUT
C            ROUTINE.
C   IREELN - THE INPUT REEL NUMBER. THIS SHOULD BE SET INITIALLY BY THE
C            CALLING ROUTINE, BUT WILL BE CHANGED WHEN REEL CHANGES OCCUR.
C   INTRCS - THE NUMBER OF DATA TRACES PER RECORD (PRESUMABLY THE NUMBER OF
C            TRACES PER SHOT) ON RECORDING.  SIO WILL SET THIS TO 0 IF ALL TRACES
C            ARE POST STACK.  THIS IS SET BY RDTRC.
C   IFMT   - THE INPUT TAPE FORMAT AS DEFINED BY SEG.  THIS IS SET BY RDTRC ON
C            THE FIRST CALL WHEN IFMT=0.   THUS THE CALLING PROGRAM MAY OVERRIDE
C            THE FORMAT CONTAINED IN THE BINARY TAPE HEADER.
C          =1,  IBM 360 32 BIT FLOATING POINT.
C          =2,  32 BIT INTEGER
C          =3,  16 BIT INTEGER
C          =4,  UTIG 16 bit floating point
C          =5,  PRIMES FLOATING POINT
C         THE DATA WILL ALWAYS BE CONVERTED TO PRIME FLOATING POINT REGARDLESS
C           OF THE INPUT DATA FORMAT.
C   NSKIP  - THE NUMBER OF TRACES ON TAPE TO SKIP BEFORE KEEPING ONE.  THIS WAS
C            CREATED FOR SPECIAL TRACE GATHERS, ESPECIALLY WHEN THE TRACE
C            FORMAT IS NOT FLOATING POINT (WHY CONVERT THE TRACE IF IT IS NOT
C            GOING TO BE USED.)
C   IDTYPE - THE DATA TYPE, IN THE SENSE OF WHAT DOMAIN IT IS IN. (TIME, DEPTH<>>)
C      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD
C           IASGND - AP ASSIGNMENT SWITCH
C                  =0,  AP IS NOT ASSIGNED - CALL APINIT
C                  =1,  AP IS ASSIGNED - DO NOT CALL APINIT
C          IRELSE - AP RELEASE SWITCH
C                 =0,  AP SHOULD BE RELEASED BEFORE RETURNING
C                 =1,  AP SHOULD BE LEFT ASSIGNED
C          IN     - A SWITCH INDICATING WHETHER THE DATA IS ALREDY IN THE AP
C                 =0,  THE DATA IS NOT IN THE AP.
C                 =1,  THE DATA IS IN THE AP AT ADDRESS IN
C          IOUT   - A SWITCH INDICATING WHETHER THE DATA SHOULD BE LEFT IN THE AP
C                 =0,  THE DATA SHOULD BE MOVED BACK TO THE HOST
C                 =1,  THE DATA CAN BE LEFT IN THE AP WITHOUT AN APGET
C                 - THE NEXT FREE AP LOCATION
C
C     PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MAY 1979
c
c   mod 26 Jan 04 - Changes nfiles so that nfiles = 0 uses the in file
c                   and and nfiles = 1 (i.e. nfiles is given) means stop
c                   without the tape change question and file in.
c  3 May 03 - don't swap bytes on the binary header disk image since
c             diskin doesn't.
c  2 May 03 - Read and save the SEG-Y Rev 1 Textual Header
c             Extension records
c  16 Sept 96 - Rewind the tape at the end of job when nfiles = 1
c  13 Nov 95 - add rewind parameter and set it to 1 after doing it!
c  10 Mar 94 - added saving of espn in Phoenix I format 
c  18 Nov 93 - increase nwrds from 30,000 to 32,767
c  28 Sep 93 - increase nwrds from 20,000 to 30,000
c  26 Aug 91 to add device (Unix fortran device independent mag tape interface)
c  24 July 91 by pch - add parameter nfiles
c  17 Apr 91 by pch - don't unpack non seismic traces (ie id=28 - ldgo)
c 2 Mar 91 by pch for:  a) nrskip  and  b) headers "SEGY" tapes
c  22 Jan 91 - Don't renumber traces when trace 0 and trace id of 28
c               (see 17 Apr 89 mod)
c  21 Jan 91 - Don't unpack the trace if ibuf(15) NE 0, 1, 2, 3, 6
c    21 Aug 90 - Change the meaning of an eof to mean a separation of SEGY
c                tape files rather that the end of the tape
c    11 Nov 89 - Allow SSC PHXV format (make jform=3)
c    27 Sep 89  - When SSC, use the number of bytes read from tape as the
c       pch       trace length rather than what is in the SSC tape header,
c                 since the tape header appaers to be wrong sometimes!
c    19 Sep 89  - put intrcs, isi and nsamps into the SEGY binary header
c       GM        so that other programs (VISTA) work
c    28 June 89 - tape changes didn't work when on different unit number.
c       GM       common /readt/ itunit should be common /readt/ ilun
c    17 Apr 89 - Change the logic of renumbering trace numbers if it has
c       pch      a trace number of 0.  If there is a trace number of 
c                zero, I'll add 1 to every trace hence forth, therefore,
c                multichannel data numbered 0-23 will become 1-24.
c    15 Apr 89 - assume if tape status is 0, then the tape record was passed in
c       pch      okay and that the trace was longer than what we asked for.
c                Set the number of words read to the max.
c    12 Apr 89 - bad header unit in common readt (ihunit vs iunhdr) made
c       pch      the SEGY headers bad (especially visible on plots).
C    MODIFIED IN NOVEMBER 1979 TO USE THE AP FOR DATA FORMAT CONVERSION - P.C.H.
C    MODIFIED IN JANUARY 1980 FOR DOUBLE BUFFERING
C
      INTEGER*2 IBUF
      LOGICAL FIRST
      DIMENSION BUF(111),IBUF(1111),LBUF(111)
      COMMON /readt/ ilun, numhdr, numdat, iunhdr, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel,
     &               nfktrc, norigtr, nrskip, nfiles, rewind
      INTEGER rewind
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *     IDECIM
      COMMON /APMEM/A(32766)
      COMMON /TAPES/ NMILS,NTRIES,NPAR,NWREAD
      COMMON /edits/ ierror,iwarn,irun,now,icompt
      COMMON /inputdev/inputdev
      CHARACTER*80 token, cheadr(40), inputdev
      EQUIVALENCE( cheadr(1), ivms )
      SAVE
      DATA FIRST/.TRUE./
      DATA NWRDS/32767/, irenumtr/0/, mfiles/0/
C****
C****
C****
      JSTAT=0
      IF(ISIG.EQ.4) GO TO 251
      NREAD=NSKIP+1
      IF(.NOT.FIRST) GO TO 100
      FIRST=.FALSE.
      NUMHDR=60
      NPAR=0
      CALL GETFIL(1,IUNHDR,token,ISTAT) 
      IF(ISIG.NE.0.AND.ISIG.NE.5) GO TO 100
      IF( inputdev .NE. ' ' ) token = inputdev
      CALL ASTAPE(ILUN, token, 0) 
c      PRINT *,' INPUT REEL ',ireel,' IS NOW MOUNTED ON UNIT ',ilun
      IF( ISIG .EQ. 0 .OR. ISIG .EQ. 5 ) THEN
          IF( rewind .EQ. 1 ) CALL MAGTAP(ILUN,IBUF,IDUM,30,ISTAT)     ! REWIND
          IF( nfskip .NE. 0 ) THEN
              DO 10 i = 1, nfskip
                 CALL magtap( ilun, idum, idum, 22, istat )            ! skip file forward
   10        CONTINUE
          ENDIF
      ENDIF
      IF( nrskip .LT. 0 ) THEN                                          ! skip records before the SEGY headers?
          DO 15 i = 1, -nrskip
             CALL magtap( ilun, idum, idum, 23, istat )                 ! skip record forward
  15      CONTINUE
      ENDIF
      jform = 0                                                         ! allow SSC and SEGY tapes to be mixed!
      CALL MAGTAP(ILUN,IBUF,20000,21,ISTAT)                             ! READ THE ALPHA HEADER
      IF( istat .EQ. 128  .OR. istat .EQ. 512 ) THEN                    ! SSC's PHXI has 256 & PHXV has 1024 byte headers
          IF( istat .EQ. 128 ) THEN
              jform = 2
              IF(icompt .EQ. 2 .OR. icompt .EQ. 4) CALL swap16(ibuf,128)
              nsampss = ibuf(10)
              jntrcs = ibuf(12)
              IF( ibuf(24) .EQ. 4 ) ifmt = 1
              IF( ibuf(24) .EQ. 10 ) ifmt = 3
              isi = ibuf(30)                                            ! microsecond sample interval
              si = REAL(ibuf(30)) / 1000000.                           ! mics to secs
          ENDIF
          IF( istat .EQ. 512 ) THEN                                     ! SSC PHXV format?
              jform = 3
              IF(icompt .EQ. 2 .OR. icompt .EQ. 4) CALL swap16(ibuf,512)
              jntrcs = ibuf(10)
              ifmt = 5                                                  ! IEEE data
              nsampss = ibuf(27)
c****         SSC is stupid - the sample interval is in bytes 51-54, which is
c****         not a 32 bit word boundary! So, use the 16 bit equivalent.
              isi = ibuf(31)                                            ! microsecond sample interval
              si = REAL(isi) / 1000000.                                ! mics to secs
          ENDIF
          DO 30 i = 1, 40                                               ! build a SEGY EBCDIC header
             WRITE( cheadr(i), 20 ) i
   20        FORMAT('c ',I2,' ')
             CALL ascebc( cheadr(i), 80, cheadr(i) )
   30     CONTINUE
          CALL wrdisc( iunhdr, ivms, 800 )                              ! write3200 bytes to disk
          DO 40 i = 1, 200                                              ! build the binary header
   40     ibuf(i) = 0
          ibuf(7) = jntrcs
          ibuf(9) = isi
          ibuf(10) = isi
          ibuf(11) = nsampss
          ibuf(12) = nsampss
          CALL wrdisc( iunhdr, buf, 100 )
          CALL magtap( ilun, idum, idum, 22, istat )                   ! skip a file
          GOTO 100
      ENDIF
      IF(ISTAT.EQ.1800) ISTAT=1600                                      ! MAY COMPANIES DO THIS FOR SOME REASON
      IF( ISTAT .NE. 1600) THEN
          PRINT *,' *** WARNING *** Missing SEGY EBCDIC header; len=',
     *        istat
          DO 50 i = 1, 40                                               ! build a SEGY EBCDIC header
             WRITE( cheadr(i), 20 ) i
             CALL ascebc( cheadr(i), 80, cheadr(i) )
   50     CONTINUE
          CALL wrdisc( iunhdr, ivms, 800 )                              ! write 3200 bytes to disk
          GOTO 65
      ELSE
          CALL PODISC(IUNHDR,1,0)                                       ! REWIND THE HEADER DISC FILE
          CALL wrdisc( iunhdr, lbuf, 800 )
      ENDIF
      DO 60 I=1,200
   60 IBUF(I)=0
      CALL MAGTAP(ILUN,ibinhdr,2000,21,ISTAT)
   65 IF( istat .NE. 200 ) THEN
          PRINT *,' ***  WARNING  *** SEGY binary header missing.'
          IF( ifmt .EQ. 0 ) THEN
              PRINT *,' ***  ERROR  ***  IFMT and SI must be given.'
              STOP
          ENDIF
          DO 70 i = 1, 200
   70     ibinhdr(i) = 0
          CALL magtap( ilun, idum, idum, 25, istat )                    ! skip back a record
      ENDIF
c*****      2 May 03   REMOVED BYTE SWAPPING OF DISK IMAGE OF BINARY HEADER
c      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(ibuf,200)
      CALL WRDISC(IUNHDR,ibinhdr,100)                                   ! WRITE THE BINARY HEADER TO THE HEADER DISC FILE
      nextra = ibinhdr(153)
      IF( nextra .GT. 0 ) THEN
          DO i = 1, nextra
             CALL magtap( ilun, lbuf, nwrds, 21, istat )
             IF( istat .NE. 1600 ) THEN
                 PRINT *,'  ***  WARNING  ***  Bad SEGY Rev 1 record.',
     &             istat*2,' bytes read, should have been 3200.'
             ENDIF
             CALL wrdiscb( iunhdr, lbuf, 3200 )
          ENDDO
      ENDIF
      IF( nrskip .GT. 0 ) THEN                                          ! skip records after the SEGY headers?
          DO 80 i = 1, nrskip
             CALL magtap( ilun, idum, idum, 23, istat )
   80     CONTINUE
      ENDIF
   90 JNTRCS = ibinhdr(7) + ibinhdr(8)
      IF(IFMT.EQ.0) IFMT = ibinhdr(13)
      IF( IFMT .LT. 1 .OR. IFMT .GT. 10 ) THEN
          PRINT *,' ***  ERROR  ***  INCORRECT INPUT TAPE FORMAT OF ',
     *        ifmt,' - USE IFMT TO OVERRIDE.'
          STOP
      ENDIF
      IF(ISIG.EQ.5) RETURN
  100 CONTINUE
      IF(ISRCF.LE.0) ISRCF=1
      IDECIM=ISRCF
      IF(NUMDAT.NE.0) NWRDS=(NUMDAT+NUMHDR)*2
      IFUNC=21                                                          ! READ WITH WAIT
      IF(ISIG.EQ.2.OR.ISIG.EQ.3) IFUNC=40                               ! WAIT FOR I/O COMPETION AND RETURN STATUS
      DO 120 I=1,NREAD                                                  ! DO READS SINCE SKIPS DON'T DETECT EOFS
          CALL MAGTAP(ILUN,IBUF,NWRDS,IFUNC,ISTAT)                      !  READ A TRACE
  120 CONTINUE
      IFUNC=21                                                          !  RESET TO READ IN CASE IFUNC=40 AND NREAD<>1
      IF(ISTAT.NE.-1) GO TO 200
      mfiles = mfiles + 1
c**** Argh.  Are there multiple files on multiple tapes?
      IF( nfiles .GT. 0 ) THEN
          IF( mfiles .GE. nfiles ) THEN
              IF( rewind .EQ. 1 ) CALL MAGTAP(ILUN,IBUF,0,30,ISTAT)     ! REWIND
              jstat = -1
              ilun = -1
              RETURN
          ELSE
              CALL magtap( ilun, ibuf, nwrds, 21, istat ) 
c****     now we don't know if the next file has the SEGY headers or not!
              IF( istat .EQ. 1600 .OR. istat .EQ. 1800 ) THEN
                  CALL magtap( ilun, ibuf, nwrds, 21, istat )           ! read the binary header
                  CALL magtap( ilun, ibuf, nwrds, 21, istat )           ! read the trace
              ENDIF
              IF( istat .GT. 0 ) GOTO 200
          ENDIF
      ENDIF
C****
C****    INPUT REEL CHANGE HERE  - HIT AN EOF
C****
      CALL MAGTAP(ILUN,IBUF,0,10,ISTAT)                                 ! INITIATE A REWIND
      CALL TPCHNG(ILUN,JLUN,0)                                          ! CHANGE THE INPUT REEL
      ILUN=JLUN
      IREELN=IREELN+1
      IF( ILUN .LT. 0 ) THEN
          JSTAT=-1                                                      ! THE OPERATOR TERMINATED THE JOB
          RETURN
      ENDIF
      IF( rewind .EQ. 1 ) CALL MAGTAP(ILUN,IBUF,0,30,ISTAT)             ! REWIND
      CALL MAGTAP(ILUN,IBUF,1600,21,ISTAT)                              ! READ THE NEW ALPHA REEL HEADER
      IF( istat .EQ. 128 ) THEN                                         ! SSC Phoenix I format?
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(ibuf,128)
          jform = 2
          nsampss = ibuf(10)                                            ! ssc doesn't have nsamps in the trace header!
          jntrcs = ibuf(12)
          IF( ibuf(24) .EQ. 4 ) ifmt = 1
          IF( ibuf(24) .EQ. 10 ) ifmt = 3
          isi = ibuf(30)                                                ! microsecond sample interval
          si = REAL(ibuf(30)) / 1000000.                               ! mics to secs
          GOTO 90
      ENDIF
      IF( istat .EQ. 512 ) THEN                                         ! SSC PHXV format?
          jform = 3
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(ibuf,512)
          jntrcs = ibuf(10)
          ifmt = 5                                                      ! IEEE data
          nsampss = ibuf(27)
c****     SSC is stupid - the sample interval is in bytes 51-54, which is
c****     not a 32 bit word boundary! So, use the 16 bit equivalent.
          isi = ibuf(31)                                                ! microsecond sample interval
          si = FLOAT(isi) / 1000000.                                    ! mics to secs
          GOTO 90
      ENDIF
      IF( ISTAT .NE. 1600) THEN
          PRINT *,' ***  WARNING  *** THE TAPE HEADER HAS AN',
     *        ' INCORRECT LENGTH OF ',istat
      ELSE
C*****    WRITE THE HEADER AT THE BEGINNING OF THE FILE
          CALL PODISC(IUNHDR,1,0)                                       ! REWIND THE HEADER DISC FILE
          CALL wrdisc( iunhdr, lbuf, 800 )
      ENDIF
      CALL MAGTAP(ILUN,IBUF,200,21,ISTAT)
      IF( ISTAT .NE. 200 ) THEN
          PRINT *,' ***  WARNING  ***  Incorrect SEGY binary ',
     &       'header length of ',istat
          IF( istat .LT. 0 ) STOP
          GOTO 200                                                      ! assume it is a trace
      ENDIF
c****   don't byte swap the copy on loacl disk
c      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(ibuf,200)
      CALL WRDISC(IUNHDR,BUF,100)
      IFMT=IBUF(13)                                                     ! ALLOW THE INPUT DATA FORMAT TO CHANGE FROM REEL TO REEL!
      idtype = ibuf(31)
      itxsi = ibuf(33)
      itxdel = ibuf(34)
      NREAD=NREAD-I+1                                                   ! TAKE CARE OF THE TRACE SKIPS!!
      nextra = ibinhdr(153)
      IF( nextra .GT. 0 ) THEN
          DO i = 1, nextra
             CALL magtap( ilun, lbuf, nwrds, 21, istat )
             IF( istat .NE. 1600 ) THEN
                 PRINT *,'  ***  WARNING  ***  Bad SEGY Rev 1 record.',
     &             istat*2,' bytes read, should have been 3200.'
             ENDIF
             CALL wrdiscb( iunhdr, lbuf, 3200 )
          ENDDO
      ENDIF
      GO TO 90
C****
C****   IF THERE WAS A PARITY THE TRACE IS PROBABLY OKAY IF IT IS THE
C****   SAME LENGTH AS THE LAST TRACE (A 'FALSE PARITY').  IF IT IS A
C****   HARD PARITY, IGNORE THE TRACE ENTIRELY (AND PRAY THAT IT DOESN'T
C****   BOMB LATER BECAUSE OF THAT!)
C****
  200 CONTINUE                                                          !  UNPACK THE DATA
      IF(ISTAT.GT.0) GO TO 230
      IF(ISTAT.EQ.-6.AND.NWREAD.EQ.LASTNW) GO TO 230                    ! IGNORE THE PARITY
      IF( istat .EQ. 0 ) THEN                                           ! assume that this means the tape record was too big
          nwread = nwrds
          GOTO 230
      ENDIF
      IF( ISTAT .EQ. -6 ) THEN                                          ! IT WASN'T A PARITY!
          IF( NREAD .EQ. 1 ) THEN                                       ! WERE WE SKIPPING TRACES?
              PRINT 205,(LBUF(JJ),JJ=4,7)
  205         FORMAT(' ***  WARNING  ***  SHOT ',I6,' TRACE ',I3,
     *     ' RP ',I6,' TRACE ',I3,' WAS DROPPED DUE TO TAPE PARITIES.')
            GO TO 100
          ELSE
              PRINT 210,ISTAT
  210         FORMAT(' ***  ERROR  ***  ABORT DUE TO INPUT TAPE READ',
     *         ' ERROR OF',I10)
              STOP
          ENDIF
      ENDIF
  220 IF( istat .EQ. 1600 .AND. ibinhdr(153) .EQ. -1 ) THEN
          IF( nextra .EQ. -1 ) nextra = 0
          nextra = nextra + 1
          CALL wrdiscb( iunhdr, lbuf, 3200 )
          CALL magtap( ilun, lbuf, nwrds, 21, istat )
c****     write the number of extras to binary header word 153 
          IF( istat .NE. 1600 ) THEN
              CALL podisc( iunhdr, 0, 3505 )
              ibinhdr(153) = nextra
              CALL wrdiscb( iunhdr, ibinhdr(153), 2 )
          ENDIF
          GOTO 220
      ENDIF
C****
C****   GOT A TRACE INTO MEMORY!!!
C*****
  230 IEOF=0
      IF( ibuf(15) .EQ. 28 ) GOTO 1000
      IF( jform .EQ. 2 ) THEN                                           ! SSC PHXI format?
c****      SSC lies sometimes - it says the data is longer than is really is!
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              CALL swap32( lbuf, 1 )
              CALL swap16( ibuf(3), 36 )
              CALL swap32( lbuf(22), 1 )
              CALL swap16( ibuf(118), 1 )
          ENDIF
          lcdp = lbuf(1)
          lespn = ibuf(3)                                               ! the source position order number
          icdptr = ibuf(4)
          ishot = ibuf(5)
          istrno = ibuf(6)
          ldist = ibuf(9)
          idelay= ibuf(13)
          iday = ibuf(35)
          ihour = ibuf(36)
          min = ibuf(37)
          isec = ibuf(38)
          DO 240 i = 1, istat-48
  240     ibuf(istat-48+121-i) = ibuf(istat+1-i)                        !  The SSC trace header is only 48 words long
      ENDIF
      IF( jform .EQ. 3 ) THEN                                           ! SSC format?
c****      SSC lies sometimes - it says the data is longer than is really is!
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              CALL swap32( lbuf, 1 )
              CALL swap16( ibuf(3), 36 )
              CALL swap32( lbuf(22), 1 )
              CALL swap16( ibuf(118), 1 )
          ENDIF
          lcdp = lbuf(1)
          lespn = lbuf(3)                                               ! the source position order number
          icdptr = lbuf(4)
          ishot = ibuf(10)
          istrno = ibuf(12)
          ldist = buf(9)
          idelay = buf(30)
          iday = ibuf(100)
          ihour = ibuf(102)
          min = ibuf(104)
          isec = ibuf(106)
          DO 243 i = 1, istat-512
  243     ibuf(istat-512+121-i) = ibuf(istat+1-i)                        !  The SSC trace header is only 48 words long
      ENDIF
      IF( jform .EQ. 2 .OR. jform .EQ. 3 ) THEN
          DO 245 i = 1, 60
  245     lbuf(i) = 0
          lbuf(3) = ishot
          lbuf(4) = istrno
          lbuf(5) = lespn
          lbuf(6) = lcdp
          lbuf(7) = icdptr
          ibuf(15) = 1
          lbuf(10) = ldist
          ibuf(55) = idelay
c****      SSC lies sometimes - it says the data is longer than is really is!
          IF( jform .EQ. 2 ) THEN
              ibuf(58) = (istat-48) / 2
              nwread = nwread + 72
          ELSE
              ibuf(58) = (istat-512) / 2
          ENDIF
          ibuf(59) = isi
          ibuf(80) = iday
          ibuf(81) = ihour
          ibuf(82) = min
          ibuf(83) = isec
      ELSE
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              CALL swap32(lbuf(1),7)
              CALL swap16(ibuf(15),1)
              CALL swap32(lbuf(10),1)
              CALL swap32(lbuf(16),1)
              CALL swap32(lbuf(19),1)
              CALL swap32(lbuf(21),1)
              CALL swap16(ibuf(55),5)
              CALL swap16(ibuf(79),6)
              CALL swap16(ibuf(110),1)
          ENDIF
      ENDIF
      LASTNW=NWREAD                                                     ! SAVE THE NUMBER OF WORDS READ FOR THE NEXT TRACE
C****
C****   SIO RAMA05 SINGLE CHANNEL DATA HAD BAD DELAYS IN THE HEADER WHEN THE
C****   WAS ZERO.  RAMA05 SINGLE CHANNEL ALSO HAD A TRACE NUMBER OF ZERO!
      IF(IBUF(55).EQ.8000.AND.IBUF(79).EQ.80) IBUF(55)=0
c****
c****   If there is a trace number of 0 (zero) change it to 1 because
c**** other parts of SIOSEIS have trouble with trace numbers of 0.
c****  LDGO and some of UT's and some of SIO's single channel data do this!!!
c****  LDGO stack tapes have zeroes for the shot number, shot trace number and
c**** rp trace number (the only thing not zero is the rp number!).
c**** Remember that SIOSEIS stack tapes have a zero shot trace number
c**** but non-zero rp trace number.
c****
      IF( lbuf(4)+lbuf(7) .EQ. 0 .AND. ibuf(15) .NE. 28 ) THEN
          IF( irenumtr .EQ. 0 ) THEN
              PRINT *,' ***  WARNING  ***  Input tape has trace numbers 
     *of zero, input will change these by adding 1.'
              irenumtr = 1
          ENDIF
      ENDIF
      IF( irenumtr .EQ. 1 ) THEN
          IF( lbuf(3) .NE. 0 ) THEN
              lbuf(4) = lbuf(4) + 1
          ELSE
              lbuf(7) = lbuf(7) + 1
          ENDIF
      ENDIF
      IF(ISIG.EQ.3) RETURN
  251 CONTINUE
      IF(jform .NE. 2 .AND. jform .NE. 3) IBUF(58) = (ISTAT-NUMHDR*2)/2
      IF( IFMT .EQ. 3 .OR. ifmt .EQ. 4 ) IBUF(58) = IBUF(58) * 2
      NUMDAT=IBUF(58)
      IN=0                                                              ! INFORM EVERYONE (INAP) THAT THE DATA IS NOT IN THE AP
c****
c****   Don't unpack dead and non seismic traces, zero them!
c****
      IF( ibuf(15) .NE. 0 .AND. ibuf(15) .NE. 1 .AND. 
     *    ibuf(15) .NE. 3 .AND. ibuf(15) .NE. 6 ) THEN
          IF(IASGND.NE.0) THEN
             CALL VCLR(1,1,NUMDAT)
             IN=1                                                       ! SET THE FLAG SAYING THAT THE TRACE IS IN THE AP
          ELSE
             DO 270 II=1,NUMDAT                                         ! ZERO OUT THE DEAD TRACE IN MEMORY
  270        BUF(NUMHDR+II)=0.
          ENDIF
          GOTO 1000                                                     ! DON'T BOTHER REFORMATTING THE DEAD TRACE!!!
      ENDIF
c****
c****   Unpack the data
c****
      GO TO (300,400,500,600,1000),  IFMT
      GOTO 1000
  300 CONTINUE                                                          !   IBM 360 32 BIT FLOATING POINT
      JFMT=3
      IF(IUSEAP.NE.0) GO TO 310
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     *        CALL swap32( buf(numhdr+1), numdat)
          CALL ibm2fp(buf(numhdr+1),numdat,buf(numhdr+1))
          GOTO 1000
  310 CONTINUE
      IN=1                                                              ! THIS MAKES INAP NO TRANSFER DATA TO THE AP
      CALL INAP(BUF(NUMHDR+1),NUMDAT)                                   ! INAP SETS VARIOUS SWITCHES FOR THE AP
C****              AP IS NOT PRIME FLOATING POINT!!!
      CALL APPUT(BUF(NUMHDR+1),IN,NUMDAT,JFMT)                          ! SEND THE DATA TO THE AP
      CALL APWD
      IF(JFMT.NE.0) GO TO 330
      CALL VFLT32(IN,1,IN,1,NUMDAT)                                     !  FLOAT THE DATA
      GO TO 350
  330 CONTINUE
      IF(JFMT.NE.1) GO TO 350
      CALL VFLT(IN,1,IN,1,NUMDAT)                                       !  CONVERT FROM 16 BIT INTEGER TO F.P.
  350 CONTINUE
      GO TO 1000
  400 CONTINUE                                                          !  32 BIT INTEGER
      JFMT=0
      IF ( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     *     CALL swap32(lbuf(numhdr+1),numdat)
      IF(IASGND.NE.0.AND.IUSEAP.NE.0) GO TO 310
      IS=NUMHDR+1
      IE=NUMHDR+NUMDAT
      DO 450 I=IS,IE
  450 BUF(I)=LBUF(I)
      GO TO 1000
  500 CONTINUE                                                          !   16 BIT INTEGER
      JFMT=1
      IF(IASGND.NE.0.AND.IUSEAP.NE.0) GO TO 310
      J=NUMDAT+numhdr+1                                                 ! DO THIS BACKWARDS SO THAT THE ARRAYS MAY BE THE SAME
      k = j + numhdr                                                    ! there are more 16 bit words than 32 bit words in the header!
      DO 510 I=1,NUMDAT
  510 BUF(J-I)=IBUF(k-I)
      GO TO 1000
  600 CONTINUE                                                          ! UTIG floating point
      CALL sfp2fp( ibuf(numhdr+numhdr+1), numdat, a )
      DO 610 i = 1, numdat
  610 buf(numhdr+i) = a(i)
      GOTO 1000
C****
C****     SAMPLE RATE CONVERT THE DATA IF NECESSARY  -  NO ANTI-ALIAS FILTER
C****
 1000 IF(IN.EQ.0.AND.IASGND.NE.0.AND.IUSEAP.NE.0)                       ! PUT THE DATA IN THE AP IF THE SP IS
     *    CALL INAP(BUF(NUMHDR),NUMDAT)                                 ! ASSIGNED AND THE DATA ISN'T THERE
      IF(ISRCF.LE.1) GO TO 2000
      IF(IN.NE.0) GO TO 1100                                            ! IS THE DATA IN THE AP?
C****  THE DATA CAN'T BE IN APMEM IF IUSEAP.EQ.0 - SEE ABOVE CODE!!
      J=NUMHDR                                                          ! NOPE, DO IT IN CORE
      DO 1010 I=1,NUMDAT,ISRCF
      J=J+1
 1010 BUF(J)=BUF(NUMHDR+I)
      NUMDAT=NUMDAT/ISRCF
      GO TO 1200
 1100 NUMDAT=NUMDAT/ISRCF
      CALL VMOV(IN,ISRCF,IN,1,NUMDAT)
 1200 IBUF(58)=NUMDAT
      IBUF(59)=IBUF(59)*ISRCF
C****
C****
C****
 2000 CONTINUE
      IF(LBUF(7).NE.0) GO TO 2020                                       ! IS THE TAPE BY RP OR SHOT
      IF(JNTRCS.LE.2) RETURN
      IF(LBUF(4).EQ.JNTRCS.OR.NSKIP.GT.1) PRINT 2010,LBUF(3),
     *   LBUF(4),IREELN
 2010 FORMAT(' READ SHOT ',I8,' TRACE ',I8,' FROM REEL ',I6)
      RETURN
 2020 IF(LBUF(51).EQ.-1) PRINT 2030,LBUF(6),LBUF(7),IREELN
 2030 FORMAT(' READ RP ',I8,' TRACE ',I8,' FROM REEL ',I6)
      RETURN
      END

