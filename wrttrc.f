      SUBROUTINE WRTTRC(BUF,IBUF,LBUF,SCR,LSCR,ISCR)
C     WRTTRC WRITES A SEISMIC TRACE ON MAG TAPE FROM MEMORY.  THE OUTPUT TAPE
C  WILL BE IN SEGY OR MODIFIED SEGY (SIO) FORMAT).  THE TAPE HEADERS ARE TAKEN
C  CARE OF BY WRTTRC AUTOMATICALLY (COMMON VARIABLE POSAFT).  THE TRACE HEADER
C  MUST PRECEDE THE DATA IN ARRAY BUF.  THE DATA MUST BE 32BIT FLOATING POINT IN
C  ARRAY BUF.
C     SEGY FORMAT CALLS FOR ALL TRACES ON A TAPE TO BE CONSECUTIVE ON TAPE
C  WITHOUT ANY EOFS.  REEL CHANGES ARE PERFORMED BY WRTTRC WITHOUT RETURNING TO
C  THE CALLING PROGRAM.  THE OPERATOR CAN HANDLE THIS THRU FILE WETALK.  SEE
C  SUBROUTINE TPCHNG.
C     THE WAY TO USE THIS PROGRAM IS TO JUST KEEP CALLING WRTTRC UNTIL THE USER
C  WANTS TO STOP, OR THE OPERATOR STOPS THE JOB THRU TPCHNG.  THE CALLING
C  PROGRAM SHOULD CHECK EACH TRACE HEADER TO SEE IF THE TRACE IS LIVE OR DEAD, A
C  DATA TRACE OR AN AUXILLARY ETC.  SEE THE ARTICLE IN GEOPHYSICS FOR THE
C  DEFINITION OF SUCH.
C     WRTTRC MUST BE USED IN CONJUNCTION WITH SUBROUTINE RDTRC OR THE CALLING
C  SUBROUTINE MUST CREATE THE TAPE HEADER FILES AND SET COMMON READT PROPERLY.
C     LBUF(3) IS THE SHOT NUMBER.
C     LBUF(4) IS THE TRACE NUMBER WITHIN THE SHOT
C     LBUF(6) IS THE RP NUMBER (SET BY GEOM)
C     LBUF(7) IS THE TRACE NUMBER WITHIN THE RP (SET BY GATHER AND STACK)
C     LBUF(7)>0 INDICATES TO WRTTRC THAT THE DATA IS SORTED BY RP RATHER
C  THAN BY SHOT.
C    LBUF(7)>0 AND LBUF(3)=0 INDICATES THAT THE DATA HAS BEEN STACKED.
C     LBUF(51) = -1 INDICATES THAT THE TRACE IS THE LAST WITHIN THE RP.
C
C  ARGUMENTS:
C  BUF    - AN ARRAY TO RECEIVE THE INTEGER HEADER AND THE FLOATING POINT DATA.
C           BUF MUST BE LONG ENOUGH TO HOLD THE DATA PLUS THE 240 BYTES OF
C           HEADER.  BUF IS DESTROYED IF THE OUTPUT TAPE FORMAT IS NOT TYPE 5
C           (MODIFIED SEGY, WHICH IS PRIME FLOATING POINT.)
C  IBUF   - THE BUF ARRAY, BUT THE INTEGER EQUIVALENT.  BUF AND IBUF MUST
C           BE THE SAME ARRAY.  THIS IS NEEDED SINCE THE FORTRAN COMPILER
C           DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C  LBUF   - THE BUF ARRAY, BUT THE LONG INTEGER EQUIVALENT.  BUF AND LBUF MUST
C           BE THE SAME ARRAY.  THIS IS NEEDED SINCE THE FORTRAN COMPILER
C           DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS.
C
C  COMMON DEFINITION:
C      COMMON /WRITET/ounit,NSAMPS,OREEL,POSAFT,OFMT,NTRCS,LFOR
C   ounit  - OUTPUT TAPE UNIT NUMBER.  THIS VARIABLE MAY BE CHANGED BY WRTTRC
C            ON REEL CHANGES.  16 BIT INTEGER.
C   NSAMPS - THE NUMBER OF DATA SAMPLES TO WRITE TO TAPE.  THE NUMBER OF 32
C            BIT FLOATING POINT SAMPLES IN ARRAY BUF.
C   OREEL  - THE OUTPUT REELN NUMBER.  INTEGER*4
C   POSAFT - OUTPUT TAPE POSITION INDICATOR.  POSAFT WILL BE SET TO 0
C            BY WRTTRC AFTER THE FIRST TRACE HAS BEEN WRIITEN TO TAPE.
C            16 BIT INTEGER.
C          <0,  POSITION THE OUTPUT TAPE AFTER THE LAST DATA TRACE ON TAPE
C               BEFORE WRITING THE FIRST TRACE TO TAPE.
C          =0,  NO ACTION.  NO POSITIONING WILL BE PERFORMED
C          >0,  THE TAPE WILL BE POSITIONED AFTER THE LAST TRACE OF THE SHOT (OR
C               RP) WITH A NUMBER OF POSAFT
C   OFMT   - TYPE OF OUTPUT (SEGY) FORMAT.  16 BIT INTEGER.
C          =1,  IBM 370 FLOATING POINT.
C          =2,  32 BIT INTEGER (INTEGER*4)
C          =3,  16 BIT INTEGER (INTEGER*2)
C          =4,  NOT AVAILABLE
C          =5,  HOST FLOATING POINT.
C   NTRCS  - THE NUMBER OF TRACES PER SEISMIC SHOT.  THE SHOT NUMBER IN THE
C            TRACE HEADER WILL BE MODIFIED BY WRTTRC WHEN THE NUMBER OF TRACES
C            WRITTEN ON A GIVEN SHOT EXCEEDS NTRCS.
C   LFOR   - FIRST OUTPUT RECORD (SHOT) NUMBER.  32 BIT INTEGER.
C          <0,  THE FIRST OUTPUT RECORD NUMBER WILL BE 1 GREATER THAN THE LAST
C               RECORD NUMBER ON TAPE WHEN POSAFT<0.
C          =0,  THE OUTPUT RECORD NUMBER WILL BE THE SAME AS THE INPUT RECORD
C               NUMBERS.
C          >0,  THE FIRST SHOT WRITTEN TO TAPE WILL HAVE A NUMBER OF LFOR.
C
C     COMMON /READT/IUNIT,NUMHDR,NUMDAT,IUNHDR,IREELN,INTRCS,IFMT,NSKIP
C   NUMHDR - THE NUMBER OF 32 BIT WORDS CONTAINED IN THE TRACE HEADER.  THIS IS
C            SET BY RDTRC.
C   IUNHDR - THE UNIT NUMBER OF THE MASS STORAGE FILE CONTAINING THE 2 INPUT
C            TAPE HEADERS.  THIS IS SET BY RDTRC.  THE TAPE HEADERS MAY BE
C            ACCESSED BY THE CALLING ROUTINE, BUT ARE PRIMARILY FOR THE OUTPUT
C            ROUTINE.
C   JNTRCS - THE NUMBER OF DATA TRACES PER RECORD (PRESUMABLY THE NUMBER OF
C            TRACES PER SHOT) ON RECORDING.  SIO WILL SET THIS TO 1 IF ALL TRACES
C            ARE POST STACK.
C   IFMT   - THE INPUT TAPE FORMAT AS DEFINED BY SEGY FORMAT.
C
C   PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JUNE 1979
C
c  MODIFICATIONS:
c   14 Apr 89 - Make the calls to xx2ibm more machine independent
c      pch
c   13 Aug 90 - move the call magtap backspace from a labeled statement
c       to an unlabeled statement to get around some bizarre sun 4.x bug
c   28 Aug 91 - Add device and magtapf77 calls - Positioning after can't be done
c       on device because the Fortran mag tape device driver doesn't do backspacing. 
C   11 MAR 92 - Redo positioning, especially for Apollo device case
c  12 May 92 by pch - Change icompt = 2 from DEC BSD to DecStation Ultrix
c  10 Nov 95 - Change posaft -1 to use skip file rather than skip
c              record because SCSI driver (on Sun anyway) refuses to
c              skip backwards over file marks!
c  13 Nov 95 - add rewindo
c  4 Sept 96 - Write trace 0 to tape before trace 1 if luntr0 is used.
c            - Start a new file when newfile is set by segdin (EOT)
c  18 Sept 96 - Clean up (redo) rewind/nfskip/posaft
c  25 June 97 - nsamps in binary header wasn't correct when newfile
c               and decimf were used in segdin.
c  11 Sept 97 - Make the output trace 0 the same length as the data
c               traces for the inflexible commercial packages.
c  15 Feb 00 - binary header sort type 2 is CDP (bug made shots = 2 also)
c  4 Sept 00 - Add parameter TRACE0 whether to write the SEG-D external
c              header as a trace 0 with trace id 28
c  7 Sept 00 - Add parameter ounit2 to allow two copies at once.
c  mod 1 July 02 - Allow ounit2 to be given without ounit being given
c                  so that ounit2 can be tested on a 1 drive system.
c  mod 1 Aug 02 - Print a warning message when output tape change about to happen.
c  mod 11 Oct 02 - Print the file number after writing an eof for newfile
c  mod 19 Oct 02 - Do the OUT/out file removal better.
c  mod 19 Mar 03 - The LDEO SEG-D external header is separate from
c                  the entire SEG-D external header.
c  mod 1 May 03 - Write the new SEG-Y Rev 1 Textual Header Extension records.
c  mod 13 Jun 05 - Don't set newfile=0 since diskox may need it
c
      COMMON /readt/ itunit, numhdr, numdat, iunhdr, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc
      COMMON /outdev/outdevice
      CHARACTER*80 outdevice, ldummy
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /WRITET/ounit,NSAMPS,OREEL,POSAFT,OFMT,NTRCS,LFOR,ONUMTR,
     &       nfskipo, rewindo, newfile, itrace0, ounit2
      INTEGER*4 OFMT,ounit,SUNIT,POSAFT,ONUMTR,OREEL, rewindo, ounit2
      COMMON /SIOAP/IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      INTEGER*2 iap(1)
      INTEGER lap(1)
      EQUIVALENCE (a(1),iap(1)), (a(1),lap(1))
      COMMON /segdin/ junit, nlists, nwrds, luntr0, luntr0a, lunldeo
      DIMENSION BUF(1000),IBUF(1000),LBUF(1000)
      DIMENSION SCR(1000),ISCR(1000),LSCR(1000)
      COMMON /binhdr/ ibinhdr(200)
      INTEGER*2 ibinhdr
      CHARACTER*80 token
      INTEGER*2 IBUF,ISCR
      DIMENSION istatus(8)
      SAVE
      LOGICAL FIRST
      DATA FIRST/.TRUE./, iprtwarn/0/
C
      NSAMPS = ibuf(58)                                                 ! ALLOW NSAMPS TO VARY ON EVERY TRACE
      NWRDS=(NUMHDR+NSAMPS)*2                                           ! THE NUMBER OF 16 BIT WORDS PER TRACE
      IF( newfile .NE. 0 ) THEN
c****   don't set newfile=0 because diskox may also need it
c          newfile = 0
          IF( ounit .GE. 0 ) THEN
              CALL magtap( ounit, scr, 0, 32, istat )  
              CALL magtap( ounit, istatus, 0, 33, istat )
              IF( outdevice .NE. ' ' ) THEN
                  PRINT *,' Wrote file mark number ',istatus(5),' on',
     &            outdevice(1:40)
              ELSE
               PRINT *,' Wrote file mark number ',istatus(5),' on unit',
     &          ounit
              ENDIF
          ENDIF
          IF( ounit2 .GE. 0 ) THEN
              CALL magtap( ounit2, scr, 0, 32, istat )
              CALL magtap( ounit2, istatus, 0, 33, istat )
              PRINT *,' Wrote file mark number ',istatus(5),' on unit',
     &          ounit2
          ENDIF
          CALL PODISC( IUNHDR, 1, 0 )
          CALL rddisc( iunhdr, lscr, 800, istat )
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,1600,31,ISTAT)    
          IF( ounit2 .GE. 0 ) CALL magtap(ounit2, scr,1600, 31, istat )
          CALL RDDISC(IUNHDR,SCR,100,ISTAT)                             ! READ THE BINARY HEADER FROM DISK
          ISCR(7)=NTRCS
          ISCR(8)=0
          IF( iscr(9) .EQ. 0 ) iscr(9) = ibuf(59)
          IF( iscr(11) .EQ. 0 ) iscr(11) = nsamps
          ISCR(13)=OFMT
          ISCR(15)=ITTYPE
          iscr(31) = idtype
          iscr(32) = nfktrc
          iscr(33) = itxsi
          iscr(34) = itxdel
          nextra = iscr(153)
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(iscr,200)
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,200,31,ISTAT)
          IF( ounit2 .GE. 0 ) CALL magtap(ounit2, scr, 200, 31, istat )
          IF( nextra .GT. 0 ) THEN
              DO i = 1, nextra
                 CALL rddiscb( iunhdr, lscr, 3200, istat )
                 IF( ounit .GE. 0 )
     &               CALL magtap( ounit, scr, 1600, 31, istat)
                 IF( ounit2 .GE. 0 )
     &               CALL magtap( ounit2, scr, 1600, 31, istat)
              ENDDO
          ENDIF
      ENDIF
      IF(ONUMTR.NE.0) NTRCS=ONUMTR
C**** PROCESS INPUT SETS NTRCS TO THE INPUT NUMBER OF TRACE ON EVERY SHOT.
C**** PROCESS STACK SETS NTRCS TO 1
C**** THE USER MAY SET NTRCS BY PARAMETER ONUMTR
      IF(.NOT.FIRST) GO TO 100
      FIRST=.FALSE.
      CALL GETFIL(2,ITEMP,token,ISTAT)                                  ! Just get a unit number
      IF(ICOMPT.EQ.1) ITEMP=ITEMP+4                                     ! IS IT A PRIME COMPUTER?
      OPEN(UNIT=ITEMP,FILE='OUT',STATUS='OLD',ERR=4)                    ! TRY TO OPEN FILE IN
      CLOSE(UNIT=ITEMP,STATUS='DELETE')                                 ! DELETE FILE IN
    4 OPEN(UNIT=ITEMP,FILE='out',STATUS='OLD',ERR=5)                    ! TRY TO OPEN FILE IN
      CLOSE(UNIT=ITEMP,STATUS='DELETE')                                 ! DELETE FILE IN
    5 IF(ICOMPT.EQ.1) ITEMP=ITEMP-4
      CALL FREFIL(1,ITEMP,ISTAT)                                        ! FREE THE UNIT NUMBER FOR GETFIL
      LTRCNO=1                                                          ! THE TRACE NUMBER WITHIN A SHOT
      LTRNO1=0                                                          ! THE TRACE NUMBER WITHIN THE SEISMIC LINE
      LTRNO2=0                                                          ! THE TRACE NUMBER ON TAPE
      IF(OFMT.EQ.0) OFMT=IFMT                                           ! PRESET THE OUTPUT FORMAT TO THE INPUT FORMAT
      IF(NTRCS.EQ.0) NTRCS=JNTRCS
      ITTYPE=1                                                          ! SET THE TAPE SORT TO SORTED BY SHOTS
c****  changed to NE 0.  sort = 2 means CDP!
      IF( LBUF(7) .NE. 0 ) ittype = 2                                   ! GATHER SETS THIS TO THE TRACE NUMBER WITHIN RP
      IF( nsamps * 4 .GT. 65500 ) THEN
          PRINT *,' ***  WARNING  ***  Some tape drives may split this',
     &   ' trace into multiple tape records resulting in illegal SEG-Y.'
       PRINT *,' Many tape drives are limited to 65KB bytes per record.'
      ENDIF
c****
c****   Assign the tape, rewind it if appropriate, and skip file if
c****   appropriate.
c****
c     This next one takes some thinking!  If nfskip <> 0 and posaft = -1
c     then skip an extra file in nfskip.
      IF( nfskipo .NE. 0 .AND. posaft .EQ. -1 ) nfskipo = nfskipo + 1
      IF( outdevice .EQ. ' ' ) THEN
          IF( ounit .GE. 0 ) CALL ASTAPE( ounit, LDUMMY, 1 )
          IF( ounit2 .GE. 0 ) CALL astape( ounit2, ldummy, 1 )
          IF( rewindo .EQ. 1 ) THEN
              IF( ounit .GE. 0 ) CALL magtap( ounit,scr,idum,30,istat)
              IF( ounit2 .GE. 0 ) CALL magtap(ounit2,scr,idum,30,istat)
          ENDIF
          IF( nfskipo .GT. 0 ) THEN                                     ! number of files to skip
              DO 10 i = 1, nfskipo
                 IF( ounit .GE. 0 ) CALL magtap(ounit,scr,idum,22,istat)
                 IF( ounit2.GE.0) CALL magtap(ounit2,scr,idum,22,istat)
   10         CONTINUE
          ENDIF
      ELSE
c****     ounit2 will not work if OUTDEVICE is used
          IF( ounit .GE. 0 ) CALL ASTAPE( ounit, outdevice, 1 )
          IF( rewindo .EQ. 1 .AND. ounit .GE. 0 )
     &        CALL magtap( ounit, scr, idum,30,istat)
          IF( nfskipo .GT. 0 ) THEN
              DO 20 i = 1, nfskipo
                 IF( ounit .GE. 0 ) CALL magtap(ounit,scr,idum,22,istat)
   20         CONTINUE
          ENDIF
      ENDIF
c****
c****   Position the tape after the last shot/rp, by skipping a file
c****
      IF( POSAFT .LT. 0 ) THEN
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,IDUM,24,ISTAT)       ! skip a file backwards
          IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,IDUM,24,ISTAT)
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,IDUM,22,ISTAT)       ! SKIP FILE FORWARD
          IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,IDUM,22,ISTAT)
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,IDUM,24,ISTAT)       ! skip a file backwards
          IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,IDUM,24,ISTAT)
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)       ! SKIP RECORD BACK
          IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,IDUM,24,ISTAT)
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,NWRDS,21,ISTAT)      ! READ THE LAST TRACE ON TAPE
          IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,NWRDS,21,ISTAT)
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              CALL swap32(lscr,7)
              CALL swap16(iscr(15),1)
          ENDIF
C****     MAKE SURE THE OUTPUT TAPE WE ARE ABOUT TO WRITE ON IS A SEISMIC LOOKING
C****     TAPE.  (THIS PROGRAM ALWAYS WRITES CERTAIN INFO - CHECK I.)
          IF(lscr(2) .LT. 0 .OR. iscr(15) .LT.0.OR.iscr(15).GT. 2) THEN ! THE TRACE NUMBER WITHIN THE REEL MUST BE POSITIVE
              PRINT *,' ***  ERROR  ***  The output tape on unit ',
     *        ounit,' is not a seismic tape and may not be positioned!.'
              STOP
          ENDIF
          ltrcno1 = lscr(1)
          ltrcno2 = lscr(2)
          IF(LFOR.LT.0.AND.LSCR(7).EQ.0) LFOR=LSCR(3)+1                 ! SORTED BY SHOT
          IF(LFOR.LT.0.AND.LSCR(7).NE.0) LFOR=LSCR(6)+1                 ! SORTED BY RP
          ndone = 0
          GOTO 100
      ENDIF
c****
c****    Position after a specific shot/rp
c****
      IF( posaft .GT. 0 ) THEN
          ndone = 0
          IF( lfor .LT. 0 ) lfor = posaft
          ltrcno1 = lscr(1) 
          ltrcno2 = lscr(2)
c***      backup 2 records just in case the last job finished here
          IF( ounit .GE. 0 ) CALL magtap( ounit, scr, idum, 25, istat)
          IF( ounit2 .GE. 0 ) CALL magtap( ounit2, scr, idum, 25, istat)
          IF( ounit .GE. 0 ) CALL magtap( ounit, scr, idum, 25, istat)
          IF( ounit2 .GE. 0 ) CALL magtap( ounit2, scr, idum, 25, istat)
  50      CONTINUE
          IF( ounit .GE. 0 ) CALL magtap( ounit, lscr, 40000, 21, istat)
          IF( ounit2 .GE. 0 ) CALL magtap( ounit2, lscr, 40000,21,istat)
          IF( istat .LT. 0 ) THEN                                       ! EOF?
              PRINT *,' ***  ERROR  ***  Can not find ',posaft,
     *            ' to position after! istat=',istat
              STOP
          ENDIF
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32(scr,7)
          IF( istat .EQ. 1600 .OR. istat .EQ. 200 ) GOTO 50
          IF( iscr(15) .NE. 1 .AND. iscr(15) .NE. 2 ) GOTO 50           ! is this a seismic trace?
          IF( lscr(7) .NE. 0 ) THEN                                     ! sorted by rp if rp trace number is nonzero
              IF( lscr(6) .NE. posaft ) GOTO 50
              IF( lbuf(51) .EQ. -1 ) GOTO 100
          ELSE
              IF( lscr(3) .NE. posaft ) GOTO 50                         ! shot sorted tape
              IF( lscr(4) .EQ. ntrcs ) GOTO 100
              GOTO 50
          ENDIF
      ENDIF
C****
c****    NO positioning
C****  GET THE EBCDIC TAPE HEADER FROM DISC AND WRITE IT TO TAPE
C****
      CALL PODISC( IUNHDR, 1, 0 )
      CALL rddisc( iunhdr, lscr, 800, istat )
      l1600 = 1600
      l31 = 31
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,lSCR,1600,31,ISTAT)
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,lSCR,1600,31,ISTAT)
      CALL RDDISC(IUNHDR,SCR,100,ISTAT)                                 ! READ THE BINARY HEADER FROM DISK
      ISCR(7)=NTRCS
      ISCR(8)=0 
      IF( iscr(9) .EQ. 0 ) iscr(9) = ibuf(59)
      IF( iscr(11) .EQ. 0 ) iscr(11) = nsamps
      ISCR(13)=OFMT
      ISCR(15)=ITTYPE
      iscr(31) = idtype
      iscr(32) = nfktrc
      iscr(33) = itxsi
      iscr(34) = itxdel
      nextra = iscr(153)
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(iscr,200)
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,200,31,ISTAT)
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,200,31,ISTAT)
      IF( nextra .GT. 0 ) THEN
          DO i = 1, nextra
             CALL rddiscb( iunhdr, lscr, 3200, istat )
             IF( ounit .GE. 0 ) 
     &           CALL magtap( ounit, scr, 1600, 31, istat )
             IF( ounit2 .GE. 0 )
     &            CALL magtap( ounit2, scr, 1600, 31, istat )
          ENDDO
      ENDIF
      NDONE=0
C****
C****    Finally we're ready to write the trace, well almost
c****
  100 CONTINUE
      DO 101 i = 1, numhdr                                              ! save the header in scr
  101 scr(i) = buf(i)
      DO 102 i = 1, nsamps                                              ! save the data for the next process
  102 scr(numhdr+i) = buf(numhdr+i)
      LTRNO1=LTRNO1+1
      LTRNO2=LTRNO2+1
      LBUF(1)=LTRNO1
      LBUF(2)=LTRNO2
      IF(LFOR.NE.0.AND.LBUF(7).EQ.0) LBUF(3)=LFOR                       !  SORTED BY SHOT
      IF(LFOR.NE.0.AND.LBUF(7).EQ.0) LBUF(4)=LTRCNO                     !  SORTED BY SHOT
      IF(LFOR.NE.0.AND.LBUF(7).NE.0) LBUF(6)=LFOR                       !  SORTED BY RP
      IF( lbuf(4) .EQ. 1 .AND. luntr0 .NE. 0 .AND. itrace0 .EQ. 1 ) THEN
          DO i = 1, numhdr
             lscr(i) = lbuf(i)
          ENDDO
          lscr(4) = 0                                                   ! trace 0
          iscr(15) = 28                                                 ! trace id = 28
          iscr(58) = nsamps                                             ! do the same length as the data trace for other systems!
          CALL podiscb( luntr0, 0, 0 )
c****     Screw the old Digicon trace 0 compatibility
c****     Save the whole SEGD general header
          CALL rddisc( luntr0, nbytes, 1, istat )
          CALL rddiscb( luntr0, lscr(numhdr+1), nbytes, istat )
          IF( nbytes .GT. nsamps*4 ) THEN
              IF( iprtwarn .LT. 10 ) THEN
                  iprtwarn = iprtwarn + 1
                  PRINT *,' ***  WARNING  ***  Trace 0 is truncated.'
              ENDIF
          ENDIF
c****     Assume 4 byte samples.  nwords is the number of 16 bit words!
          nwords = (nsamps + numhdr) * 2
          IF( ounit .GE. 0 ) CALL magtap( ounit,iscr,nwords,31,istat ) 
          IF( ounit2 .GE. 0 ) CALL magtap( ounit2,iscr,nwords,31,istat)
          IF( luntr0a .NE. 0 )                                          ! write trace 0 to an external file
     &         CALL wrdiscb( luntr0a, lscr(numhdr+1), 3940, istat )
          ltrno1 = ltrno1 + 1
          ltrno2 = ltrno2 + 1
          lbuf(1) = ltrno1
          lbuf(2) = ltrno2
      ENDIF
      LTRCNO=LTRCNO+1
C****
C****    PACK THE DATA
C****
      IF( ibuf(15) .EQ. 2 ) THEN                                        ! is it a dead trace?
          DO 110 i = 1, nsamps                                          ! don't format dead traces
  110     lbuf(numhdr+i) = 0
          GOTO 200
      ENDIF
      IF( ofmt .GT. 4 ) THEN                                            ! lost data format?
          iout = 0                                                      ! get the data out of the ap
          CALL rlseap( buf(numhdr+1), nsamps )
      ENDIF
      IF( ofmt .EQ. 1 ) THEN                                            ! IBM floating point
          IF( iuseap .NE. 0 .AND. iasgnd .NE. 0 ) THEN                  ! in the ap?
              CALL apwr
              CALL apget( buf(numhdr+1), in, nsamps, 3 )                ! convert to IBM while getting out of the ap
              CALL apwd
          ELSE
              IF( iasgnd .EQ. 0 ) THEN                                  ! is the data in the simulator?
                  IF( icompt .EQ. 1 )                                   ! Prime?
     *                CALL pr2ibm(buf(numhdr+1),nsamps,buf(numhdr+1))
                  IF( icompt .EQ. 2 ) THEN                              ! DecStation (IEEE word, Dec byte order)
                      CALL ie2ibm(buf(numhdr+1),nsamps,buf(numhdr+1))
                      CALL swap32( lbuf(numhdr+1), nsamps )
                  ENDIF
                  IF( icompt.EQ.3.OR. icompt .EQ. 6 .OR. icompt .EQ. 7 )
     *                CALL ie2ibm(buf(numhdr+1),nsamps,buf(numhdr+1))
                  IF( icompt .EQ. 4 ) THEN                              ! Vax
                      CALL dr2ibm(buf(numhdr+1),nsamps,buf(numhdr+1))
                      CALL swap32( lbuf(numhdr+1), nsamps )
                  ENDIF
              ELSE
                  IF( icompt .EQ. 1 )                                   ! Prime?
     *                CALL pr2ibm( a, nsamps, a )
                  IF( icompt .EQ. 2 ) THEN
                      CALL ie2ibm( a, nsamps, a )
                      CALL swap32( a, nsamps )
                  ENDIF
                  IF( icompt.EQ.3.OR. icompt .EQ. 6 .OR. icompt .EQ. 7 )
     *                CALL ie2ibm( a, nsamps, a )
                  IF( icompt .EQ. 4 ) THEN                              ! Vax 
                      CALL dr2ibm( a, nsamps, a )
                      CALL swap32( a, nsamps )
                  ENDIF
              ENDIF
          ENDIF
          GOTO 200
      ENDIF
      IF( ofmt .EQ. 2 ) THEN                                            ! INTEGER*4 ?
          IF( iuseap .NE. 0 .AND. iasgnd .NE. 0 ) THEN                  ! do we have an ap and is it in it?
              CALL vfix32( in, 1, nextad, 1, nsamps)
              CALL apwr
              CALL apget( buf(numhdr+1), nextad, nsamps, 0 )            ! don't convert from ap floating point
              CALL apwd
          ELSE
              IF( iasgnd .EQ. 0 ) THEN
                  DO 140 i = 1, nsamps
  140             lbuf(numhdr+i) = buf(numhdr+i)
              ELSE
                  DO 150 i = 1, nsamps
  150             lbuf(numhdr+i) = a(in+i-1)
              ENDIF
          ENDIF
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )                        ! DEC needs byte swap
     *        CALL swap32( lbuf(numhdr+1), nsamps )
          GOTO 200
      ENDIF
      IF( ofmt .EQ. 3 ) THEN                                            ! INTEGER*2 ?
          IF( iuseap .NE. 0 .AND. iasgnd .NE. 0 ) THEN                  ! do we have an ap and is it in it?
              CALL vpk16( in, 1, nextad, 1, nsamps)
              CALL apwr
              CALL apget( buf(numhdr+1), nextad, nsamps, 0 )            ! don't convert from ap floating point
              CALL apwd
          ELSE
              IF( iasgnd .EQ. 0 ) THEN
                  itemp = numhdr + numhdr                               ! twice as many 16 bit words as 32 bit!
                  DO 170 i = 1, nsamps
  170             ibuf(itemp+i) = buf(numhdr+i)
              ELSE
                  DO 180 i = 1, nsamps
  180             ibuf(itemp+i) = a(in+i-1)
              ENDIF
          ENDIF
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )                        ! DEC needs byte swap
     *        CALL swap16( ibuf(itemp), nsamps )
      ENDIF
C****
C****     WRITE THE TRACE AND TRACE HEADER TO TAPE
C****
  200 CONTINUE
      IOUT=0
      CALL RLSEAP(BUF(NUMHDR+1),NSAMPS)                                 ! GET THE DATA FROM THE AP
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN                       ! is it a DEC machine?
          CALL swap32(lbuf(1),7)
          CALL swap16(ibuf(15),1)
          CALL swap32(lbuf(10),1)
          CALL swap32(lbuf(16),1)
          CALL swap32(lbuf(19),1)
          CALL swap32(lbuf(21),1)
          CALL swap16(ibuf(55),5)
          CALL swap16(ibuf(79),6)
      ENDIF
      istat1 = 0
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,BUF,NWRDS,31,ISTAT1)
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,BUF,NWRDS,31,ISTAT)
      IF( ISTAT .NE. -2 .AND. istat1 .NE. -2 ) GOTO 300
C****
C****   EOT DETECTED  - many computers don't allow anything after or over the
c****                   tape marker (even an EOF) - I believe that we should
c****                   be able to move the tape off the end if I want to and
c****                   that software should only see 2 EOFs as EOT on read.  I
c****                   really should be able to finish the current write and
c****                   write 2 EOFs, but no, the current tape record must be
c****                   on the next tape.
c   Mod 29 Sep 97.  Remove the 2 eof writes from wrttrc because it's
c done elsewehere.  The sequence (logic) goes as:
c 1) wrttrc gets a scratch disk file and backs up and rereads the shot
c    in order to save the shot to disk.
c 2) wrttrc then backs over them again so the tape is positioned at the
c    end of the previous shot.  SEG-Y says a record may not be split.
c 3) tpchng is called to "talk" to the operator and get the new tape.
c 4) tpchng calls reltap
c 5) reltap writes an eof and another if rewindo =0.
c 6) reltap rewinds, then does a chmod 400, closes the unit, chmod 600.
c         (the chmod "should" prevent close from writing a file mark)
c 7) wrttrc then rewinds and writes the SEG-Y headers 
c   
C****
  202 CALL GETFIL(1,SUNIT,token,ISTAT) 
      PRINT *,' ***  WARNING  ***  Preparing for an OUTPUT tape change.'
c****  The rules seem to have changed.  It used to be that the trace
c**** was on the tape - you could write OVER the EOT mark.  Now you
c**** can't.  The last trace is NOT on tape.
      N = NDONE
      IF( ntrcs .EQ. 1 ) n = n + 2                                      ! make sure the EOFs fit before the EOT
      DO I = 1,N
         IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)        ! SKIP RECORD BACKWARDS
         IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,IDUM,25,ISTAT) 
      ENDDO
c****  Assume ounit exists - get the old traces from ounit
      DO I = 1, N
         CALL MAGTAP(ounit,SCR,NWRDS,21,ISTAT)  
         CALL WRDISC(SUNIT,SCR,ISTAT/2)
      ENDDO
      DO I=1,N                                                          ! BACK UP OVER THE PARTIAL SHOT AGAIN
         CALL MAGTAP(ounit,SCR,NWRDS,25,ISTAT)
      ENDDO
c          DO 227 I=1,2                                                 ! NOW CLOSE THE TAPE OUT WITH 2 FILE MARKS
c  227CALL MAGTAP(ounit,SCR,IDUM,32,ISTAT)
c     CALL MAGTAP(ounit,SCR,0,30,ISTAT)                                 ! INITIATE A REWIND ON THE TAPE
c**** no positioning after on the new tape
      IF( ounit .GE. 0 ) THEN
          CALL TPCHNG(ounit,ounit,1)                                    ! CHANGE THE OUTPUT TAPE
          CALL MAGTAP(ounit,SCR,0,30,ISTAT)                             ! INITIATE A REWIND ON THE NEW TAPE
      ENDIF
      IF( ounit2 .GE. 0 ) THEN
          CALL TPCHNG(ounit2,ounit2,1)                                  ! CHANGE THE OUTPUT TAPE
          CALL MAGTAP(ounit2,SCR,0,30,ISTAT)                            ! INITIATE A REWIND ON THE NEW TAPE
      ENDIF
      LTRNO2=0                                                          ! SET THE REEL TRACE NUMBER TO 0
      PRINT *,'                          OUTPUT TAPE CHANGE'
C****  WRITE THE TAPE HEADERS
      CALL PODISC(IUNHDR,1,0)                                           ! REWIND THE TAPE HEADER FILE
      CALL RDDISC(IUNHDR,SCR,800,ISTAT)                                 ! GET THE EBCDIC HEADER
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,1600,31,ISTAT)           ! WRITE IT TO TAPE
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,1600,31,ISTAT)         ! WRITE IT TO TAPE
C**** DO THE BINARY HEADER
      CALL RDDISC(IUNHDR,SCR,100,ISTAT)
      ISCR(7)=NTRCS 
      ISCR(8)=0    
      ISCR(13)=OFMT  
      ISCR(15)=ITTYPE                                                   ! THE TAPE SORT CODE
      nextra = iscr(153)
      IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap16(iscr,200)
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,200,31,ISTAT)            ! WRITE IT TO TAPE
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,200,31,ISTAT)          ! WRITE IT TO TAPE
      IF( nextra .GT. 0 ) THEN
          DO i = 1, nextra
             CALL rddiscb( iunhdr, lscr, 3200, istat )
             IF( ounit .GE. 0 )
     &           CALL magtap( ounit, scr, 1600, 31, istat )
             IF( ounit2 .GE. 0 )
     &           CALL magtap( ounit2, scr, 1600, 31, istat )
          ENDDO
      ENDIF
C**** NOW WRITE THE TRACES TO TAPE
      CALL podisc( sunit, 1, 0 )                                        ! rewind the scratch disc file
      ltrno2 = 0
      DO 290 I=1,N
         CALL RDDISC(SUNIT,SCR,NWRDS/2,ISTAT)
         LTRNO2=LTRNO2+1
         lscr(2)=LTRNO2
         IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32(lscr(2),1)
         IF( ounit .GE. 0 ) CALL MAGTAP(ounit,SCR,nwrds,31,ISTAT)       ! WRITE IT TO TAPE
         IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,SCR,nwrds,31,ISTAT)     ! WRITE IT TO TAPE
  290 CONTINUE
      IF( ounit .GE. 0 ) CALL MAGTAP(ounit,BUF,NWRDS,31,ISTAT)
      IF( ounit2 .GE. 0 ) CALL MAGTAP(ounit2,BUF,NWRDS,31,ISTAT)
      CALL FREFIL(3,SUNIT,ISTAT)                                        ! FREE  AND DELETE THE TEMPORARY FILE
C****
C****     THE TRACE IS ON TAPE!!!
C****
  300 CONTINUE
      NDONE=NDONE+1
      IF(LBUF(7).EQ.0) GO TO 307                                        ! ARE WE WRITING A GATHER?
      IF(LBUF(51).EQ.-1) GO TO 308                                      ! IS IT THE END OF A GATHER?
      IF(LBUF(3).EQ.0 .AND. ntrcs .EQ. 1 ) GO TO 340                    ! IS IT A STACKED TAPE?
      RETURN
  307 IF(NDONE.LT.NTRCS) RETURN
C****
C****  WRITE 2 EOFS AFTER THE SHOT IF THE SHOT IS NOT A SINGLE CHANNEL
C****  SHOT.  REMEMBER THAT AN EOF IS 3 INCHES OF GAP, THEREFORE DON'T WRITE
C****  TOO MANY OF THEM OR THE TAPE WILL BE FILLED WITH GAPS!!
c****  NIX that.  The EOF writing really slows some drives down.  Also, the f77
c****   tape I/O doesn't allow backspacing, so it won't work!!!!!!
  308 IF(NTRCS.LE.1) GO TO 340                                          ! DON'T PRINT IF EACH TRACE IS A SHOT
c      CALL MAGTAP(ounit,SCR,IDUM,32,ISTAT)                             !  WRITE AN EOF
c      IF(ISTAT.NE.-2) GO TO 310  
c  309 CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)                             !  BACK OVER THE EOF
c      NDONE=NDONE-1
c      GO TO 202
c  310 CALL MAGTAP(ounit,SCR,IDUM,32,ISTAT)                             !  WRITE AN EOF
c      IF(ISTAT.NE.-2) GO TO 320 
c      CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)                             !  SKIP RECORD BACKWARDS
c      GO TO 309
c  320 CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)                             !  SKIP BACK
c      CALL MAGTAP(ounit,SCR,IDUM,25,ISTAT)
      IF( lbuf(7) .EQ. 0 ) THEN
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32(lbuf(3),1)
          PRINT *,'                   SHOT ',lbuf(3),' WRITTEN ON TAPE'
      ELSE
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) CALL swap32(lbuf(6),1)
          PRINT *,'                     RP ',lbuf(6),' WRITTEN ON TAPE'
      ENDIF
  340 IF(LFOR.NE.0) LFOR=LFOR+1
      LTRCNO=1
      NDONE=0
      RETURN
      END
