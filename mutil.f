c   mutil is a tape utility program using the Unix fortran tape interface.
c   mutil can read, write, skip file, write file marks
c IT CAN NOT GO IN REVERSE
c.  It also dumps tapes records in many different formats.
c   The commands for tutil are two letter command names followed by a blank
c followed by the number of items you want the command to perform.
c   Use the help (he) command to see what commands are available.
C
C    PAUL HENKART S.I.O. JUNE 20,1978
C
C
      PARAMETER (NTYPES=20)                                              /* THE NUMBER OF COMMANDS
      COMMON /IBUF5/IBUF5(15000),IBUF4(15000)
      DIMENSION OBUF(1),LOBUF(1),IOBUF(1)
      DIMENSION IASGND(10)                                               /* KEEP TRACK OF THE TAPE DRIVES WE'VE ASSIGNED
      INTEGER*2 IOBUF
      EQUIVALENCE (OBUF(1),IBUF5(1)),(LOBUF(1),IBUF5(1)),
     *    (IOBUF(1),IBUF5(1))
      INTEGER*4 LOBUF, untape
      CHARACTER*2  JTYPE(ntypes)
      character*3 itype
      CHARACTER*5 M
      CHARACTER*20 token
      CHARACTER*4 harray(15000)
      CHARACTER*1 hex(16)
      LOGICAL IEOF
      DATA hex/'0','1','2','3','4','5','6','7',
     *         '8','9','A','B','C','D','E','F'/
      DATA IUNIT/0/, NRECS/0/, NWRDS/20000/, icount/1/
      DATA JTYPE/ 'HE',                                                 /* HELP - LIST THE COMMANDS
     2        'SF',                                                     /*  SKIP FILE     - MEANS BACKWARD
     3        'SR',                                                     /*  SKIP RECORD
     4        'RW',                                                     /*  REWIND
     5        'WE',                                                     /*   WRITE EOF
     6        'RD',                                                     /*    READ N RECORDS
     7        'WR',                                                     /*   WRITE N RECORDS
     8        'TT',                                                     /*  TEST TAPE OPTION
     9        'DI',                                                     /*  DUMP N WORDS IN I FORMAT
     *        'DF',                                                     /*  DUMP N WORDS IN F FORMAT
     1        'DA',                                                     /*  DUMP N WORDS IN 40A2 FORMAT
     2        'DE',                                                     /* CONVERT FROM EBCDIC AND PRINT
     3        'DL',                                                     /*  DUMP N  INT*4 WORDS IN I10 FORMAT
     4        'CU',                                                     /*  CHANGE UNIT TO PLAY WITH
     5        'DM',                                                     /*  DUMP IBM 360 FLOATING VALUES
     6        'S1',                                                     /*  swap 16 bit integers
     7        'S3',                                                     /*  swap 32 bit integers
     8        'QU',                                                     /*  QUIT
     9        'UN',                                                     /*  unassign the tape
     *        'DZ'/                                                     /* dump in HEX or packed BCD
      PRINT *,' MUTIL - A tape utilty program useful for manipulating'
      PRINT *,' and examining mag tapes.  The program is driven by two'
      PRINT *,' letter commands followed by the number of items to be'
      PRINT *,' performed.  Type he for help.'
      NWRDSS=NWRDS
  105 PRINT 110
  110 FORMAT(' Enter the device name: (/dev/rmt0)')
      CALL RDLINE                                                        /* READ A LINE FROM THE USER
      CALL GETOKE( token, NCHAR )                                       /* GET A TOKEN
      IF(NCHAR.EQ.0) GO TO 105                                          /* DID THE USER GIVE ANYTHING?
      IUNIT = 0
      CALL astapef77( iunit, token, istat )
  111 PRINT 1
    1 FORMAT (' ENTER A COMMAND - he dl di da de dm dz rd rw wr we sr',
     *     ' sf cu s1 s3 un qu')
      CALL RDLINE                                                        /*  READ A LINE OF INPUT FROM THE USER
      CALL GETOKE(ITYPE,NCHART)                                          /* GET THE FIRST TOKEN INTO STRING ITYPE
      IF(NCHART.EQ.0) GO TO 111                                          /* TRY AGAIN IF NO CHARCTERS INPUT
      CALL GETOKE(M,NCHAR)                                              /* GET THE NUMBER OF THINGS TO DO
      N=1                                                               /* PRESET THE NUMBER OF THINGS TO DO 
      IF(NCHAR.EQ.0) GO TO 120
      CALL DCODE(M,NCHAR,AREAL,ISTAT)
      IF(ISTAT.NE.2) GO TO 111                                          /* WAS THERE AN ERROR?
      N=AREAL                                                           /* FIX THE RESULT
  120 CALL upcase(itype,nchart)
      DO 121 I=1,NTYPES
      NUMBER=I
      IF(ITYPE(1:2).EQ.JTYPE(I)(1:2)) GO TO 123
  121 CONTINUE
      PRINT 122,itype
  122 FORMAT(' ***  ERROR  ***       NO SUCH COMMAND SUCH AS ',a2)
      GO TO 111
  123 GO TO (2100,2200,2300,2400,2500,2600,2700,115,2800,2900,3040,
     *    3070,3000,3100,3200,3300,3400,9999,3500,3600),NUMBER
  115 PRINT 124
  124 FORMAT(' ENTER 1 TO WRITE, 2 TO READ, 3 TO STOP')
      READ (*,*) KTYPE
      NRECS=0
      NRECSR=0
      PRINT 125
  125 FORMAT(' ENTER THE RECORD LENGTH (12000 MAX)')
      READ (*,*) NWRDSS
      GO TO (140,1000,9999), KTYPE
  140 CALL MAGTAPf77(IUNIT,IDUM,IDUM,30,ISTAT)
C
  200 CONTINUE
      CALL MAGTAPf77(IUNIT,IBUF5,NWRDSS,31,ISTAT)
      IF(ISTAT.GT.0) GO TO 158
      IF(ISTAT.EQ.-2) GO TO 115
      PRINT 151,ISTAT
  151 FORMAT(' WRITE STATUS=',I6)
  158 CONTINUE
      NRECS=NRECS+1
      IF(NRECS.LT.100) GO TO 200
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,32,ISTAT)
      PRINT 160
  160 FORMAT(' WROTE EOF')
      NRECS=0
      GO TO 200
C
C
C  NOW READ THE TAPE
C
 1000 CONTINUE
      icount=1
      PRINT 1001
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,30,ISTAT)
 1001 FORMAT(' REWINDING')
      IWRT=0
C
C LOOP TO READ THE RECORDS
 1100 N=NWRDSS
      NRECSR=NRECSR+1
      CALL MAGTAPf77(IUNIT,IBUF5,N,21,ISTAT)
      IF(ISTAT) 1101,1101,1110
 1101 PRINT 1103,ISTAT
 1103 FORMAT(' ISTAT=',I6)
 1110 IF(ISTAT.EQ.-2) GO TO 115
      IF(ISTAT.NE.-1) GO TO 1150
      IF(IEOF) GO TO 1120
      IEOF=.TRUE.
      GO TO 1160
 1120 PRINT 1121
 1121 FORMAT(' LOGICAL END OF TAPE!!!!')
      PAUSE
 1150 IEOF=.FALSE.
      if(ibuf5(1).ne.icount) THEN
          print *,' ****  ERROR **** icount=',icount,' ibuf=',ibuf5(1)
          stop
      ENDIF
      icount=icount+1
 1160 CONTINUE
      GO TO 1100
C
C
 9999 STOP
C
 2100 CONTINUE                                                           /*  LIST ALL THE COMMANDS
      print *,' TUTIL operates with one memory buffer.  This buffer '
      PRINT *,' is filled with zeroes before each tape record is read.'
      PRINT 2101
 2101 FORMAT(' THE FOLLOWING COMMANDS ARE AVAILABLE:',/,5X,
     *          'HE    -  HELP, LISTS ALL COMMANDS',/,5X,
     *          'SF N  -  SKIP N FILES,  -N MEANS BACKWARDS',/,5X,
     *          'SR N  -  SKIP N RECORDS,  -N MEANS BACKWARDS',/,5X,
     *          'RW    -  REWIND',/,5X,
     *          'WE N  -  WRITE N EOFS',/,5X,
     *          'RD N  -  READ N RECORDS',/,5X,
     *          'WR N  -  WRITE N DUMMY RECORDS',/,5X,
     *          'TT    -  TEST TAPE ROUTINE.',/,5X,
     *    'DI N  -  DUMP N WORDS TO TERMINAL IN I FORMAT',/,5X,
     *    'DF N  -  DUMP N WORDS TO TERMINAL IN F FORMAT.',/,5X,
     1    'DL N  -  DUMP N INT*4 WORDS IN I10 FORMAT',/,5X,
     2    'DA N  -  DUMP N CHARACTERS IN 20A4 FORMAT',/,5X,
     3    'DE N  -  CONVERT N CHARACTERS FROM EBCDIC AND PRINT IN ',
     3   '20A4 FORMAT',/,5X,
     3    'DZ N  -  DUMP n words in HEX or packed BCD',/,5x,
     2  'CU N   -  CHANGE THE LUN NUMBER TO READ OR WRITE FROM.',/,5X,
     *    'DM N  -  DUMP N IBM 360 FLOATING POINT VALUES.')
       PRINT 2102
 2102 FORMAT('S1 N  -  SWAP BYTES IN N 16 BIT INTEGERS.',/,5X,
     *  'S3 N  -  SWAP BYTES IN N 32 BIT INTEGERS.',/,5X,
     *  'UN  -  Unassign the current tape drive.',/,5x,
     3    'QU   -  QUIT THE JOB')
      GO TO 111
CC
C
 2200 CONTINUE                                                           /*  SKIP FILE
      IFUN=22
      IF(N.LT.0) IFUN=24
      MM=IABS(N)
      IF(MM.LE.0) GO TO 2251
      DO 2250 I=1,MM
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,IFUN,ISTAT)
      IF(ISTAT.EQ.-1 .OR. istat .EQ. 0) GO TO 2250
      PRINT 2210,I,ISTAT
 2210 FORMAT(' ***  ERROR  ***    THE',I4,' SKIP FILE ABORTED WITH',
     *  ' STATUS=',I6)
      GO TO 111
 2250 CONTINUE
 2251 CONTINUE
      PRINT 2260,N
 2260 FORMAT(1X,I5,' FILES SKIPPED.')
      GO TO 111
C
C
 2300 CONTINUE                                                           /*  SKIP RECORDS
      IFUN=23
      ISTAT=0
      IF(N.LT.0) IFUN=25
      MM=IABS(N)
      DO 2350 I=1,MM
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,IFUN,ISTAT)
 2350 CONTINUE
      PRINT 2360,N
 2360 FORMAT(1X,I6,' TAPE RECORDS SKIPPED.')
      GO TO 111
C
C
 2400 CONTINUE
      PRINT 2401
 2401 FORMAT(' REWINDING')
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,30,ISTAT)                              /* REWIND
      if(istat.eq.0) go to 111
      IF(ISTAT.EQ.-3) GO TO 111
      PRINT 2410,ISTAT
 2410 FORMAT(' ***  ERROR  ***  REWIND ABORTED WITH STATUS=',I4)
      GO TO 111
C
C
C
 2500 CONTINUE                                                             /*  WRITE EOF
      DO 2550 I=1,N
      CALL MAGTAPf77(IUNIT,IDUM,IDUM,32,ISTAT)
      IF(ISTAT.GE.0) GO TO 2550
      PRINT 2510,I,istat
 2510 FORMAT(' ***  ERROR  ***  THE',I4,' FILE MARK ABORTED WITH ',
     *  'STATUS=',I6)
 2550 CONTINUE
      PRINT 2560,N
 2560 FORMAT(1X,I3,' FILE MARKS WRITTEN.')
      GO TO 111
C
C
C
 2600 CONTINUE                                                            /*   READ N RECORDS
      NWRD=NWRDS
      DO 2650 I=1,N
      IFUNC=21
      CALL MAGTAPf77(IUNIT,IBUF5(1),NWRD,IFUNC,ISTAT)
      IF(ISTAT.GT.0) GO TO 2640
      IF( istat .EQ. -1) THEN
          PRINT *,' Tape record ',i,' was a file mark.'
          GOTO 2650
      ENDIF
      PRINT 2610,I,ISTAT
 2610 FORMAT(' ***  ERROR  ***  THE',I4,' READ ABORTED WITH STATUS=',I6)
      GO TO 111
 2640 PRINT 2641,I,ISTAT
      MWRDS=ISTAT                                                        /* SET UP THE NUMBER OF WORDS TO WRITE
 2641 FORMAT(1X,' THE ',I4,' TAPE RECORD HAD ',I6,' WORDS')
 2650 CONTINUE
      GO TO 111
C
C
C
 2700 CONTINUE                                                           /* WRITE N RECORDS
      NWRD=4000
      IF(MWRDS.GT.0) NWRD=MWRDS                                          /* WRITE THE NUMBER OF WORDS LAST READ, THUS
C     ALLOWING A ONE RECORD TAPE COPY BY DOING A RD FOLLOWED BY A CU BY A WR
      DO 2750 I=1,N
      ibuf5(1) = icount
      icount = icount + 1
      CALL MAGTAPf77(IUNIT,IBUF5(1),NWRD,31,ISTAT)
      IF(ISTAT.GT.0) GO TO 2750
      PRINT 2710,I,ISTAT
 2710 FORMAT(' ***  ERROR  ***  THE ',I4,' WRITE RECORD ABORTED WITH',
     *   ' STATUS=',I6)
      GO TO 111
 2750 CONTINUE
      PRINT 2760,N
 2760 FORMAT(1X,I4,' RECORDS WRITTEN')
      GO TO 111
C
C
 2800 CONTINUE
      PRINT 2810, (IOBUF(I),I=1,N)
 2810 FORMAT(10(1X,I10))
      GO TO 111
C
C
 2900 CONTINUE
      PRINT 2910, (OBUF(I),I=1,N)
 2910 FORMAT(10(1X,G11.4))
      GO TO 111
C
C
 3000 CONTINUE
      PRINT 3010, (LOBUF(I),I=1,N)
 3010 FORMAT(10(1X,I10))
      GO TO 111
C
C    DUMP N WORDS IN 20A4 FORMAT
C
 3040 CONTINUE
      NN=N/4                                                            /* THE NUMBER OF 32 BIT WORDS
      PRINT 3050, (IBUF5(I),I=1,NN)
 3050 FORMAT(1X,20A4)
      GO TO 111
C
C     CONVERT FROM EBCDIC AND PRINT IN 50A2 FORMAT
C
 3070 CONTINUE
      NN=N/4                                                            /* THE NUMBER OF INTEGER*4 WORDS TO DUMP
      CALL EBCASC(IBUF5,N,IBUF4)
      PRINT 3050, (IBUF4(I),I=1,NN)
      GO TO 111
C
C
 3100 CONTINUE
      IUNIT=N
      IF(IASGND(IUNIT+1).GE.0) GO TO 111                                 /* IS THE TAPE DRIVE ALREADY ASSIGNED?
      CALL ASTAPEf77(IUNIT,m,1)                                             ! open it for read an write
      IASGND(IUNIT+1)=IUNIT
      GO TO 111
C
C
 3200 CONTINUE
      CALL ibm2fp(obuf,n,obuf)
      GO TO 2900
c****
c****    swap bytes of 16 bit integers
c****
 3300 continue
      call swap16(ibuf5,n)
      go to 111
c****
c****    swap bytes of 32 bit integers
c****
 3400 continue
      call swap32(lobuf,n)
      go to 111
c****
c****   Unassign the tape drive
c****
 3500 CONTINUE
      jstat = freetpf77(iunit)
      iasgnd(iunit+1) = 0                                                /* clear the assigment table entry
      GOTO 111
c****
c****    Dump in HEX
c****
 3600 CONTINUE
      DO 3550 i = 1,n
         j = AND(rshift(iobuf(i),12),15) + 1
         harray(i)(1:1) = hex(j)
         j = AND(rshift(iobuf(i),8),15) + 1
         harray(i)(2:2) = hex(j)
         j = AND(rshift(iobuf(i),4),15) + 1
         harray(i)(3:3) = hex(j)
         j = AND(iobuf(i),15) + 1
         harray(i)(4:4) = hex(j)
 3550 CONTINUE
      PRINT 3560,(harray(i),i=1,n)
 3560 FORMAT(10(1x,A4))
      GOTO 111
c****
c****
      END
