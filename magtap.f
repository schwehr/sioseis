      SUBROUTINE magtap(lun,buffer,nwrds,ifunc,istat)
c Damn.  I thought the statuses were standard now, but the Ewing machine
c indicates that's not so.  I get a -1 status on write, which seems to
c mean a write parity!.  I'm also getting a parity when reading a
c file mark (kstat=18).  Oct. 1997
c  1996 version
c     MAGTAP is a tape I/O subroutine allowing the calling routine to manipulate
c a tape in whatever fashion it desires.  This version of magtap is fully
c compatible with the MAGTAP on the Apollo computer and partially compatible
c with the version on the Prime (the Prime INTEGER arguments are integer*2).
c     This MAGTAP uses 1600 bpi Emulex tape controller (/dev/rmt12, 13, 14, 15).
c     Magtap takes care of assigning the tape drive every time a new tape unit
c is used.  All tape I/O is completed before control is returned to the calling
c routine (thus equivalencing the Prime MAGTAP functions less than 20 to those
c greater than 20 - thus double buffering is not possible!!!).
c
c  ARGUMENTS:
c  LUN    - The tape drive unit number (0, 1, 2, 3, 4, 6).  integer*4
c  BUFFER - The array or buffer to read/write to/from.  The array must be large
c           enough to hold NWRDS 16 bit integers.
c  NWRDS  - The number of 16 bit words to read/write.  integer*4
c  IFUNC  - The mag tape function code.  integer*4|
c         =1 or 21,  NWRDS from the next record are READ.
c         =2 or 22,  Skip file forward one file mark.
c         =3 or 23,  Skip record forward on record.
c         =4 or 24,  Skip file backwards (tape position is in the IRG prior to
c                    the EOF).
c         =5 or 25,  Skip record backwards (tape position is in the IRG prior toc                    the previous record).
c         =10 or 30, Rewind to BOT. (The tape is left online - see entry offlmt
c                    in mag.c for rewind with offline)
c         =11 or 31, Write NWRDS 2 byte words to tape.
c         =12 or 32, Write a file mark.
c         =13 or 33, Return the 8 word mt_get structure
c  ISTAT  - The return status of the tape I/O or the number of words actually
c           read.
c         >0,  The number of 2 byte words read from tape.
c         =-1,  EOF detected on read.
c         =-2,  EOT detected. Record not written on tape!
c         =-5,  An unknown tape drive error has occurred - the status PRINTed
c               can be dissected into bits and diagnosed using the status bits
c               listed in the Emulex controller manual or listed for your
c               convenience in routine mag.c
c         =-6,  A parity has occurred on read - the data transfered may be
c               garbage or it may be good!
c         =-10, Illegal MAGTAP function.
c
c  The following common is used:
c  common /tapes/ idummy,ntries,jdummy
c  where ntries is the number of retries on read errors before giving up (preset
c  to 5). 
c
c  Written and copyrighted (C) by:
c  Paul Henkart, Scripps Institution of Oceanography, La Jolla Ca., May l984
c
c   mod Sept. 96 - The drive status is no longer available, so I
c                  can't check for specific errors.  I just don't
c                  have as much control any more, so get rid of the
c                  drive status checks etc.
c   mod 9 July 02 - Increase maxdrs to 100 and check for bad ones!
c   mod 5 Aug 02 - Add function 33 to return mt_get
c
c  There seem to be 3 read statuses these days:
c           < 0, error
c           = 0, EOF
c           > 0, the number of bytes transferred
c  There seem to be 3 write statuses these days:
c           < 0, error
c           = 0, EOF or EOT
c           > 0, the number of bytes transferred
c
      PARAMETER (maxdrs = 100)                                           /* the maximum number of tape drives
      DIMENSION itapes(maxdrs), lstat(maxdrs)
      DIMENSION buffer(1)
      INTEGER*4 lun,nwrds,ifunc,istat
      INTEGER*4 rdmt,wrmt,weofmt,skffmt,skbfmt,skrfmt,skrbmt,rewmt
      COMMON /tapes/ idummy,ntries,jdummy
      SAVE itapes,lstat
      DATA itapes/maxdrs*0/
c
      istat = 0
c	print *,' magtap, lun=',lun,' ifunc=',ifunc,' nwrds=',nwrds
      IF( lun .LT. 0 .OR. lun+1 .GT. maxdrs ) THEN
          PRINT *,' ***  ERROR  ***  BAD tape unit no.',lun
          STOP
      ENDIF
      IF( itapes(lun+1) .EQ. 0 ) THEN                                   /* make sure the tape has been assigned
          ntimes = 0
          IF( ntries .LE. 0 ) ntries = 1
          itapes(lun+1)=1
          CALL astape(lun,' ',1)
      ENDIF
      IF( ifunc .NE. 1 .AND. ifunc .NE. 21 ) GOTO 100
c****
c****     read a tape record
c****
   10 CONTINUE
      ntimes = 0
      nbytes = nwrds*2
   20 jstat = rdmt(lun,buffer,nbytes)
      ntimes = ntimes+1
   25 CONTINUE
      IF( jstat .GT. 0 ) THEN
          istat = jstat / 2
          lstat(lun+1) = istat
          RETURN
      ENDIF
      IF( nwrds .LE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  magtap can not read ',nwrds,
     *           ' from tape!'
         istat = -5
         RETURN
      ENDIF
      IF( jstat .EQ. 0 ) THEN                                          ! EOF
          istat = -1
          lstat(lun+1) = istat
          RETURN
      ENDIF
      istat = -6
      lstat(lun+1) = istat
      RETURN
c
  100 CONTINUE
      IF( ifunc .NE. 2 .AND. ifunc .NE. 22 ) GOTO 200
c****
c****         skip file forward
c****
      ntimes = 0
  110 ntimes = ntimes + 1
      istat = skffmt(lun)
      lstat(lun+1) = istat
      IF( istat .NE. 0 ) THEN                                           /* ready?
          IF( ntimes .LE. ntries) PRINT *,' ***  ERROR  ***  Tape ',
     *       ' unit ',lun,' error, status=',istat
          CALL SLEEP(15)                                                /* GOTO sleep for 15 seconds
          GOTO 110
      ENDIF
      istat = 0
      RETURN
c****
  200 CONTINUE
      IF( ifunc .NE. 3 .AND. ifunc .NE. 23 ) GOTO 300
c****
c****    skip record forward
c****
      ntimes = 0
  210 ntimes = ntimes + 1
      istat = skrfmt(lun)
      lstat(lun+1) = istat
      IF( istat .EQ. -1 ) istat = 0                                     /* EOF gives -1!
      IF( istat .NE. 0 ) THEN                                           /* ready?
          IF( ntimes .LE. ntries) PRINT *,' ***  ERROR  ***  Tape ',
     *       ' unit ',lun,' error, status=',istat
          CALL SLEEP(15)                                                /* GOTO sleep for 15 seconds
          GOTO 210
      ENDIF
      RETURN

c****
  300 CONTINUE
      IF( ifunc .NE. 4 .AND. ifunc .NE. 24) GOTO 400
c****
c****      skip file backwards
c****
      ntimes = 0
  310 ntimes = ntimes + 1
      istat = skbfmt(lun)
      lstat(lun+1) = istat
      IF( istat .NE. 0 ) THEN
          IF( ntimes .LE. ntries) PRINT *,' ^G***  ERROR  ***  Tape ',
     *       ' unit ',lun,' error, status=',istat
          PRINT *,' Sun permits file skip backwards on 1/2" tape only.'
          CALL SLEEP(15)
	  GOTO 310
      ENDIF
      istat = 0
      RETURN
c****
  400 CONTINUE
      IF( ifunc .NE. 5 .AND. ifunc .NE. 25 ) GOTO 450
c****
c****   skip a record backwards 
c****  Sun no longer permits backspace over a file mark!
c****
      ntimes = 0
  410 ntimes = ntimes + 1
      istat = skrbmt(lun)
      IF( istat .EQ. -1 ) istat = 0                                     /* eof?
      lstat(lun+1) = istat
      IF( istat .NE. 0 ) THEN                                           /* ready?
          IF( ntimes .LE. ntries) PRINT *,' ***  ERROR  ***  Tape ',
     *       ' unit ',lun,' error, status=',istat
          CALL SLEEP(15)                                                /* GOTO sleep for 15 seconds
          GOTO 410
      ENDIF
      IF( istat .NE. 0 )
     *    PRINT *,' istat=',istat,' jstat=',jstat,' kstat=',kstat
      RETURN

c****
  450 CONTINUE
      IF( ifunc.ne.10.and.ifunc.ne.30) GOTO 500
c****
c****    rewind
c****
      ntimes = 0
  460 ntimes = ntimes + 1
      istat = rewmt(lun)
c****  For some reason I'm gettin a -1 sometimes.  Evereything is ok though
      istat = 0
      lstat(lun+1) = istat
      IF( istat .NE. 0 ) THEN                                           /* ready?
          IF( ntimes .LE. ntries) PRINT *,' ***  ERROR  ***  Tape ',
     *       ' unit ',lun,' is OFF-LINE.'
          CALL SLEEP(15)                                                /* GOTO sleep for 15 seconds
          GOTO 460
      ENDIF
      RETURN
c****
  500 CONTINUE
      IF( ifunc .NE. 11 .AND. ifunc .NE. 31) GOTO 600
c****
c****    write a record
c****
      nbytes = nwrds*2
      ntimes = 0
  510 CONTINUE
      jstat = wrmt(lun,buffer,nbytes)
      istat = jstat/2
      lstat(lun+1) = istat
      IF( jstat .EQ. nbytes ) RETURN
      ntimes = ntimes+1
      IF( nwrds .LE. 0 .OR. nbytes .GT. 32768 ) THEN
          PRINT *,' ***  ERROR  ***  magtap can not write ',nwrds,
     *         ' to tape!'
          istat = -6
          RETURN
      ENDIF
      IF( jstat .EQ. -1 ) THEN
          ntimes = ntimes + 1
          IF( ntimes .LE. ntries) THEN
               PRINT *,' ***  ERROR  ***  Tape drive ',lun,
     &             ' is OFFLINE or WRITE PROTECTED.'
               CALL SLEEP (15)
               GOTO 510
          ENDIF
      ENDIF
      IF( jstat .EQ. 0 ) THEN
c          PRINT *,' TAPE DRIVE ',lun,' error, assuming E.O.T.' 
          istat = -2
          lstat(lun+1) = istat
          RETURN
      ENDIF
      PRINT *,' TAPE DRIVE ',lun,' unknown error status ',jstat
      istat = -6
      RETURN
c****
  600 CONTINUE
      IF( ifunc .NE. 12 .AND. ifunc .NE. 32 ) GOTO 700
c****
c****    write an end of file mark
c****
c****
      ntimes = 0
  610 ntimes = ntimes + 1
      istat = weofmt(lun)
      lstat(lun+1) = istat
      IF( istat .LT. 0 ) THEN
c**** assume an error is E.O.T.
          istat = -2
          RETURN
      ENDIF
      IF( istat .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Tape drive ',lun,
     &       ' is offline or write protected.'
          CALL SLEEP(15)
          GOTO 610
      ENDIF
      istat = 0 
      lstat(lun+1) = istat
      RETURN
c****
  700 CONTINUE
      IF( ifunc .NE. 40) GOTO 800
      istat = lstat(lun+1)
      RETURN
c****
c****   Return the 8 word mt_get structure
c****
  800 CONTINUE
      IF( ifunc .NE. 33 ) GOTO 900
      DO i = 0, 7
         CALL getmtget( lun, i, buffer(i+1) )
      ENDDO
      RETURN
c****
c****     incorrect tape function requested
c****
  900 CONTINUE
      PRINT *,' ***  ERROR  *** Illegal tape function requested on ',
     *  ' drive',lun
      istat = -10
      RETURN
      END 
