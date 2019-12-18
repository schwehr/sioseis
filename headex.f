      SUBROUTINE headex( buf, lbuf, ibuf, dbuf, cbuff, numhdr )
c     The execution phase of SIOSEIS process HEADER, the SEGY trace
c  header changer.
c
c  mod 19 Dec 93 - correct spatial interpolation of REAL variables
c  mod 5 Jan 94 - correct problem when lists have same fno but different
c                  trace numbers
c  mod 12 June 95 - Add clean
c  mod 26 June 95 - Fix incorrect lists when nlists > 2
c  mod 23 Dec 96 - Allow math operations on up to 10 header values.
c  mod 20 June 2001 - Allow multiple header processes and make this reentrant.
c  mod 9 Apr 03 - multiple lists didn't work right (how did it ever work?0
c  mod 10 Apr 03 - Made scr big enough to hold the parameter list!
c                - Read (rddisc) into lscr rather than scr.
c  mod 6 May 03 - Add REV1 PATHNAME
c  mod 12 Jun 03 - Adding another parameter screwed up multiple lists.
c  mod 9 Aug 03 - Giving every trace on multiple shots failed.
c  mod 15 Jun 05 - Do the math in REAL*8
c  mod 28 Jul 06 - Allow unsigned integer numdat & ibuf(58)
c                - Watch out for byte swap when doing unsigned numdat
c  mod 12 Feb 07 - Add parameter SWAP
c  mod 28 Nov 07 - Add NOINC and TRINC
c  mod 7 May 08 - Change fno/lno presets so that if if only one list and fno/lno
c                 are not given, then do all (fno =0, lno = 99999987).
c                - If fno is given, but lno is not, then set lno = fno.
c  mod 14 Apr 14 - add arg cbuff and add B (byte) to "equation"
c                - Add arg dbuf and D (double or 64 bit) to swap
c
      INTEGER harsh, mild
      PARAMETER (mild=2, harsh=4)
      PARAMETER ( maxrep = 70 )
      PARAMETER ( maxswap = 50 * 2 )
      PARAMETER ( MAXHDR = 3 )
      REAL buf(1111), replc(MAXHDR,maxrep)
      REAL*8 dbuf(30)
      INTEGER lbuf(1111)
      INTEGER*2 ibuf(1111), iscr(1)
      CHARACTER*1 cbuff(240)
      INTEGER ihdr1(MAXHDR,120), ihdr2(MAXHDR,120)
      INTEGER lhdr1(MAXHDR,120), lhdr2(MAXHDR,120)
      INTEGER hdr1(MAXHDR,120), hdr2(MAXHDR,120) 
      INTEGER dhdr1(MAXHDR,120), dhdr2(MAXHDR,120)
      INTEGER chdr1(MAXHDR,240), chdr2(MAXHDR,240)    ! watch ff vs f  (cbuff for arcgument and cbuf for rdline)
      INTEGER*4 ltemp
      CHARACTER*1 ctemp(4)
      EQUIVALENCE (ctemp(1),ltemp)
      DIMENSION ireplc1(MAXHDR,maxrep), ireplc2(MAXHDR,maxrep)
      EQUIVALENCE (ireplc1(1,1),replc(1,1))
      DIMENSION fno1(MAXHDR), lno1(MAXHDR), ftr1(MAXHDR), ltr1(MAXHDR),
     &     type1(MAXHDR), clean1(MAXHDR), trinc1(MAXHDR), noinc1(MAXHDR)
      DIMENSION fno2(MAXHDR), lno2(MAXHDR), ftr2(MAXHDR), ltr2(MAXHDR),
     &     type2(MAXHDR), clean2(MAXHDR), trinc2(MAXHDR), noinc2(MAXHDR)
      INTEGER fno1, lno1, ftr1, ltr1, type1, clean1, trinc1, trinc2
      INTEGER fno2, lno2, ftr2, ltr2, type2, clean2
      DIMENSION iswap1(maxswap), iswap2(maxswap)
      REAL*8 dtemp1, dtemp2, dtemp3
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT,isite, maxsamps,
     & nbperw, ireal
      COMMON /header/ iunit(MAXHDR), nlists(MAXHDR), nwrds
      CHARACTER*80 cheadr(40)
      COMMON /binhdr/ ibhead(200)
      INTEGER*2 ibhead
      COMMON /SEGY/ header(60)             !  only used/filled when leaving headex
      COMMON /SIOLN3/ CBUF		! used by rdline
      CHARACTER*200 CBUF
      COMMON /sioln4/ ICHAR4, NCHARS4, iprint4, lunpo4
      COMMON /readt/ itunit, numtrhdr, numdat, ihunit, ireeln, intrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     &               nrskip, nfiles
      DIMENSION lscr(1000), scr(1000)
      EQUIVALENCE (lscr(1),scr(1)), (lscr(1),iscr(1))
      DIMENSION mlists(MAXHDR), ispat1(MAXHDR), ltype1(MAXHDR),
     &          itype1(MAXHDR), lprint1(MAXHDR), lprint2(MAXHDR),
     &          ispat2(MAXHDR), itype2(MAXHDR), ltype2(MAXHDR),
     &          jtype1(MAXHDR), jtype2(MAXHDR), jtype3(MAXHDR),
     &          iop(MAXHDR)
      SAVE
      DATA mlists/MAXHDR*0/, fno2/MAXHDR*-1/, lno2/MAXHDR*-1/
c****
c****  get the first two lists
c****
    1 IF( mlists(numhdr) .EQ. 0 ) THEN
          CALL podisc( iunit(numhdr), 1, 0 )
          CALL rddisc( iunit(numhdr), lscr, nwrds, istat )
          mlists(numhdr) = 1
          fno1(numhdr) = lscr(1)
          lno1(numhdr) = lscr(2)
          ftr1(numhdr) = lscr(3)
          ltr1(numhdr) = lscr(4)
          lprint1(numhdr) = lscr(5)
          ispat1(numhdr) = lscr(6)
          type1(numhdr) = lscr(7)
          ltype1(numhdr) = lscr(8)
          itype1(numhdr) = lscr(9)
          clean1(numhdr) = lscr(10)
          lunrev1 = lscr(11)
          noinc1(numhdr) = lscr(12)
          trinc1(numhdr) = lscr(13)
          DO 10 i = 1, 120
             ihdr1(numhdr,i) = lscr(13+i)
             lhdr1(numhdr,i) = lscr(13+120+i)
             hdr1(numhdr,i) = lscr(13+120+120+i)           ! integer index
             dhdr1(numhdr,i) = lscr(13+120+120+120+i)           ! integer index
             chdr1(numhdr,i) = lscr(13+120+120+120+120+i)           ! integer index
   10     CONTINUE
          DO  i = 1, maxrep
              ireplc1(numhdr,i) = lscr(13+120+120+120+120+120+i)
          ENDDO
          DO i = 1, maxswap
             iswap1(i) = lscr(13+120+120+120+120+120+maxrep+i)
          ENDDO
          IF( mlists(numhdr) .LT. nlists(numhdr) ) THEN
              CALL rddisc( iunit(numhdr), lscr, nwrds, istat )
              mlists(numhdr) = mlists(numhdr) + 1
c  The following gives a IEEE error on the Sun, but the answer is ok.
              fno2(numhdr) = lscr(1)
              lno2(numhdr) = lscr(2)
              ftr2(numhdr) = lscr(3)
              ltr2(numhdr) = lscr(4)
              lprint2(numhdr) = lscr(5)
              ispat2(numhdr) = lscr(6)
              type2(numhdr) = lscr(7)
              ltype2(numhdr) = lscr(8)
              itype2(numhdr) = lscr(9)
              clean2(numhdr) = lscr(10)
              lunrev2 = lscr(11)
              noinc2(numhdr) = lscr(12)
              trinc2(numhdr) = lscr(13)
              DO i = 1, 120
                 ihdr2(numhdr,i) = lscr(13+i)
                 lhdr2(numhdr,i) = lscr(13+120+i)
                 hdr2(numhdr,i) = lscr(13+120+120+i)
                 dhdr2(numhdr,i) = lscr(13+120+120+120+i)
                 chdr2(numhdr,i) = lscr(13+120+120+120+120+i)           ! integer index
              ENDDO
              DO  i = 1, maxrep
                  ireplc2(numhdr,i) = lscr(13+120+120+120+120+120+i)
              ENDDO
              DO i = 1, maxswap
                 iswap2(i) = lscr(13+120+120+120+120+120+120+maxrep+i)
              ENDDO
          ENDIF
c****     Clean the binary header once and only once
          IF( IAND(clean1(numhdr),1) .NE. 0 ) THEN
              CALL podiscb( ihunit, 1, 3200 )
              CALL rddiscb( ihunit, ibhead, 400, istat )
              DO i = 1, 6
                 ibhead(i) = 0
              ENDDO
              ibhead(10) = 0
              ibhead(12) = 0
              ibhead(14) = 0
              DO i = 16, 200
                 ibhead(i) = 0
              ENDDO
              CALL podiscb( ihunit, 1, 3200 )
              CALL wrdiscb( ihunit, ibhead, 400 )
          ENDIF
c****     Do the SEG-Y Rev 1 stuff once and only once
c****     Assume the binary header is in common
          IF( lunrev1 .NE. 0 ) THEN
c****         The user want to do something with Rev1 Extension records
              nextr = ibhead(153)
              IF( ibhead(151) .NE. 0 ) THEN
                  IF( ibhead(153) .EQ. -1 ) THEN                        ! we don't know how many records there are.
                      nextr = 0
                      nlines = 0
   20                 DO i = 1, 40
                         cheadr(i) = ' '
                      ENDDO
                      DO i = 1, 40
                         CALL rline1( lunrev1 )
                         IF( nchars4 .LT. 0 ) GOTO 21
                         nlines = i
                         cheadr(i) = cbuf(1:78)
                      ENDDO
   21                 CALL upcase( cheadr(1), 11 )
                      nextr = nextr + 1
c****                 This will fail if there are spaces after the '(('
                      IF( cheadr(1)(1:11) .NE. '((ENDTEXT))' .AND. 
     &                    nchars4 .GE. 0 ) GOTO 20
                  ENDIF
              ENDIF
              IF( nextr .GT. 0 ) nextr = nextr - 1                      ! write over the last record
              ltemp = 3200 + 400 + nextr*3200
              CALL podiscb( ihunit, 0, ltemp )
              IF( lunrev1 .EQ. -1 ) THEN                                ! The user's file doesn't exist
c****             write a SIOSEIS SEGY stanza
                  cheadr(1) = '(( SIOSEIS SEG-Y ))'
                  cheadr(2) = 
     &'1. Deep water delay (trace header bytes 109-110) may be used.'
                  cheadr(3) =
     &'2. Deep water delay may vary on each trace.'
                  cheadr(4) =
     &'3. Number of samples (bytes 115-116) may vary on each trace.'
                  cheadr(5) =
     &'4. Shot time millisecond replaces "time basis" (bytes 167-168).'
                  cheadr(6) =
     &'5. Trace id (bytes 29-30) = 28 means non-seismic metadata trace.'
                  cheadr(7) =
     &'6. CDP gathers and fold are variable.'
                  cheadr(8) =
     &'7. 32 bit Number of samples permitted by eliminating Lag Time B.'
                  DO i = 9, 40
                     cheadr(i) = ' '
                  ENDDO
                  DO i = 1, 40
                     cheadr(i)(79:79) = CHAR(13)
                     cheadr(i)(80:80) = CHAR(10)
                  ENDDO
                  CALL wrdiscb( ihunit, cheadr, 3200 )
                  nextr = nextr + 1
              ELSE
   22             CONTINUE
                      nlines = 0
                      DO i = 1, 40
                         cheadr(i) = ' '
                      ENDDO
                      DO i = 1, 40
                         CALL rline1( lunrev1 )
                         IF( nchars4 .LT. 0 ) GOTO 23
                         nlines = i
                         cheadr(i) = cbuf(1:78)
                      ENDDO
   23                 CONTINUE
                      nextr = nextr + 1
                      DO i = 1, 40
                         cheadr(i)(79:79) = CHAR(13)
                         cheadr(i)(80:80) = CHAR(10)
                      ENDDO
                      CALL wrdiscb( ihunit, cheadr, 3200 )
                      IF( nchars4 .GE. 0 ) GOTO 22
              ENDIF
              cheadr(1) = '((EndText))'
              DO i = 2, 40
                 cheadr(i) = ' '
              ENDDO
              DO i = 1, 40
                 cheadr(i)(79:79) = CHAR(13)
                 cheadr(i)(80:80) = CHAR(10)
              ENDDO
              CALL wrdiscb( ihunit, cheadr, 3200 )
              ibhead(151) = 256
              ibhead(153) = nextr + 1
              CALL podiscb( ihunit, 0, 3500 )
              CALL wrdiscb( ihunit, ibhead(151), 6 )
          ENDIF
      ENDIF
c****
c****   should we get a new list?
c****
      IF( lbuf(7) .EQ. 0 ) THEN
          nogot = lbuf(3)
          itrno = lbuf(4)
      ELSE
          nogot = lbuf(6)
          itrno = lbuf(7)
      ENDIF
c**** if only 1 list and fno is given and lno is not, then lno = fno
      IF( nlists(numhdr) .EQ. 1 .AND. lno1(numhdr) .EQ. 9999987 .AND.
     &    fno1(numhdr) .NE. -1 ) lno1(numhdr) = fno1(numhdr)
c**** if more than 1 list and lno is not given, the lno = fno
      IF( nlists(numhdr) .GT. 1 .AND. lno1(numhdr) .EQ. 9999987 )
     &    lno1(numhdr) = fno1(numhdr)
      IF( IAND(lprint1(numhdr),2) .NE. 0 ) THEN
          PRINT *,' nogot=',nogot,' itrno=',itrno,' fno1=',fno1(numhdr),
     &         ' lno1=',lno1(numhdr)
          PRINT *,' ftr1=',ftr1(numhdr),' ltr1=',ltr1(numhdr),
     &         ' ispat1=',ispat1(numhdr), ' type1=',type1(numhdr),
     &         ' ltype1=',ltype1(numhdr),' itype1=',itype1(numhdr)
          PRINT *,' fno2=',fno2(numhdr),ftr2(numhdr),ltr2(numhdr),
     &         ' mlists=',mlists(numhdr), ' nlists=',nlists(numhdr)
          PRINT *,' replc1=',(ireplc1(numhdr,i),i=1,14),
     &         ' clean1=',clean1(numhdr)
          PRINT *,' swap1',(iswap1(i),i=1,10)
      ENDIF
      IF( nogot .LT. fno1(numhdr) .AND. ispat1(numhdr) .EQ. 0 ) THEN
          IF( mlists(numhdr) .LE. 2 ) RETURN
          GOTO 1
      ENDIF
   30 IF( nogot .GT. lno1(numhdr) .AND. mlists(numhdr).GT.nlists(numhdr)
     &   .AND. ispat1(numhdr) .EQ. 0 ) RETURN
      IF( nogot .LE. lno1(numhdr) .AND. itrno .LT. ftr1(numhdr) ) RETURN
      IF( itrno .GT. ltr1(numhdr) ) RETURN
c      IF( itrno .GT. ltr1(numhdr) .AND. fno1(numhdr) .NE. fno2(numhdr) )
c     &    RETURN
      IF( nogot .GE. fno2(numhdr) .AND. fno2(numhdr) .NE. -1 ) THEN
          IF( fno2(numhdr) .EQ. fno1(numhdr) .AND. 
     &        itrno .EQ. ftr1(numhdr) ) GOTO 70
          IF( mlists(numhdr) .GT. nlists(numhdr) .AND. 
     &        ispat1(numhdr) .EQ. 0 ) RETURN
          fno1(numhdr) = fno2(numhdr)
          lno1(numhdr) = lno2(numhdr)
          ftr1(numhdr) = ftr2(numhdr)
          ltr1(numhdr) = ltr2(numhdr)
          lprint1(numhdr) = lprint2(numhdr)
          ispat1(numhdr) = ispat2(numhdr)
          type1(numhdr) = type2(numhdr)
          ltype1(numhdr) = ltype2(numhdr)
          itype1(numhdr) = itype2(numhdr)
          clean1(numhdr) = clean2(numhdr)
          lunrev1 = lunrev2
          noinc1(numhdr) = noinc2(numhdr)
          trinc1(numhdr) = trinc2(numhdr)
          DO 50 i = 1, 120
             ihdr1(numhdr,i) = ihdr2(numhdr,i)
             lhdr1(numhdr,i) = lhdr2(numhdr,i)
             hdr1(numhdr,i) = hdr2(numhdr,i)
             dhdr1(numhdr,i) = dhdr2(numhdr,i)
             chdr1(numhdr,i) = chdr2(numhdr,i)
   50     CONTINUE
          DO i = 1, maxrep
             ireplc1(numhdr,i) = ireplc2(numhdr,i)
          ENDDO
          DO i = 1, maxswap
             iswap1(i) = iswap2(i)
          ENDDO
          IF( mlists(numhdr) .LT. nlists(numhdr) ) THEN
              CALL rddisc( iunit(numhdr), lscr, nwrds, istat)
              fno2(numhdr) = lscr(1)
              lno2(numhdr) = lscr(2)
              ftr2(numhdr) = lscr(3)
              ltr2(numhdr) = lscr(4)
              lprint2(numhdr) = lscr(5)
              ispat2(numhdr) = lscr(6)
              type2(numhdr) = lscr(7)
              ltype2(numhdr) = lscr(8)
              itype2(numhdr) = lscr(9)
              clean2(numhdr) = lscr(10)
              lunrev2 = lscr(11)
              noinc2(numhdr) = lscr(12)
              trinc2(numhdr) = lscr(13)
              DO i = 1, 120
                 ihdr2(numhdr,i) = lscr(13+i)
                 lhdr2(numhdr,i) = lscr(13+120+i)
                 hdr2(numhdr,i) = scr(13+120+120+i)
                 dhdr2(numhdr,i) = scr(13+120+120+120+i)
              ENDDO
              DO i = 1, maxrep
                 ireplc2(numhdr,i) = lscr(13+120+120+120+120+i)
              ENDDO
              DO i = 1, maxswap
                 iswap2(i) = lscr(13+120+120+120+120+maxrep+i)
              ENDDO
          ELSE
              fno2(numhdr) = 99999999
          ENDIF
          mlists(numhdr) = mlists(numhdr) + 1
          GOTO 30
      ELSE
          IF( nogot .GT. lno1(numhdr) .AND. ispat1(numhdr).EQ.0) RETURN
      ENDIF
   70 CONTINUE
      IF( noinc1(numhdr) .NE. 9999999 ) THEN
          DO i = fno1(numhdr), lno1(numhdr), noinc1(numhdr)
             IF( nogot .EQ. i ) GOTO 80
          ENDDO
          RETURN
   80     CONTINUE
      ENDIF
      IF( trinc1(numhdr) .NE. 9999 ) THEN
          DO i = ftr1(numhdr), ltr1(numhdr), trinc1(numhdr)
             IF( itrno .EQ. i ) GOTO 90
          ENDDO
          RETURN
   90     CONTINUE
      ENDIF
c****
c****      CHANGE THOSE HEADER VALUES!!!
c****
c****     Do byte swapping first
c****
      DO i = 1, maxswap, 2
         IF( iswap1(i) .GT. 0 ) THEN
             index = iswap1(i+1)
             IF( iswap1(i) .EQ. 1 ) CALL swap16( ibuf(index), 1 )
             IF( iswap1(i) .EQ. 2 ) CALL swap32( lbuf(index), 1 )
             IF( iswap1(i) .EQ. 3 ) CALL swap64( dbuf(index), 1 )
         ENDIF
      ENDDO
c****
c****   type 1 = replc, type 2 = add, type 3 multiply
c****
      factor =FLOAT(nogot-lno1(numhdr))/FLOAT(fno2(numhdr)-lno1(numhdr))
      DO 1000 i = 1, 120, 2
         IF( ihdr1(numhdr,i) .GT. 0 ) THEN
             itemp = ihdr1(numhdr,i+1)
             IF( ispat1(numhdr) .EQ. 1 .AND. nogot.GT.lno1(numhdr).AND.
     &           nogot .LT. fno2(numhdr) )
     &           itemp = NINT(factor * REAL(ihdr2(numhdr,i+1) 
     &                 - ihdr1(numhdr,i+1))
     &                 + REAL(ihdr1(numhdr,i+1)))
             IF( itype1(numhdr) .EQ. 1 ) ibuf(ihdr1(numhdr,i)) = itemp
             IF( itype1(numhdr) .EQ. 2 ) 
     &           ibuf(ihdr1(numhdr,i)) = ibuf(ihdr1(numhdr,i)) + itemp
             IF( itype1(numhdr) .EQ. 3 )
     &           ibuf(ihdr1(numhdr,i)) = ibuf(ihdr1(numhdr,i)) * itemp
             IF( IAND(lprint1(numhdr),2) .NE. 0 ) PRINT *,' ihdr word ',
     &           ihdr1(numhdr,i),' is now ',ibuf(ihdr1(numhdr,i))
         ENDIF
         IF( lhdr1(numhdr,i) .GT. 0 ) THEN
             itemp = lhdr1(numhdr,i+1)
             IF( ispat1(numhdr) .EQ. 1 .AND. nogot.GT.lno1(numhdr) .AND.
     &           nogot .LT. fno2(numhdr) ) itemp = NINT(factor * 
     &             FLOAT(lhdr2(numhdr,i+1) - lhdr1(numhdr,i+1)) +
     &             FLOAT(lhdr1(numhdr,i+1)))
             IF( ltype1(numhdr) .EQ. 1 ) lbuf(lhdr1(numhdr,i)) = itemp
             IF( ltype1(numhdr) .EQ. 2 ) lbuf(lhdr1(numhdr,i)) = 
     &           lbuf(lhdr1(numhdr,i)) + itemp
             IF( ltype1(numhdr) .EQ. 3 ) lbuf(lhdr1(numhdr,i)) = 
     &           lbuf(lhdr1(numhdr,i)) * itemp
             IF( IAND(lprint1(numhdr),2) .NE. 0 ) PRINT *,' lhdr word ',
     &           lhdr1(numhdr,i),' is now ',lbuf(lhdr1(numhdr,i))
         ENDIF
         IF( hdr1(numhdr,i) .GT. 0 ) THEN
             temp = hdr1(numhdr,i+1)   ! this is a value
             IF( ispat1(numhdr) .EQ. 1 .AND. nogot .GT.lno1(numhdr).AND.
     &           nogot .LT. fno2(numhdr) ) temp = factor * 
     &          (hdr2(numhdr,i+1) - hdr1(numhdr,i+1)) + hdr1(numhdr,i+1)
             index = hdr1(numhdr,i)
             IF( type1(numhdr) .EQ. 1 ) buf(index) = temp
             IF( type1(numhdr) .EQ. 2 ) buf(index) = buf(index) + temp
             IF( type1(numhdr) .EQ. 3 ) buf(index) = buf(index) * temp
             IF( IAND(lprint1(numhdr),2) .NE. 0 ) PRINT *,' hdr word ',
     &           hdr1(numhdr,i), ' is now ',buf(index)
         ENDIF
         IF( dhdr1(numhdr,i) .GT. 0 ) THEN
             dtemp = dhdr1(numhdr,i+1)   ! this is a value
             IF( ispat1(numhdr) .EQ. 1 .AND. nogot .GT.lno1(numhdr).AND.
     &           nogot .LT. fno2(numhdr) ) temp = factor *
     &       (dhdr2(numhdr,i+1) - dhdr1(numhdr,i+1)) + dhdr1(numhdr,i+1)
             index = dhdr1(numhdr,i)
             IF( type1(numhdr) .EQ. 1 ) dbuf(index) = dtemp
             IF( type1(numhdr) .EQ. 2 ) dbuf(index)= dbuf(index) + dtemp
             IF( type1(numhdr) .EQ. 3 ) dbuf(index)= dbuf(index) * dtemp
             IF( IAND(lprint1(numhdr),2) .NE. 0 ) PRINT *,' dhdr word ',
     &           dhdr1(numhdr,i), ' is now ',dbuf(index)
         ENDIF
 1000 CONTINUE
      nrep = -7
      DO i = 1, maxrep/7
c  ireplc order is: 1=type, 2=index, 3=type, 4=index or constant, 5=operation, 
c                   6=type, 7=index or constant
c jtypes = 1 means INTEGER*2
c          2       INTEGER*4
c          3       REAL
c          4       constant
c          5       REAL*8
c          6       byte
         nrep = nrep + 7
         IF( ireplc1(numhdr,nrep+1) .GT. 0 ) THEN
             jtype1(numhdr) = ireplc1(numhdr,nrep+1)
             index1 = ireplc1(numhdr,nrep+2)
             jtype2(numhdr) = ireplc1(numhdr,nrep+3)
             index2 = ireplc1(numhdr,nrep+4)
             iop(numhdr) = ireplc1(numhdr,nrep+5)
             jtype3(numhdr) = ireplc1(numhdr,nrep+6)
             index3 = ireplc1(numhdr,nrep+7)
             IF( jtype2(numhdr) .EQ. 1 ) dtemp2 = DFLOAT(ibuf(index2))
             IF( jtype2(numhdr) .EQ. 2 ) dtemp2 = DFLOAT(lbuf(index2))
             IF( jtype2(numhdr) .EQ. 3 ) dtemp2 = buf(index2)
             IF( jtype2(numhdr) .EQ. 4 ) dtemp2 = replc(numhdr,nrep+4)
             IF( jtype2(numhdr) .EQ. 5 ) dtemp2 = dbuf(index2)
             IF( jtype2(numhdr) .EQ. 6 ) THEN
c*****  byte manipulation is messy because of the segy trace header is a mix of 2 & 4 byte integers
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
                 ctemp(4) = cbuff(index2)
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &               CALL swap32(ltemp,1)
                 dtemp2 = ltemp
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
             ENDIF
             IF( jtype3(numhdr) .EQ. 1 ) dtemp3 = DFLOAT(ibuf(index3))
             IF( jtype3(numhdr) .EQ. 2 ) dtemp3 = DFLOAT(lbuf(index3))
             IF( jtype3(numhdr) .EQ. 3 ) dtemp3 = buf(index3)
             IF( jtype3(numhdr) .EQ. 4 ) dtemp3 = replc(numhdr,nrep+7)
             IF( jtype3(numhdr) .EQ. 5 ) dtemp3 = dbuf(index3)
             IF( jtype3(numhdr) .EQ. 6 ) THEN
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
                 ctemp(4) = cbuff(index3)
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &               CALL swap32(ltemp,1)
                 dtemp3 = ltemp
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
             ENDIF
             IF( iop(numhdr) .EQ. 0 ) dtemp1 = dtemp2
             IF( iop(numhdr) .EQ. 1 ) dtemp1 = dtemp2 + dtemp3
             IF( iop(numhdr) .EQ. 2 ) dtemp1 = dtemp2 - dtemp3
             IF( iop(numhdr) .EQ. 3 ) dtemp1 = dtemp2 * dtemp3
             IF( iop(numhdr) .EQ. 4 ) dtemp1 = dtemp2 / dtemp3
             IF( iop(numhdr) .EQ. 5 ) dtemp1 = dtemp2 ** dtemp3
             IF( jtype1(numhdr) .EQ. 1 ) ibuf(index1) = NINT(dtemp1)
             IF( jtype1(numhdr) .EQ. 2 ) lbuf(index1) = NINT(dtemp1)
c****        I'm seeing some weird rounding when doing the REAL*8 to
c****        REAL*4 conversion.  e.g. 36361526.0 becomes 36361528.0
             IF( jtype1(numhdr) .EQ. 3 ) buf(index1) = dtemp1
c                               .EQ. 4   is a constant and can't exist on the left had side
             IF( jtype1(numhdr) .EQ. 5 ) dbuf(index1) = dtemp1
             IF( jtype1(numhdr) .EQ. 6 ) THEN    !  byte
                 ltemp = dtemp1
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 )
     &               CALL swap32(ltemp,1)
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
                 cbuff(index1) = ctemp(4)
                 IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) 
     &               CALL swp_trhdr( ibuf, lbuf )
             ENDIF
         ENDIF
      ENDDO
      IF( IAND(clean1(numhdr),mild) .NE. 0 ) THEN
          lbuf(1) = 0
          lbuf(2) = 0
          ibuf(16) = 0
          DO 1120 i = 21, 30
 1120     ibuf(i) = 0
          DO 1130 i = 33, 36
 1130     ibuf(i) = 0
          DO 1140 i = 44, 45
 1140     ibuf(i) = 0
          DO 1150 i = 47, 52
 1150     ibuf(i) = 0
          DO 1160 i = 60, 78
 1160     ibuf(i) = 0
          DO 1170 i = 85, 120
 1170     ibuf(i) = 0
      ENDIF
      IF( IAND(clean1(numhdr),harsh) .NE. 0 ) THEN
          lbuf(1) = 0
          lbuf(2) = 0
          DO 1210 i = 9, 57
 1210     ibuf(i) = 0
          DO 1220 i = 60, 120
 1220     ibuf(i) = 0
      ENDIF
c****
c****  Save the current trace header in COMMON.  Mainly for XSTAR, but ...
c****
      DO i = 1, numtrhdr
         header(i) = buf(i)
      ENDDO
      numdat = ibuf(58)
      IF( ibuf(58) .LT. 0 ) THEN
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              ibuf(57) = ibuf(58)
              ibuf(58) = 0
          ENDIF
          numdat = lbuf(29)
      ENDIF
      RETURN
      END
