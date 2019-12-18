      PROGRAM DUTIL
c    DUTIL is a disk utility program meant for cracking into any disk
c  file and figuring out what it contains.
      PARAMETER ( nopts = 26 )                                          ! the number of DUTIL options
      INTEGER*4 lbuf(50000)
c      INTEGER*8 lltemp
      INTEGER*4 lltemp(2)
      INTEGER*2 ibuf(1)
      REAL buf(1), scr(50000)
      DOUBLE PRECISION dbuf(1)
      CHARACTER*1 cbuf(1)
      EQUIVALENCE( buf(1), lbuf(1) )
      EQUIVALENCE( buf(1), ibuf(1) )
      EQUIVALENCE( buf(1), cbuf(1) )
      EQUIVALENCE( buf(1), dbuf(1) )
      COMMON /sioln2/ ichar, mchars, iprint, lunpo
      CHARACTER*200 filenm, token
      CHARACTER*2 options(nopts)
      CHARACTER*20 thisop
      CHARACTER*1 harray(10000)
      CHARACTER*1000 ctemp
      DATA options/'HE','RD','DR','DZ','DA','DE','DI','DL','DM', 'DD',
     *    'DV','DW','D1','PO','PW','QU','S1','S3','S6','CO','WR','OP',
     *    'CU','RK','AD','FZ'/

      PRINT *,' Enter the disk file pathname.'
      CALL rdline
      CALL getoke( filenm, nchars )
      CALL getfil( 4, lun, filenm, istat )
      IF( istat .NE. 0 ) THEN
          PRINT *,' ***  ERROR  ***  Could not open file ',filenm
          STOP
      ENDIF
      PRINT *,' The file is opened on unit ',lun

  100 CONTINUE
      PRINT *,
     &' Enter an option: he, rd, dr, da, de, dz, di, dl, dm, dd, dv,'
      PRINT *,
     &'  dw, d1, po, pw, qu, s1, s3, s6, co, wr, op, cu, rk, ad, fz'
      CALL rdline
      CALL getoke( thisop, nchars )
      IF( nchars .NE. 2 ) THEN
          PRINT *,' Bad option, try again.'
          GOTO 100
      ENDIF
      CALL upcase( thisop, 2 )
      CALL getoke( token, nchars )
      IF( nchars .GT. 0 ) THEN
          CALL dcode( token, nchars, areal, istat )
          IF( istat .NE. 2 ) THEN
              PRINT *,' Bad number of bytes  ',token(1:nchars)
              GOTO 100
          ENDIF
          nbytes = areal
      ENDIF
      CALL getoke( token, nchars )
      ipoint = 1
      IF( nchars .GT. 0 ) THEN
          CALL dcode( token, nchars, areal, istat )
          IF( istat .NE. 2 ) THEN
              PRINT *,' Bad pointer',token(1:nchars)
              GOTO 100
          ENDIF
          ipoint = areal
      ENDIF


      IF( thisop(1:2) .EQ. 'HE' ) THEN
         PRINT *,' DUTIL help:  dutil is a disk file "utility" program',
     *     ' that allows you to examine a file.  The command format is',
     *     ' a 2 letter code followed by the number of things to do.'
          PRINT *,' he = help'
          PRINT *,' rd = read n bytes'
          PRINT *,' dr = dump n REAL*4 words'
          PRINT *,' da = dump n ASCII characters'
          PRINT *,' de = dump n EBCDIC characters'
          PRINT *,' dz = dump n HEX bytes'
          PRINT *,' di = dump n INTEGER*2 words'
          PRINT *,' dl = dump n INTEGER*4 words'
          PRINT *,' dm = dump n IBM REAL*4 words'
          PRINT *,' dd = dump n DOUBLE PRECISION words'
          PRINT *,' dv = dump n VAX (DEC) words'
          PRINT *,' dw = dump n VAX (DEC) DOUBLE PRECISION words'
          PRINT *,' d1 = dump n 16 bit floating point words'
          PRINT *,' po = position to byte n in the file'
          PRINT *,' pw = position to 32 bit word n.'
          PRINT *,' qu = quit = return to the operating system'
          PRINT *,' s1 = swap n 16 bit integers'
          PRINT *,' s3 = swap n 32 bit integers'
          PRINT *,' s6 = swap n 64 bit integers'
          PRINT *,' co = complement n INTEGER*2 words'
          PRINT *,' wr = write n bytes to the current unit'
          PRINT *,' op = open another file on a new unit'
          PRINT *,' cu = change unit numbers to unit n'
          PRINT *,' rk = reads nbytes from the keyboard (tty)'
          PRINT *,' ad = returns the current disk byte address.'
          PRINT *,' fz = returns the file size.'
          PRINT *,' e.g.  rd 512    means read 512 bytes'
          PRINT *,' '
          GOTO 100
      ENDIF



      IF( thisop(1:2) .EQ. 'RD' ) THEN
          CALL rddiscb( lun, buf, nbytes, istat )
          IF( istat .EQ. nbytes ) THEN
              PRINT *,' Read ',nbytes,' bytes'
          ELSE
              PRINT *,' Disk read failed - status=',istat
          ENDIF
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DR' ) THEN
          PRINT 150,(buf(i),i=1,nbytes)
  150     FORMAT(5(1x,G13.6))
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DZ' ) THEN
          CALL tohex( lbuf, nbytes, harray )
          PRINT 210,(harray(i),i=ipoint,ipoint+nbytes*2-1)                      ! remember: there are 2 hex characters for each byte
  210     FORMAT( 5(1x,8A1) )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DA' ) THEN
          PRINT 310,(cbuf(i),i=ipoint,ipoint+nbytes-1)
  310     FORMAT( 1x,40A1 )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DE' ) THEN
          CALL ebcasc( cbuf(ipoint), nbytes, harray )
          PRINT 320,(harray(i),i=1, nbytes )
  320     FORMAT( 1x,40A1 )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'QU' ) THEN
          CALL frefil( 2, lun, istat )                                  ! release, close but do not delete
          STOP
      ENDIF

      
      IF( thisop(1:2) .EQ. 'PO' ) THEN
          CALL podiscb( lun, 1, nbytes )
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'PW' ) THEN
          CALL podiscun( lun, 1, nbytes )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DI' ) THEN
          PRINT 500,(ibuf(i),i=ipoint,ipoint+nbytes-1)
  500     FORMAT(10(1x,I7))
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DL' ) THEN
          PRINT 520,(lbuf(i),i=ipoint,ipoint+nbytes-1)
  520     FORMAT( 5(1x,I10) )
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'DM' ) THEN
          CALL ibm2fp( buf, nbytes, scr )
          PRINT 150,(scr(i),i=ipoint,ipoint+nbytes-1)
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'DD' ) THEN
          PRINT 150,(dbuf(i),i=ipoint,ipoint+nbytes-1)
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'S1' ) THEN
          CALL swap16( ibuf, nbytes )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'S3' ) THEN
          CALL swap32( lbuf, nbytes )
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'S6' ) THEN
          CALL swap64( lbuf, nbytes )
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'WR' ) THEN
          CALL wrdiscb( lun, buf, nbytes )
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'OP' ) THEN
          PRINT *,' Enter the disk file pathname.'
          CALL rdline
          CALL getoke( filenm, nchars )
          CALL getfil( 4, lun, filenm, istat )
          IF( istat .NE. 0 ) STOP
          PRINT *,' The file is opened on unit ',lun
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'CU' ) THEN
          lun = nbytes
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'RK' ) THEN
          PRINT *,' Enter',nbytes,' characters:'
          READ (*,'(A1000)') ctemp
          DO 900 i = 1, nbytes
  900     cbuf(i) = ctemp(i:i)
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'DV' ) THEN
          CALL dr2iee( buf, nbytes, scr )
          PRINT 150,(scr(i),i=ipoint,ipoint+nbytes-1)
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'DW' ) THEN
          CALL dd2ieee( buf, nbytes, dbuf )
          PRINT 150,(dbuf(i),i=ipoint,ipoint+nbytes-1)
          GOTO 100
      ENDIF


      IF( thisop(1:2) .EQ. 'D1' ) THEN
          CALL mp32ieee( buf, nbytes, scr )
          PRINT 150,(scr(i),i=ipoint,ipoint+nbytes-1)
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'CO' ) THEN
          CALL icomplement( ibuf, n, ibuf )
          GOTO 100
      ENDIF
          
      IF( thisop(1:2) .EQ. 'AD' ) THEN
          CALL adrdisc( lun, itemp )
          PRINT *,' The current disk address is: ',itemp
          GOTO 100
      ENDIF

      IF( thisop(1:2) .EQ. 'FZ' ) THEN
          CALL filsiz(filenm, lltemp )
          PRINT *,' File ',filenm,' is ',lltemp,' bytes long.'
          GOTO 100
      ENDIF

      PRINT *,' Bad option, try again.'
      GOTO 100

      END
