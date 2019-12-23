      SUBROUTINE sortex( buf, lbuf, ibuf )
c    The execution phase of SIOSEIS process SORT, creating a disk file
c a list of disk addresses for process diskin.
c
c  The format of the output sort file is:
c  80 ASCII characters pathname of the input file
c  INTEGER number of traces of the sort file
c  ntrace INTEGER list of disk addresses of traces to read from IPATH
c  ntrace INTEGER list of disk addresses of traces after
c           sort and desort.  The desorted file is not necessarily the
c           same as the input file because of limits.
c  ntrace INTEGER words being the list of trace lengths.
c
c  Copyright (C) 1991 The Regents of the University of California.
c  ALL RIGHTS RESERVED.
c
c  Written by Paul Henkart, Scripps Institution of Oceanography,
c       La Jolla, CA 92093-0225
c  Mod 26 Jul 93 - Secondary key sort had major bug when only 1 thing
c        to sort.
c  Mod 12 Jun 95 - Do limit checks before negating for reverse.
c  Mod 9 Jul 97 - reverse of secondary key didn't work.
c  Mod 11 Jul 97 - separate reverse into reverse1 and reverse2
c  Mod 14 Nov 00 - Up iapsiz from 3500000 to 5000000
c  Mod 2 June 06 - ikey1 and ikey2 gave bad answers
c  Mod 11 June 07 - Check for the maximum number of traces that can be sorted.
c  Mod 19 Sep 08 - Up iapsiz from 5000000 to 10000000
c  Mod 22 Sep 08 - Use unsigned integer arithmetic for disk addresses
c  Mod 23 Jan 09 - remove the above - no need for that contortion
c  Mod 9 Apr 09 - Use unsigned int for the header number of samples per trace
c  Mod 14 May 09 - Above was bad on Intel
c
      INTEGER prime, vaxunix, apollo, vaxvms, cray, convex, ieee
      PARAMETER ( prime = 1, vaxunix = 2, apollo = 3, vaxvms = 4,
     &            cray = 5, convex = 6, ieee = 7 )
      PARAMETER (iapsiz = 10000000)
      COMMON /sort1/ lprint, lunin, lunout, lkey1, ikey1, limit1(2),
     &            lkey2, ikey2, limit2(2), iflag51, reverse1, reverse2
      INTEGER reverse1, reverse2
      REAL limit1, limit2
      COMMON /sort1a/ ipath
      CHARACTER*80 ipath
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps,
     &   nbperw
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr,  lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr, ldesort
c****  Use the ap to store stuff.  Break it into the following chunks.
      PARAMETER ( iaddr = 1, ival1 = 1000001, ival2 = 2000001,
     &            isamp = 3000001, indx1 = 4000001, indx2 = 5000001,
     &            indx3 = 6000001, iscr = 7000001, iorder = 8000001,
     &            iflag = 9000001 )
c  iaddr - The list of disk addresses
c  ival1 - The list of primary values (unsorted)
c  ival2 - The list of secondary values (unsorted)
c  isamp - the list of nsamps (trace lengths) (unsorted)
c  indx1 - The disk addresses in sorted order
c  indx2 - A sublist of secondary values
c  iscr  - The indeces within the secondary sort
c  iorder -
c  iflag - The "end-of sort" flags in sorted order
      REAL    APDATA(0:iapsiz)
      INTEGER IAPDATA(0:iapsiz)
      COMMON /apmem/ apdata
      EQUIVALENCE (APDATA,IAPDATA)
      DIMENSION buf(60), lbuf(111), ibuf(111)
      INTEGER position
      INTEGER*2 ibuf
      DATA ntraces/0/
      SAVE ntraces
c
      IF( ntraces .NE. 0 ) RETURN
      IF( ntraces .EQ. 0 ) CALL wrdisc( lunout, ipath, 20 )
c****
c****   find the data type to see how many bytes are in a data sample
c****
      IF( IAND(lprint,2) .NE. 0 ) PRINT *,lunin, lunout, lkey1, ikey1,
     &    limit1, lkey2, ikey2, limit2, ntraces
      CALL podisc( lunin, 2, 3200/nbperw)
      CALL rddisc( lunin, ibuf, 400/nbperw, istat )
      IF( icompt .EQ. vaxunix .OR. icompt .EQ. vaxvms )
     &    CALL swap16( ibuf(13), 1 )
      idtype = ibuf(13)
      factor = 1.
      IF( idtype .EQ. 3 ) factor = .5
      IF( idtype .EQ. 5 .AND. icompt .EQ. cray ) factor = 2.
c****
c****  Read each and every trace header, remember it's start address
c****  and fetch the primary and secondary sort keys.
c****  position is the disk address in this computer's "word".
c****
      position = 3600 / nbperw
  100 CALL rddisc( lunin, lbuf, numhdr, istat )
      IF( istat .EQ. numhdr ) THEN
          IF( icompt .EQ. vaxunix .OR. icompt .EQ. vaxvms ) THEN
              CALL swap16( ibuf(isampptr), 1 )
c***   ltemp isn't used!
c              CALL ushort2long( ibuf(isampptr), ltemp )
c              CALL swap16( ltemp, 1 )
              IF( lkey1 .NE. 0 ) CALL swap32( lbuf(lkey1), 1 )
              IF( lkey2 .NE. 0 ) CALL swap32( lbuf(lkey2), 1 )
              IF( ikey1 .NE. 0 ) CALL swap16( ibuf(ikey1), 1 )
              IF( ikey2 .NE. 0 ) CALL swap16( ibuf(ikey2), 1 )
          ENDIF
          ntraces = ntraces + 1
          iapdata(ntraces) = position
          position = position + numhdr
          iapdata(indx2) = ntraces
c          nsamps = ibuf(isampptr)
c          CALL long2ushort( ibuf(isampptr), nsamps )
          CALL ushort2long( ibuf(isampptr), nsamps )
          iapdata(isamp+ntraces-1) = nsamps
          itemp = nsamps * factor
          CALL podisc( lunin, 2, itemp )
          position = position + itemp
          IF( lkey1 .NE. 0 ) apdata(ival1+ntraces-1) = lbuf(lkey1)
          IF( ikey1 .NE. 0 ) apdata(ival1+ntraces-1) = ibuf(ikey1)
          IF( limit1(1) .NE. -999999. .AND.  limit1(2).NE.-999999.) THEN
              temp = apdata(ival1+ntraces-1)
              IF( temp .LT. limit1(1) .OR. temp .GT. limit1(2) ) THEN
                  ntraces = ntraces - 1
                  GOTO 100
              ENDIF
          ENDIF
c****     Do the limit checks before negating for reverse!
          IF( reverse1 .EQ. 1 )
     &        apdata(ival1+ntraces-1) = -apdata(ival1+ntraces-1)
          IF( lkey2 .NE. 0 ) apdata(ival2+ntraces-1) = lbuf(lkey2)
          IF( ikey2 .NE. 0 ) apdata(ival2+ntraces-1) = ibuf(ikey2)
          IF( limit2(1) .NE. -999999. .AND.  limit2(2).NE.-999999.) THEN
              temp = apdata(ival2+ntraces-1)
              IF( temp .LT. limit2(1) .OR. temp .GT. limit2(2) ) THEN
                  ntraces = ntraces - 1
                  GOTO 100
              ENDIF
          ENDIF
          IF( reverse2 .EQ. 1 )
     &        apdata(ival2+ntraces-1) = -apdata(ival2+ntraces-1)
          GOTO 100
      ENDIF
      IF( ntraces .EQ. 0 ) THEN
          PRINT *,' ***  ERROR  ***  No traces output from process SORT'
          STOP
      ENDIF
      IF( ntraces .GT. iapsiz/10 ) THEN
          PRINT *,' ***  ERROR  ***   Too many traces to sort.'
          PRINT *,' There are ',ntraces,' traces.  Maximum allowed is ',
     &       iapsiz/10
          STOP
      ENDIF
      CALL wrdisc( lunout, ntraces, 1 )
      CALL indexx( ntraces, apdata(ival1), iapdata(indx1) )
      IF( (limit2(1) .EQ. limit2(2) .AND. limit2(1) .NE. -999999.)
     &    .OR. lkey2 + ikey2 .EQ. 0 )THEN
          n = ntraces
          DO 190 i = 1, ntraces
  190     iapdata(iorder+i-1) = iapdata(indx1+i-1)
          GOTO 400
      ENDIF
      IF( lkey2 .NE. 0 .OR. ikey2 .NE. 0 ) THEN
          DO 150 i = 1, ntraces
  150     iapdata(indx3+i-1) = iapdata(indx1+i-1)
c****
c****  Sorting by the secondary key isn't that easy.  First see what
c****  have the same primary, count them, then sort them, then redo
c****  the indices.
c****
          i = 1
  200     CONTINUE
c****        indx3 has the order after the primary key
             index1 = iapdata(indx3+i-1)
             apdata(indx2) = apdata(ival2+index1-1)
c****        indx2 points to the secondary key values
             n = 1
             DO 250 j = 1, ntraces-i
                index2 = iapdata(indx3+i-1+j)
                apdata(indx2+n) = apdata(ival2+index2-1)
                IF( apdata(ival1+index1-1) .NE. apdata(ival1+index2-1) )
     &              GOTO 260
                n = n + 1
  250        CONTINUE
  260        IF( n .GT. 1 ) THEN
                 CALL indexx( n, apdata(indx2), iapdata(iscr) )
c****            iscr now points to the order due to the secondary
                 DO 280 j = 1, n
                    index = iapdata(iscr+j-1)
                    index1 = iapdata(indx3+i-1+index-1)
c****               save the order
                    iapdata(iorder+i-1+j-1) = index1
                    iapdata(indx1+i-1+j-1) = iapdata(iaddr+index1-1)
c****               indx1 now contains the input address ordered
  280            CONTINUE
             ELSE
                 iapdata(iorder+i-1) = iapdata(indx1+i-1)
                 iapdata(indx1+i-1) = iapdata(iaddr+index1-1)
             ENDIF
             i = i + n
             IF( i .LE. ntraces ) GOTO 200
      ENDIF
      n = 0
c****
c****  now recompute where the input traces would be in the input disk
c****  file were it not for limits.  This is so desort knows where to
c****  put the traces ( all it sees is the sorted file).
c****      Figure out the "end-of"sort" flag too!
C****
  400 iapdata(iscr) = 3600  / nbperw                                    ! nbperw = number of bytes per word
      DO 500 i = 1, ntraces-1
         iapdata(iscr+i) = iapdata(iscr+i-1) + numhdr +
     &            iapdata(isamp+i-1)
         IF( iflag51 .EQ. -99999 ) THEN
             iapdata(iflag+i-1) = -99999
         ELSE
             iapdata(iflag+i-1) = 0
         ENDIF
  500 CONTINUE
      iapdata(iflag+ntraces-1) = -1
      IF( iflag51 .EQ. -99999 ) iapdata(iflag+ntraces-1) = -99999
c**** now order the addresses so that they correspond to the sorted data
c****  and put the address and flag into pairs for a single write
      IF( n .EQ. ntraces ) THEN
          DO 510 i = 1, ntraces
             index1 = iapdata(indx1+i-1)
             iapdata(indx2+i*2-2) = iapdata(iaddr-1+index1)
             iapdata(indx2+i*2-1) = iapdata(iflag+i-1)
  510     CONTINUE
          GOTO 600
      ENDIF
      DO 520 i = 1, ntraces
         j = iapdata(iorder+i-1)
         j1 = iapdata(iorder+i)
         iapdata(i) = iapdata(iscr+j-1)
         IF( iflag51 .NE. -99999 .AND. iapdata(ival1+j-1) .NE.
     &       iapdata(ival1+j1-1) ) iapdata(iflag+i-1) = iflag51
         j = (i-1) * 2
         iapdata(indx2+j) = iapdata(indx1+i-1)
         iapdata(indx2+j+1) = iapdata(iflag+i-1)
  520 CONTINUE
c****
c****   write the disk addresses (in sort order) to disk
c****
  600 CALL wrdisc( lunout, iapdata(indx2), ntraces*2 )
      CALL wrdisc( lunout, iapdata(1), ntraces )
c****
c****  Now write out the trace lengths for desort also.
c****
      CALL wrdisc( lunout, iapdata(isamp), ntraces )
c****
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' Addresses',(iapdata(i),i=1,ntraces)
          PRINT *,' key1',(apdata(i+ival1-1),i=1,ntraces)
          PRINT *,' key2',(apdata(i+ival2-1),i=1,ntraces)
          PRINT *,' addresses & flags',(iapdata(i+indx2-1),
     &               i=1,ntraces*2)
      ENDIF
      CALL frefil( 2, lunin, istat )
      CALL frefil( 2, lunout, istat )
      RETURN
      END
