      SUBROUTINE trans
      DIMENSION buf(nsamps), cbuf(ncsamps)
      COMPLEX cbuf
c   transp holds the transpose matrix t
      PARAMETER (isize = 262144 )                                       ! 512x512
      COMMON /transp/ t(isize)
      COMPLEX ct(1)
      EQUIVALENCE (t(1),ct(1))
      LOGICAL first, first1, first2, first3, first4
      SAVE
      DATA first/.TRUE./ first1/.TRUE./, first2/.TRUE./, first3/.TRUE./,
     *     first4/.TRUE./

      ENTRY transinit
      first = .TRUE.
      first1 = .TRUE.
      first2 = .TRUE.
      first3 = .TRUE.
      first4 = .TRUE.
      RETURN

      ENTRY trans1( buf, nsamps, nx, istop )
      IF( first1 ) THEN
          icol = 1
          first1 = .FALSE.
      ENDIF
      index = icol
      DO 200 i = 1, nsamps
         t(index) = buf(i)
         index = index + nx
  200 CONTINUE
      icol = icol + 1
      RETURN


      ENTRY trans2( buf, nsamps, nx, lun, istop )
c****   The first word in file lun is at word 1, not word 0
      IF( first ) THEN
          first = .FALSE.
          CALL podisc( lun, 1, 0 )
c         most systems make you write to the file before you can position them
          DO 300 i = 1, nsamps
             CALL wrdisc( lun, t, nx )
  300     CONTINUE
          CALL wrdisc( lun, t, 1 )                                      ! write an extra word for systems that start at 0!
          maxcol = isize / nsamps                                        ! the maximum number of columns that can be held in t at a time
          maxc = maxcol
      ENDIF
      IF( first2 ) THEN
          icol = 1
          nsofar = 0
          first2 = .FALSE.
      ENDIF

      index = icol
      DO 400 i = 1, nsamps
         t(index) = buf(i)
         index = index + maxcol
  400 CONTINUE
      icol = icol + 1
      IF( istop .NE. 0 ) maxcol = icol - 1
      IF( icol .GT. maxcol ) THEN
          index = 1
          n = icol - 1
          DO 500 i = 1, nsamps 
             ipos = (i-1)*nx+nsofar+1
             CALL podisc( lun, 1, ipos )
             CALL wrdisc( lun, t(index), n )
c       print *,' write ',n,' words at ',ipos,' index=',index,
c     *      ' dat=',t(index),t(index+1),t(index+2)
             index = index + maxc
  500     CONTINUE
          nsofar = nsofar + maxcol
          icol = 1
      ENDIF
      IF( istop .NE. 0 ) CALL podisc( lun, 1, 0 )
      RETURN


      ENTRY trans3( buf,nsamps, nx )
      IF( first3 ) THEN
          icol = 1
          first3 = .FALSE.
      ENDIF
      index = icol
      DO 600 i = 1, nsamps
         buf(i) = t(index)
         index = index + nx
  600 CONTINUE
      icol = icol + 1
      RETURN


      ENTRY trans4( buf,nsamps, nx, lun )
      IF( first4 ) THEN
          maxcol = isize / nsamps                                        ! the maximum number of columns that can be held in t at a time
          icol = maxcol + 1
          nsofar = 0
          first4 = .FALSE.
          n = min0(nx,maxcol)
      ENDIF
      IF( icol .GT. maxcol ) THEN
          index = 1
          DO 800 i = 1, nsamps 
             ipos = (i-1)*nx+nsofar+1
             CALL podisc( lun, 1, ipos )
             CALL rddisc( lun, t(index), n, istat )
c             IF( istat .NE. n ) THEN
c                 PRINT *,' disc read error, ipos=',ipos,' index=',
c     *                   index,' n=',n,' nsamps=',nsamps,' nx=',nx,
c     *                   ' nsofar=',nsofar,' maxcol=',maxcol
c             ENDIF
             index = index + n
  800     CONTINUE
          icol = 1
          nsofar = nsofar + n
      ENDIF
      index = icol
      DO 900 i = 1, nsamps
         buf(i) = t(index)
         index = index + n
  900 CONTINUE
      icol = icol + 1
      RETURN


      ENTRY ctrans1( cbuf, ncsamps, nx, istop )
      IF( first1 ) THEN
          icol = 1
          first1 = .FALSE.
      ENDIF
      index = icol
      DO 1200 i = 1, ncsamps
         ct(index) = cbuf(i)
         index = index + nx
 1200 CONTINUE
      icol = icol + 1
      RETURN


      ENTRY ctrans2( cbuf, ncsamps, nx, lun, istop )
c****  WATCH OUT!  lun starts at word address 0
      IF( first ) THEN
          first = .FALSE.
          CALL podisc( lun, 1, 0 )
c         most systems make you write to the file before you can position them
          DO 1300 i = 1, ncsamps*2
             CALL wrdisc( lun, ct, nx )
 1300     CONTINUE
          maxcol = isize / (ncsamps*2)                                  ! the maximum number of columns that can be held in t at a time
          maxc = maxcol
      ENDIF
      IF( first2 ) THEN
          icol = 1
          nsofar = 0
          first2 = .FALSE.
      ENDIF

      index = icol
      DO 1400 i = 1, ncsamps
         ct(index) = cbuf(i)
         index = index + maxcol
 1400 CONTINUE
      icol = icol + 1
      IF( istop .NE. 0 ) maxcol = icol - 1
      IF( icol .GT. maxcol ) THEN
          index = 1
          n = icol - 1
          DO 1500 i = 1, ncsamps 
             ipos = ((i-1)*nx+nsofar)*2
             CALL podisc( lun, 1, ipos )
             CALL wrdisc( lun, ct(index), n*2 )
c       print *,' write ',n*2,' words at ',ipos,' index=',index,
c     *      ' dat=',t(index),t(index+1),t(index+2)
             index = index + maxc
 1500     CONTINUE
          nsofar = nsofar + maxcol
          icol = 1
      ENDIF
      IF( istop .NE. 0 ) CALL podisc( lun, 1, 0 )
      RETURN


      ENTRY ctrans3( cbuf, ncsamps, nx )
      IF( first3 ) THEN
          icol = 1
          first3 = .FALSE.
      ENDIF
      index = icol
      DO 1600 i = 1, ncsamps
         cbuf(i) = ct(index)
         index = index + nx
 1600 CONTINUE
      icol = icol + 1
      RETURN


      ENTRY ctrans4( cbuf,ncsamps, nx, lun )
      IF( first4 ) THEN
          maxcol = isize / (ncsamps*2)                                  ! the maximum number of columns that can be held in t at a time
          icol = maxcol + 1
          nsofar = 0
          first4 = .FALSE.
          n = min0(nx,maxcol)
      ENDIF
      IF( icol .GT. maxcol ) THEN
          index = 1
          DO 1800 i = 1, ncsamps 
             ipos = ((i-1)*nx+nsofar)*2
             CALL podisc( lun, 1, ipos )
             CALL rddisc( lun, ct(index), n*2, istat )
c             IF( istat .NE. n ) THEN
c                 PRINT *,' disc read error, ipos=',ipos,' index=',
c     *                   index,' n=',n*2,' ncsamps=',ncsamps,' nx=',nx,
c     *                   ' nsofar=',nsofar,' maxcol=',maxcol
c             ENDIF
             index = index + n
 1800     CONTINUE
          icol = 1
          nsofar = nsofar + n
      ENDIF
      index = icol
      DO 1900 i = 1, ncsamps
         cbuf(i) = ct(index)
         index = index + n
 1900 CONTINUE
      icol = icol + 1
      RETURN

      END
