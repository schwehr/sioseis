      SUBROUTINE UDECEX( SCR, lscr, istop )
C     UDECEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS UDECON
C  (USER DECONVOLUTION).  THE USER'S PARAMETERS MUST BE IN
C  DISC FILE MUNIT (IN COMMON /UDECON/) AND THE TRACE WITH TRACE HEADER
C  MUST BE IN MEMORY ARRAY BUF.  DECON WINDOW TIMES FOR TRACES BETWEEN
C  THOSE SHOTS OR RPS DESCRIBED BY THE USER ARE CALCULATED BY LINEAR
C  INTERPOLATION.
C
C  ARGUMENTS:
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.
C
C  COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, AUGUST 1986
c
c   mod 17 Jun 10 - change the print output to be 1 value per line
C
C
      DIMENSION scr(111), lscr(111)
      COMMON /UDECON/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/ A(32766)
      INTEGER oprint
      LOGICAL FIRST
      SAVE
      DATA FIRST/.TRUE./,mlists/0/,inpts/0/,icross/0/,iauto/0/,idesir/0/
C****
C****     FIND THE PARAMETER LIST (ON DISC)
C****
      IF( FIRST ) THEN
          FIRST=.FALSE.
          CALL PODISC(MUNIT,1,0)                                        ! /* REWIND THE PARAMETER FILE
      ENDIF
      istop = 0
      nread = 0
      next = 1
      CALL RDDISC(MUNIT,SCR,9,ISTAT)                                    ! /* READ A PARAMETER LIST INTO SCR
      mlists = mlists + 1
      lprint = lscr(1)
      prewhi = scr(2)
      nfpts = lscr(3)
      oprint = lscr(4)
      ninpts = lscr(5)
      ndesir = lscr(6)
      nauto = lscr(7)
      ncross = lscr(8)
      ntypes = lscr(9)
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' lprint=',lprint,' prewhi=',prewhi,' nfpts=',nfpts
          PRINT *,' ninpts=',ninpts,' ndesir=',ndesir,' nauto=',nauto
          PRINT *,' ncross=',ncross,' ntypes=',ntypes,' iatype=',iatype
          PRINT *,' ntypes=',ntypes
      ENDIF
  100 CONTINUE
      CALL rddisc( munit, iatype, 1, istat)
      nread = nread + 1
      IF( iatype .EQ. 9991 ) THEN                                       ! /* is it input data?
          IF( inpts .EQ. 0 ) THEN
              inpts = next
              next = next + ninpts
          ENDIF
          CALL rddisc( munit, scr(inpts), ninpts, istat )               ! /* read the input
      ENDIF
      IF( iatype .EQ. 9992 ) THEN                                       ! /* is it the desired output?
          IF( idesir .EQ. 0 ) THEN
              idesir = next
              next = next + ndesir
          ENDIF
          CALL rddisc( munit, scr(idesir), ndesir, istat )              ! /* read the desired o
          IF( nfpts .EQ. 0 ) nfpts = ndesir
      ENDIF
      IF (iatype .EQ. 9993 ) THEN                                       ! /* is it the autocorrelation
          IF( iauto .EQ. 0 ) THEN
              iauto = next
              next = next + nauto
          ENDIF
          CALL rddisc( munit, scr(iauto), nauto, istat )                ! /* read it and the next
      ENDIF
      IF( iatype .EQ. 9994 ) THEN                                       ! /* is it the crosscorrelation
          IF( icross .EQ. 0 ) THEN
              icross = next
              next = next + ncross
          ENDIF
          CALL rddisc( munit, scr(icross), ncross, istat )
      ENDIF
      IF( nread .LT. ntypes ) GO TO 100                                 ! /* any more data types to be read?
c****
c****   set up the indices
c****   assume that if input is given, there is a desired output
c****   assume that if there is an autocorrelation there is a crosscorrelation
c****   assume that if there is an input, there is not an autocorrelatiobn
c****
      IF( IAND( lprint,2 ) .NE. 0 ) THEN
          PRINT *,' prewhi=',prewhi,' nfpts=',nfpts,' ninpts=',ninpts,
     *         ' ndesir=',ndesir,' ncross=',ncross,' nauto=',nauto
          PRINT *,' inpts=',inpts,' idesir=',idesir,' icross=',icross,
     *         ' iauto=',iauto
          IF( ninpts .NE. 0 ) THEN
              i1 = inpts + 10
              PRINT *,' input',(scr(i),i=inpts,i1)
          ENDIF
          IF( ndesir .NE. 0 ) THEN
              i1 = idesir + 10
              PRINT *,' desire',(scr(i),i=idesir,i1)
          ENDIF
          IF( nauto .NE. 0 ) THEN
              i1 = iauto + 10
              PRINT *,' auto',(scr(i),i=iauto,i1)
          ENDIF
          IF( ncross .NE. 0 ) THEN
              i1 = icross + 10
              PRINT *,' cross',(scr(i),i=icross,i1)
          ENDIF
      ENDIF
c****
c****
c****
      IF( ninpts .NE. 0 ) THEN                                          ! /* make the cross and auto correlations
          icross = idesir + ndesir
          ncross = nfpts
          IF( IAND(lprint,2) .NE. 0 ) print *,' idesir=',idesir,
     &       ' ndesir=',ndesir,' idesir=',idesir, ' ninpts=',ninpts,
     &       ' icross=',icross,' nfpts=',nfpts,' iauto=',iauto
          iauto = icross + nfpts
          nauto = nfpts
          next = nauto + nauto
          IF( iuseap .EQ. 0 ) THEN
              CALL convo(+2,scr(inpts),ndesir,scr(idesir),ninpts,
     *             scr(icross),nfpts)                                   ! /* do the crosscorrelation
              CALL convo(+2,scr(inpts),ninpts,scr(inpts),ninpts,
     *             scr(iauto),nfpts)                                    ! /* do the autocorrelation
          ELSE
              CALL APPUT(scr(inpts),inpts,ninpts,2)
              CALL APWD
              CALL CONV(inpts,1,inpts,1,iauto,1,nfpts,ninpts-nfpts)
              CALL APPUT(scr(idesir),idesir,ndesir,2)
              CALL APWAIT
c ninpts must be GE ndesir+nfpts
              CALL CONV(inpts,1,idesir,1,icross,1,nfpts,ndesir)
          ENDIF
      ELSE
          IF( iuseap .EQ. 1 ) THEN
              CALL APPUT(scr(inpts),inpts,ninpts,2)
              CALL APPUT(scr(idesir),idesir,ndesir,2)
              CALL APWD
          ENDIF
      ENDIF
c****
c****    add in the prewhittener  (add a percentage of the 0 lag to the 0 lag)
c****
      IF( prewhi .NE. 0. ) THEN
          IF( iuseap .EQ. 0 ) THEN
              scr(iauto) = scr(iauto)+scr(iauto)*prewhi/100.
          ELSE
              CALL APPUT(prewhi/100.,next,1,2)                          ! /* put the prewhittener in the ap at n
              CALL APWD
              CALL VSMA(iauto,1,next,iauto,1,iauto,1,1)
          ENDIF
      ENDIF
c****
c****   print the cross and auto correlation
c****
      IF( IAND(oprint,1) .NE. 0 ) THEN
          PRINT *,' The crosscorrelation is:'
          IF( iuseap .NE. 0 ) THEN                                      ! /* get the data out of the ap
              CALL APWR
              CALL APGET(scr(icross),icross,ncross,2)
              CALL APWD
          ENDIF
          j = icross
          DO i = 1, ncross, 10
             PRINT *,(scr(k),k=j,j+9)
             j = j + 10
          ENDDO
      ENDIF
      IF( IAND(oprint,2) .NE. 0 ) THEN
          PRINT *,' The autocorrelation is:'
          IF( iuseap .NE. 0 ) THEN                                      ! /* get the data out of the ap
              CALL APWR
              CALL APGET(scr(iauto),nauto,nauto,2)
              CALL APWD
          ENDIF
          j = iauto
          DO i = 1, nauto, 10
             PRINT *,(scr(k),k=j,j+9)
             j = j + 10
          ENDDO
      ENDIF
c****
c****    now do the wiener-levinson
c****
      ifilt = next                                                      ! /* figure out the ap addresses
      next = next + nfpts
      iprede = next
      next = next + nfpts
      ioutp = next

c      IF( iuseap .EQ. 0 ) THEN
c          CALL eureka( nfpts, scr(iauto), scr(icross), scr(ifilt),
c     *                 scr(iprede) )
          CALL wiener( nfpts, scr(iauto), scr(icross), scr(ifilt),
     *                scr(iprede), 1, ierr )
c      ELSE
c          CALL WIENER(nfpts,iauto,icross,ifilt,iprede,1)
c      ENDIF
c****
c****   print the filter if requested to do so
c****
      IF( IAND(oprint,4) .NE. 0 ) THEN
          PRINT *,' The prediction filter is:'
          IF( iuseap .NE. 0 ) THEN                ! get the data out of the ap
              CALL APWR
              CALL APGET(scr(ifilt),ifilt,nfpts,2)
              CALL APWD
          ENDIF
          j = ifilt
          DO i = 1, nfpts, 10
             PRINT *,(scr(k),k=j,j+9)
             j = j + 10
          ENDDO
      ENDIF
c****
c****    Print the "prediction error"
c****
      IF( IAND(oprint,8) .NE. 0 ) THEN
          PRINT *,' The Wiener-Levinson error is:'
          IF( iuseap .NE. 0 ) THEN
              CALL APWR
              CALL APGET(scr(iprede),iprede,nfpts,2)
              CALL APWD
          ENDIF
          j = iprede
          DO i = 1, nfpts, 10
             PRINT *,(scr(k),k=j,j+9)
             j = j + 10
          ENDDO
      ENDIF
c****
c****      DEconvolve the input (if it exists)
c****
      IF( ninpts .NE. 0 ) THEN
          IF( iuseap .EQ. 0 ) THEN
              CALL convo(-1,scr(inpts),ninpts,scr(ifilt),nfpts,
     *                   scr(ioutp),ninpts+nfpts)
          ELSE
              CALL CONV(inpts,1,inpts+nfpts-1,-1,ioutp,1,ninpts,
     *                  nfpts)
          ENDIF
          IF( IAND(oprint,16) .NE. 0 ) THEN
              PRINT *,' The deconvolved output is:'
              IF( iuseap .NE. 0 ) THEN
                  CALL APWR
                  CALL APGET(scr(ioutp),ioutp,ninpts,2)
                  CALL APWD
              ENDIF
              j = ioutp
              DO i = 1, ninpts, 10
                 PRINT *,(scr(k),k=j,j+9)
                 j = j + 10
              ENDDO
          ENDIF
          IF( IAND( oprint,32) .NE. 0 ) THEN                            ! /* compute the rms error
              IF( iuseap .EQ. 0 ) THEN
                  rms = 0.
                  DO 1000 i = 1, ninpts
                     temp = scr(ioutp+i-1) - scr(inpts+i-1)
                     rms = rms + temp * temp
 1000             CONTINUE
              ENDIF
              rms = SQRT(rms/ninpts)
              PRINT *,' The rms error is:',rms
          ENDIF
      ENDIF



      IF( mlists .EQ. nlists ) istop = 1
      RETURN
      END
