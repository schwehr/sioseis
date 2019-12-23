      SUBROUTINE flatex( BUF,LBUF,IBUF,SCR,LSCR)
C     FLATEX IS THE EXECUTION PHASE OF THE SEISMIC REFLECTION PROCESS FLATEN..
C  THE USER'S PARAMETERS MUST BE IN DISC FILE MUNIT (IN COMMON /FLATEN/) AND
c  THE TRACE WITH TRACE HEADER MUST BE IN MEMORY ARRAY BUF.
C
C  ARGUMENTS:
C  BUF    - THE TRACE TO BE PROCESSED, INCLUDING THE TRACE HEADER.  THE FIRST
C           DATA SAMPLE MUST BE AT TIME DELAY.  THIS IS THE FLOATING
C           POINT (REAL) TRACE ARRAY.
C  LBUF   - THE LONG INTEGER TRACE ARRAY.  THIS IS REALLY THE SAME AS BUF, BUT
C           PRIME FORTRAN DOESN'T ALLOW EQUIVALENCING ANYTHING TO AN ARGUMENT.
C  IBUF   - THE SHORT INTEGER TRACE ARRAY.  NEEDED FOR 16 BIT TRACE HEADER
C           ADDRESSES.
C  SCR    - A SCRATCH ARRAY FOR READING THE PARAMETERS.  THEREFORE, SCR MUST
C           BE AT LEAST 7 32BIT WORDS BIG.  SCR MAY BE DESTROYED BY THE CALLING
C           ROUTINE.
C  LSCR   - THE SAME SCRATCH ARRAY BECAUSE OF THE EQUIVALENCING PROBLEM.
C
C  WRITTEN AND COPYRIGHTED (C) BY
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, February 1987
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
      PARAMETER (NPARS=9)                                                /* THE LENGTH OF EACH PARAMETER LIST
      PARAMETER (ndeps = 100)
      DIMENSION BUF(111),LBUF(111),IBUF(111),SCR(111),LSCR(111)
      INTEGER*2 IBUF
      COMMON /FLATEN/ MUNIT,NLISTS
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /APMEM/A(32766)
      COMMON /READT/ ILUN,NUMHDR
      INTEGER  FNO
      LOGICAL FIRST
      DIMENSION depths(ndeps)
      SAVE
      DATA FIRST /.TRUE./, llnum/0/, dlast/0./
C****
C****     FIND THE PARAMETER LIST (ON DISC) FOR THIS SHOT (RP)
C****
      IF(IBUF(15).EQ.2) RETURN                                          /* IS IT A DEAD TRACE
      ISIG=0
   10 CONTINUE
      IF( FIRST ) THEN
          FIRST=.FALSE.
          odelay = buf(46)
          CALL PODISC(MUNIT,1,0)                                            /* REWIND THE PARAMETER FILE
          CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)
          ISIG=1                                                            /* SET SIGNAL INDICATING THAT PARAM LIST IS IN SCR
          MLISTS=1
          otime = scr(1)
          vel = scr(2) / 2.
          ihdr = lscr(3)
          lhdr = lscr(4)
          lprint = lscr(5)
          fno = lscr(6)
          lno = lscr(7)
          nave = lscr(8)
          jhdr = lscr(9)
          IF( odelay .GT. otime ) THEN                                  /* make sure the output time is after the delay
              itemp = otime                                             /* truncate to an integer number of seconds
              odelay = itemp
          ENDIF
          DO 20 i = 1, ndeps
   20     depths(i) = 0.
      ENDIF
   50 lnum = lbuf(3)
      IF( lbuf(7) .NE. 0 ) lnum = lbuf(6)
      IF( lnum .EQ. llnum .AND. mlists .NE. 1 ) GOTO 1000
      llnum = lnum
   70 IF( lnum .GE. fno ) GOTO 100
      IF( mlists .EQ. 1 ) GOTO 500
      IF( lnum .LE. lno ) THEN
          first = .TRUE.
          GOTO 10
      ENDIF
      GOTO 500
  100 CONTINUE                                                          /*  THE CURRENT SHOT (RP) IS >= LNO
      IF(LNUM.LE.LNO) GO TO 500                                          /* USE THE PARAMETERS OF THIS LIST
      IF(MLISTS.LT.NLISTS) GO TO 110                                    /* ANY MORE USER PARAM LISTS ON DISC
      IF(ISIG.EQ.0) GO TO 1000                                          /* IS THERE A LIST IN MEMORY
      GO TO 500                                                          /* YES THE LAST LIST IS IN SCR
C****
C****   GET ANOTHER USER PARAMETER LIST FROM DISC
C****
  110 CONTINUE                                                          /* SET THE PRESENT LIST INTO OLD SO WE CAN GET A NEW ONE IN SCR
      CALL RDDISC(MUNIT,SCR,NPARS,ISTAT)
      ISIG=1
      fno = lscr(5)
      lno = lscr(6)
      MLISTS=MLISTS+1
      GO TO 70
C****
C****     SAVE THE CURRENT LIST IN CUR AND LEVS
C****
  500 CONTINUE
      IF(ISIG.EQ.0) GO TO 1000
      otime = scr(1)
      vel = scr(2) / 2.
      ihdr = lscr(3)
      lhdr = lscr(4)
      lprint = lscr(5)
      fno = lscr(6)
      lno = lscr(7)
      nave = lscr(8)
      jhdr = lscr(9)
c****
C****     NO SPATIAL VARIATION ALLOWED
C****
 1000 CONTINUE
      NSAMPS=IBUF(58)                                                    /* THE NUMBER OF DATA SAMPLES IN THE TRACE
      DELAY=BUF(46)                                                      /* THE FLOATING POINT DEEP WATER DELAY IN SECONDS
      SI=BUF(49)                                                         /* THE FLOATING POINT SAMPLE INTERVAL IN SECONDS
c****  the Sea Beam puts depth (in meters) into the header  It doesn't do
c****  this occasionally (at the beginning of a SeaBeam file)
      IF( ihdr .NE. 0 ) hdr = ibuf(ihdr)
      IF( lhdr .NE. 0 ) hdr = lbuf(lhdr)
      IF( jhdr .NE. 0 ) hdr = buf(jhdr)
      IF( hdr .NE. 0. ) THEN
          dlast = hdr
      ELSE
          hdr = dlast
      ENDIF
      IF( nave .GT. 1 ) THEN
          n = ndeps - 1
          DO 1050 i = 1, n
 1050     depths(ndeps-i+1) = depths(ndeps-i)
          depths(1) = hdr
          hdr = 0.
          divisr = 0.
          DO 1060 i = 1, nave
             IF( depths(i) .GT. 0. ) THEN
                 divisr = divisr + 1.
                 hdr = hdr + depths(i)
             ENDIF
 1060     CONTINUE
          IF( divisr .EQ. 0. ) RETURN
          hdr = hdr / divisr
      ENDIF
      IF( hdr .GT. 100. ) THEN
          IF( vel .EQ. 0. ) THEN
              PRINT *,' ***  ERROR  ***  Bad FLATEN VEL of ',vel
              STOP
          ENDIF
          nshift = ( hdr/vel - otime - ( delay - odelay )) / si           /* must be in units of depth
      ELSE
          nshift = ( hdr - otime - ( delay - odelay )) / si               /* must be in unit of time
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' nshift=',nshift,' otime=',otime,' hdr=',hdr,' vel=',
     *    vel,' si=',si
          PRINT *,' dlast=',dlast,' odelay=',odelay,' delay=',delay
      ENDIF
      IF( iuseap .EQ. 0 ) THEN                                           /* no ap and not in ap simulator?
          IF( nshift .GT. 0 ) THEN
              n = nsamps - nshift
              IF( in .EQ. 0 ) THEN
                  DO 1100 i = 1, n
 1100             buf(numhdr+i) = buf(numhdr+nshift+i)
                  DO 1110 i = 1, nshift
 1110             buf(numhdr+n+i) = 0.
              ELSE
                  DO 1120 i = 1, n
 1120             a(in+i-1) = a(in+nshift+i-2)
                  DO 1130 i = 1, nshift
 1130             a(in+n+i-1) = 0.
              ENDIF
          ELSE
              nshift = -nshift
              n = nsamps - nshift
              IF( in .EQ. 0 ) THEN
                  DO 1160 i = 1, n
 1160             buf(numhdr+nsamps-i+1) = buf(numhdr+nsamps-nshift-i+1)
                  DO 1170 i = 1, nshift
 1170             buf(numhdr+i) = 0.
              ELSE
                  DO 1180 i = 1, n
 1180             a(in+nsamps-i) = a(in+nsamps-nshift-i)
                  DO 1190 i = 1, nshift
 1190             a(in+i-1) = 0.
              ENDIF
          ENDIF
      ELSE
          IF( nshift .GT. 0 ) THEN
              n = nsamps - nshift
              CALL vmov(in+nshift,1,in,1,n)
              CALL vclr(in+n,1,nshift)
          ELSE
              nshift = -nshift
              n = nsamps - nshift
              CALL vmov(in+nsamps-nshift,-1,in+nsamps-1,-1,n)
              CALL vclr(in,1,nshift)
          ENDIF
      ENDIF
      IF( odelay .NE. delay ) THEN
          buf(46) = odelay
          ibuf(55) = odelay *1000.
      ENDIF
      RETURN
      END
