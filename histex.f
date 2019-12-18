      SUBROUTINE histex( cscr, istop )
C
C  COPYRIGHTED (c) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, February 1997
c
C
      COMMON /history/ lunhist, iall, lunappend
      COMMON /ver/ver
      CHARACTER*80 ver
      COMMON /sioln1/ cbuf
      COMMON /sioln2/ idum1, ncinline, iprint, lunpo
      CHARACTER*200 cbuf
      COMMON /pnames/ pnames(100)                                       ! holds the process names in the order of PROCS
      CHARACTER*7 pnames
      COMMON /porder/ numprocs
      LOGICAL FIRST
      CHARACTER*80 line
      SAVE
      DATA FIRST /.TRUE./
 
      IF( first ) THEN
          first = .FALSE.
          iprint = 0                                                   ! turnoff printing in rline
          jobnum = 0
          REWIND lunhist
  100     CALL rline( lunhist )
          IF( cbuf(1:4) .EQ. 'Job ' ) READ( cbuf, '(4x,I6)' ) jobnum
          IF( ncinline .GE. 0 ) GOTO 100
  200     jobnum = jobnum + 1
          cbuf = ' '
          CALL getdate1( line )
          WRITE( cbuf, '(4HJob ,I6,12H started on ,A24,1H,,A32)' ) 
     &      jobnum, line(1:24), ver(1:32)
          ncinline = 46 + 33
          cbuf(ncinline+1:ncinline+1) = ' '
          cbuf(ncinline+2:ncinline+2) = CHAR(0)
          CALL wline( lunhist )
          IF( iall .NE. 0 ) THEN
              REWIND lunpo
  210         CALL rline( lunpo )
              IF( ncinline .GE. 0 ) THEN
                  CALL wline( lunhist )
                  GOTO 210
              ENDIF
          ELSE
              WRITE( cbuf, '(6HPROCS ,20(A7,1X))' )
     &            ( pnames(i),i=1,numprocs)
              ncinline = numprocs * 8 + 6
              cbuf(ncinline+1:ncinline+1) = ' '
              cbuf(ncinline+2:ncinline+2) = CHAR(0)
              CALL wline( lunhist )
          ENDIF
          IF( lunappend .NE. 0 ) THEN
              REWIND lunappend
  300         CALL rline( lunappend )
              IF( ncinline .GE. 0 ) THEN
                  CALL wline( lunhist )
                  GOTO 300
              ENDIF
              CLOSE( UNIT=lunappend )
          ENDIF
      ENDIF
      IF( istop .NE. 0 ) THEN
          CALL getdate1( line )
          WRITE( cbuf, '(4HJob ,I6,13H finished on ,A24)' ) 
     &      jobnum, line(1:24)
          ncinline = 47
          cbuf(ncinline+1:ncinline+1) = ' '
          cbuf(ncinline+2:ncinline+2) = CHAR(0)
          CALL wline( lunhist )
          ncinline = 1
          cbuf(1:2) = ' '
          cbuf(3:3) = CHAR(0)
          CALL wline( lunhist )
      ENDIF
      RETURN
      END
