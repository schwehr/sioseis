      SUBROUTINE RDLINE
C     RDLINE READS A LINE OF INPUT FROM THE STANDARD INPUT DEVICE (READER OR
C  TERMINAL.  THE LINE IS STORED IN LABELED COMMON Q$LINE AS A CHARACTER STRING.
C  THE STRING IS CLEARED TO BLANKS PRIOR TO READ.
C
C
C  WRITTEN AND COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 29 FEBRUARY 1984
c  mod 3mar90 by pch to print 1 extra character because blank lines
c             the YMP to barf on print *, cbuf(1:0)
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod 11 Feb. 97 - Add luno - when positive, write the line image to luno
c  mod 4 Oct 00 - Use FORMAT(A200) rather tha FORMAT(A100)
c  mod 24 May 06 - Use frefil 4 (delete scratch files) before STOP

      PARAMETER (MAXC=3000)
      COMMON /sioln1/ cbuf
      COMMON /sioln2/ ichar, nchars, iprint, luno
      CHARACTER*3000 cbuf

      CBUF(1:maxc)=' '
      READ (*,20, END=900, ERR=910) CBUF(1:MAXC)
   20 FORMAT(A3000)
      nchars=0
      DO 30 i=1,maxc
         IF( cbuf(i:i) .NE. ' ') nchars=i
   30 CONTINUE
      cbuf(nchars+1:nchars+1)=' '
      cbuf(nchars+2:nchars+2)=char(0)
      IF( iprint .EQ. 1 ) PRINT *,cbuf(1:nchars+1)
      IF( luno .GT. 0 .AND. luno .LT. 200 ) 
     &    CALL wline( luno )
      ICHAR=1
      RETURN

  900 PRINT *,' ***  ERROR  ***  End detected while reading params.'
      CALL frefil( 4, itemp, istat )                                    ! close and delete tmp files
      STOP
  910 PRINT *,' ***  ERROR  ***  Error detected while reading params.'
      CALL frefil( 4, itemp, istat )                                    ! close and delete tmp files
      STOP
      END

