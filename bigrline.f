      SUBROUTINE rline( lun )
C     RLINE READS A LINE OF INPUT FROM unit lun.
c  ********    The unit must have been opened by FORTRAN.   *********
C  THE LINE IS STORED IN LABELED COMMON SIOLN1 AS A CHARACTER STRING.
C  THE STRING IS CLEARED TO BLANKS PRIOR TO READ.
C
c  COMMON variable nchars is the column count of the last non
c  blank character on the line.  nchars 0 means it is a blank
c  line.  nchars < 1 means EOF was detected.
C
C  WRITTEN AND COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 29 FEBRUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod Feb 97 - add wline
c  mod 21 Dec 98 - Remove optional printing (conflict with rdline)
c  mod 19 Jul 02 - Add entry rline1 to use different common blocks.
c
      INTEGER maxc, lun
      PARAMETER (MAXC=3000)
      COMMON /SIOLN1/ CBUF
      COMMON /sioln2/ ICHAR, NCHARS, iprint, lunpo
      INTEGER ichar, nchars, iprint, i, lunpo
      CHARACTER*3000 CBUF

      nchars = -1
   10 CBUF(1:maxc)=' '
      READ (lun,20,END=100,ERR=100) CBUF(1:MAXC)
      nchars = 0
   20 FORMAT(A2000)
      DO 30 i=maxc,1,-1
         IF( cbuf(i:i) .NE. ' ') THEN
             nchars=i
             GOTO 40
         ENDIF
   30 CONTINUE
   40 cbuf(nchars+1:nchars+1)=' '
      cbuf(nchars+2:nchars+2)=char(0)
c***   watch out for DOS lines that have cr/lf - blank the cr
      IF( cbuf(nchars-1:nchars-1) .EQ. CHAR(10) ) 
     &    cbuf(nchars-1:nchars-1) = ' '
      IF( cbuf(nchars-1:nchars-1) .EQ. CHAR(13) ) 
     &    cbuf(nchars-1:nchars-1) = ' '
      IF( cbuf(nchars:nchars) .EQ. CHAR(10) ) 
     &    cbuf(nchars:nchars) = ' '
      IF( cbuf(nchars:nchars) .EQ. CHAR(13) ) 
     &    cbuf(nchars:nchars) = ' '
c      IF( iprint .EQ. 1 ) PRINT *,cbuf(1:nchars)
      ichar = 1
  100 RETURN

      ENTRY wline( lun )
c    Write nchars+2 characters (includes a blank and null)
c  in A format to Fortran unit lun
      WRITE( lun, '(A)' ) cbuf(1:nchars+2)
      RETURN
      END

      SUBROUTINE rline1( lun )
c**** Use rline1 if rline is being used by someone else.  This allows
c**** rdline and rline to coexist without rline using rdline's common.
c**** Use getoke1 to get the tokens from rline1 lines (common sioln3).
C     RLINE READS A LINE OF INPUT FROM unit lun.
c  ********    The unit must have been opened by FORTRAN.   *********
C  THE LINE IS STORED IN LABELED COMMON SIOLN1 AS A CHARACTER STRING.
C  THE STRING IS CLEARED TO BLANKS PRIOR TO READ.
C
c  COMMON variable nchars is the column count of the last non
c  blank character on the line.  nchars 0 means it is a blank
c  line.  nchars < 1 means EOF was detected.
C
C  WRITTEN AND COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 29 FEBRUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod Feb 97 - add wline
c  mod 21 Dec 98 - Remove optional printing (conflict with rdline)
c  mod Jul 02 - Add entry rline1
c
      INTEGER maxc, lun
      PARAMETER (MAXC=200)
      COMMON /SIOLN3/ CBUF
      COMMON /sioln4/ ICHAR4, NCHARS4, iprint4, lunpo4
      INTEGER ichar4, nchars4, iprint4, i, lunpo4
      CHARACTER*200 CBUF

      nchars4 = -1
   10 CBUF(1:maxc)=' '
      READ (lun,20,END=100,ERR=200) CBUF(1:MAXC)
      nchars4 = 0
   20 FORMAT(A200)
      DO 30 i=maxc,1,-1
         IF( cbuf(i:i) .NE. ' ') THEN
             nchars4=i
             GOTO 40
         ENDIF
   30 CONTINUE
   40 cbuf(nchars4+1:nchars4+1)=' '
      cbuf(nchars4+2:nchars4+2)=char(0)
c***   watch out for DOS lines that have cr/lf - blank the cr
      IF( cbuf(nchars4-1:nchars4-1) .EQ. CHAR(10) ) 
     &    cbuf(nchars4-1:nchars4-1) = ' '
      IF( cbuf(nchars4-1:nchars4-1) .EQ. CHAR(13) ) 
     &    cbuf(nchars4-1:nchars4-1) = ' '
      IF( cbuf(nchars4:nchars4) .EQ. CHAR(10) ) 
     &    cbuf(nchars4:nchars4) = ' '
      IF( cbuf(nchars4:nchars4) .EQ. CHAR(13) ) 
     &    cbuf(nchars4:nchars4) = ' '
      IF( iprint4 .EQ. 1 ) PRINT *,cbuf(1:nchars4)
      ichar4 = 1
      RETURN
  100 CONTINUE
c  100 PRINT *,' END'
      RETURN
  200 PRINT *,' rline err'
      RETURN
      END
