      SUBROUTINE LCODE(ALPHA,NCHARS, longint, ISTAT )
C     LCODE RETURNS A long integer GIVEN A STRING OF CHARACTERS.  THIS RESEMBLES
C  THE OLD DECODE STATEMENT FOUND IN PRE-FORTRAN 77.  LCODE doesn't
c  handle more than 7 digits correctly.
C  THIS ROUTINE DOES
C  NOT BOMB IF THE ALPHA IS NOT A NUMBER!  THE MESSAGE IS IN ENGLISH TOO!
C
C  ARGUMENTS:
C    ALPHA - THE STRING OF CHARACTERS TO BE CONVERTED TO AN INTERNAL FLOATING
C            POINT NUMBER.  THIS MUST BE OF TYPE CHARACTER.   CHARACTER* (*)
C    NCHARS - THE NUMBER OF CHARACTERS IN THE STRING TO BE DECODED.  INTEGER*4
C     - THE INTEGER*4 NUMBER DECODED BY LCODE.  THIS VALUE
C            IS RETURNED BY LCODE.   INTEGER*4
C    ISTAT - THE STATUS OF THE DECODE.    INTEGER*4
C          =0, THE DECODE HAD AN ERROR.  THE RETURN VALUE AREAL IS MEANINGLESS.
C              THE "NUMBER" HAD A NON-NUMERIC IN IT.
C          =1, THE CHARACTER STRING WAS AN ALPHA (THE FIRST CHARACTER WAS NOT
C              NUMERIC).  THE RETURNED VALUE OF AREAL IS MEANINGLESS.
C          =2, THE DECODE WAS SUCCESSFUL.
C
C
C  WRITTEN AND COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 14 JULY 1983
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c  modified July 1989 by pch to not modify nchars!!!
c  mod Dec 99 for g77 by ah - The format in the read must be I20 rather than I
C
      CHARACTER*(*) ALPHA
      CHARACTER*20 CTEMP
      INTEGER*4 longint
C
      longint = 0
      ISTART=0
      JSTAT=0
      nchar = nchars
      DO 100 I=1,NCHAR
      IF(ALPHA(I:I).GE.'0'.AND.ALPHA(I:I).LE.'9') GO TO 50              ! ASSUME ASCII!!
      IF(ALPHA(I:I).EQ.'.') GO TO 100                                   ! ALLOW A DECIMAL POINT
      IF(ALPHA(I:I).EQ.'-'.OR.ALPHA(I:I).EQ.'+') GO TO 100              ! ALLOW SIGNED VALUES

      IF(ALPHA(I:I).EQ.'E'.OR.ALPHA(I:I).EQ.'e') GO TO 100              ! ALLOW EXPONENTIALS

      ISTAT=1                                                           ! PRESET TO AN ERROR

      GO TO 200
C
   50 ISTAT=2                                                            ! THE CHARACTER IS A NUMERIC
      IF(I.EQ.1.OR.ISTAT.EQ.JSTAT.AND.ISTART.EQ.0) GO TO 90
      IF(JSTAT.NE.0) GO TO 200
   90 JSTAT=ISTAT                                                       ! SAVE THE TYPE OF STRING TO COMPARE THE NEXT CHARACTER WITH
  100 CONTINUE
C
C   FINISHED SEARCH FOR ERRORS, NOW DECODE THE THING
C
      IF(ISTAT.EQ.1) RETURN                                              ! DON'T DECODE AN ALPHA STRING
      CTEMP(1:20)=' '
      CTEMP(1:NCHAR)=ALPHA(1:NCHAR)
c**** the following gyration is because VMS insists that there be a period
c**** when using an F or G format!
      DO 150 i = 1, nchar
         IF( ctemp(i:i) .EQ. '.' ) THEN
            nchar = nchar - 1
            GOTO 160
         ENDIF
  150 CONTINUE
      nchar = nchar + 1
  160 CONTINUE
      IF( nchar .GT. 20 ) THEN
          PRINT *,' ***  WARNING  ***  The number ',alpha(1:nchar),
     *    ' exceeds the maximum field width of 20.'
      ENDIF
      READ(CTEMP,'(I20)',ERR=200) longint
      RETURN
C****
C****   PRINT AN ERROR MESSAGE
C****
  200 PRINT 210, ALPHA(1:NCHAR)
  210 FORMAT('  ***  ERROR  ***  THE STRING ',A10,' IS NOT A NUMBER.')
      ISTAT=0
      RETURN
      END

