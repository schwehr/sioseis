      SUBROUTINE GETOKE(CBUFO,NCHARS)
c  ****   Watch out - getoke writes a null after the last char,
c         so cbufo must be bigger than the number of characters
C
C   GETOKE RETURNS CONSECUTIVE TOKENS (ITEMS BETWEEN A DELIMITER), ONE PER CALL,
C   FROM CHARACTER STRING in COMMON/sioln1/. AN ALPHA STRING IS RETURNED IN
C   TOKEN WHEN IT STARTS AND ENDS WHEN SINGLE QUOTES.  (THE QUOTES ARE NOT
C   RETURNED.  THE STRING MUST BE TERMINATED WITH A QUOTE AND A BLANK, SO THAT
C   QUOTES MAY BE INCUDED IN THE STRING SO LONG AS THE QUOTE IS NOT FOLLOWED BY
C   A BLANK).
c       nchars will be a zero if end of line is detected or an comment character
c   is detected.  A comment character is either ! (VMS & Cray), { (Apollo), 
c   # (Unix). Any of theses comment characters will work on any machine!
C
C  AGUMENTS:
C    CBUFO  - THE CHARACTER*(*) STRING SET BY GETOKE CONTAINING THE NEXT TOKEN
C             FOUND.  CBUFO MUST BE NCHARS+1 CHARACTERS LONG (SINCE C STRINGS
C             MUST BE TERMINATED WITH A NULL).
C    NCHARS - THE NUMBER OF CHARACTERS IN THE TOKEN RETURNED IN CBUFO. A 0
C             (ZERO) NUMBER OF CHARACTERS MEANS THAT NO TOKEN WAS FOUND AND
C             THAT ANOTHER LINE SHOULD BE READ AND GETOKE CALLED AGAIN WITH
C             jchar=1.
C
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 29 FEBRUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod 25 Aug 98 - Make a NULL be a delimiter
c  mod 19 Jul 02 - Add entry getoke1 to go along with rline1, using different
c                  common blocks.
c  mod 2 May 04 - Add entry getokec for getoke with a comma delimiter.
c
      CHARACTER*1 CDELIM,QUOTE,tab, null
      CHARACTER*3000 CBUFIN
      COMMON /SIOLN1/ CBUFIN
      COMMON /sioln2/ jchar,NCBUF
      CHARACTER* (*) CBUFO
      DATA CDELIM/' '/, QUOTE/''''/

      cbufo = ' '
      NCHARS=0                                                          ! COUNT THE NON BLANK CHARACTERS IN THE TOKEN
      IQUOTE=0                                                          ! COUNT THE QUOTES IN THE TOKEN
      tab = CHAR(9)                                                     ! the tab character
      null = CHAR(0)                                                    ! NULL
      IF(jchar.LT.1) jchar=1
   10 CONTINUE
      IF( CBUFIN(jchar:jchar) .NE. CDELIM .AND. 
     *   cbufin(jchar:jchar) .NE. null .AND.
     *   cbufin(jchar:jchar) .NE. tab ) GOTO 20                         ! STRIP OFF LEADING BLANKS
      jchar=jchar+1
      IF(jchar.GT.NCBUF) RETURN
      GO TO 10
   20 IF(CBUFIN(jchar:jchar).NE.QUOTE) GO TO 30                         ! IS IT A QUOTE?
      ISTART=jchar+1                                                    ! STRIP OF THE LEADING QUOTE
      IQUOTE=1                                                          ! SIGNAL THAT THE STRING STARTED WITH A QUOTE
      GO TO 40
   30 CONTINUE
      IF( cbufin(jchar:jchar) .EQ. '!' .OR. 
     *    cbufin(jchar:jchar) .EQ. '{' .OR.
     *    cbufin(jchar:jchar) .EQ. '#' ) THEN
             nchars = 0
             GOTO 100
      ENDIF
c**** toss out non ASCII characters
      IF( ICHAR(cbufin(jchar:jchar)) .LT. 32 ) THEN
          jchar = jchar + 1
          GOTO 10
      ENDIF
      ISTART=jchar                                                      ! THE FIRST CHARACTER OF THE TOKEN TO BE RETURNED
   40 CONTINUE                                                          ! NOW FIND THE END OF THE TOKEN
      IF(jchar.GT.NCBUF) GO TO 110                                      ! ARE WE AT THE END OF THE BUFFER?
      IF(CBUFIN(jchar:jchar).EQ.CDELIM.AND.IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. tab .AND. IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. null .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A NULL?
      jchar=jchar+1
      IF( CBUFIN(jchar:jchar) .EQ. QUOTE .AND. IQUOTE .EQ. 1 ) THEN
          IQUOTE = 2
          GOTO 100
      ENDIF
      NCHARS=NCHARS+1                                                   ! THE CURREN CHARACTER IS NOT A BLANK OR A QUOTE
      GO TO 40                                                          ! GO LOOK AT THE NEXT CHARACTER
  100 CONTINUE
      IF( NCHARS .EQ. 0 ) RETURN                                        ! DON'T TRY TO MOVE ZERO CHARACTERS!!
      IF(IQUOTE.ne.1) GO TO 110                                         ! THE CURRENT CHARCTER IS A BLANK WITHIN QUOTES
      jchar=jchar+1
      nchars=nchars+1
      go to 40
c****  end the returned string with a null character so that c rcognizes
c**** the end of sting!!
  110 IF( nchars .GT. 0 ) CBUFO(1:NCHARS)=CBUFIN(ISTART:jchar)
      cbufo(nchars+1:nchars+1) = null
      jchar=jchar+1                                                     ! STRIP OFF THE BLANK
      RETURN
      END

      SUBROUTINE GETOKE1(CBUFO,NCHARS)
c**** Use rline1 if rline is being used by someone else.  This allows
c**** rdline and rline to coexist without rline using rdline's common.
c**** Use getoke1 to get the tokens from rline1 lines (common sioln3).
C
C   GETOKE RETURNS CONSECUTIVE TOKENS (ITEMS BETWEEN A DELIMITER), ONE PER CALL,
C   FROM CHARACTER STRING in COMMON/sioln1/. AN ALPHA STRING IS RETURNED IN
C   TOKEN WHEN IT STARTS AND ENDS WHEN SINGLE QUOTES.  (THE QUOTES ARE NOT
C   RETURNED.  THE STRING MUST BE TERMINATED WITH A QUOTE AND A BLANK, SO THAT
C   QUOTES MAY BE INCUDED IN THE STRING SO LONG AS THE QUOTE IS NOT FOLLOWED BY
C   A BLANK).
c       nchars will be a zero if end of line is detected or an comment character
c   is detected.  A comment character is either ! (VMS & Cray), { (Apollo), 
c   # (Unix). Any of theses comment characters will work on any machine!
C
C  AGUMENTS:
C    CBUFO  - THE CHARACTER*(*) STRING SET BY GETOKE CONTAINING THE NEXT TOKEN
C             FOUND.  CBUFO MUST BE NCHARS+1 CHARACTERS LONG (SINCE C STRINGS
C             MUST BE TERMINATED WITH A NULL).
C    NCHARS - THE NUMBER OF CHARACTERS IN THE TOKEN RETURNED IN CBUFO. A 0
C             (ZERO) NUMBER OF CHARACTERS MEANS THAT NO TOKEN WAS FOUND AND
C             THAT ANOTHER LINE SHOULD BE READ AND GETOKE CALLED AGAIN WITH
C             jchar=1.
C
C
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 29 FEBRUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod 25 Aug 98 - Make a NULL be a delimiter
c
      CHARACTER*1 CDELIM,QUOTE,tab, null
      CHARACTER*200 CBUFIN
      COMMON /SIOLN3/ CBUFIN
      COMMON /sioln4/ jchar,NCBUF
      CHARACTER* (*) CBUFO
      DATA CDELIM/' '/, QUOTE/''''/

      cbufo = ' '
      NCHARS=0                                                          ! COUNT THE NON BLANK CHARACTERS IN THE TOKEN
      IQUOTE=0                                                          ! COUNT THE QUOTES IN THE TOKEN
      tab = CHAR(9)                                                     ! the tab character
      null = CHAR(0)                                                    ! NULL
      IF(jchar.LT.1) jchar=1
   10 CONTINUE
      IF( CBUFIN(jchar:jchar) .NE. CDELIM .AND. 
     *   cbufin(jchar:jchar) .NE. null .AND.
     *   cbufin(jchar:jchar) .NE. tab ) GOTO 20                         ! STRIP OFF LEADING BLANKS
      jchar=jchar+1
      IF(jchar.GT.NCBUF) RETURN
      GO TO 10
   20 IF(CBUFIN(jchar:jchar).NE.QUOTE) GO TO 30                         ! IS IT A QUOTE?
      ISTART=jchar+1                                                    ! STRIP OF THE LEADING QUOTE
      IQUOTE=1                                                          ! SIGNAL THAT THE STRING STARTED WITH A QUOTE
      GO TO 40
   30 CONTINUE
      IF( cbufin(jchar:jchar) .EQ. '!' .OR. 
     *    cbufin(jchar:jchar) .EQ. '{' .OR.
     *    cbufin(jchar:jchar) .EQ. '#' ) THEN
             nchars = 0
             GOTO 100
      ENDIF
c**** toss out non ASCII characters
      IF( ICHAR(cbufin(jchar:jchar)) .LT. 32 ) THEN
          jchar = jchar + 1
          GOTO 10
      ENDIF
      ISTART=jchar                                                      ! THE FIRST CHARACTER OF THE TOKEN TO BE RETURNED
   40 CONTINUE                                                          ! NOW FIND THE END OF THE TOKEN
      IF(jchar.GT.NCBUF) GO TO 110                                      ! ARE WE AT THE END OF THE BUFFER?
      IF(CBUFIN(jchar:jchar).EQ.CDELIM.AND.IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. tab .AND. IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. null .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A NULL?
      jchar=jchar+1
      IF( CBUFIN(jchar:jchar) .EQ. QUOTE .AND. IQUOTE .EQ. 1 ) THEN
          IQUOTE = 2
          GOTO 100
      ENDIF
      NCHARS=NCHARS+1                                                   ! THE CURREN CHARACTER IS NOT A BLANK OR A QUOTE
      GO TO 40                                                          ! GO LOOK AT THE NEXT CHARACTER
  100 CONTINUE
      IF( NCHARS .EQ. 0 ) RETURN                                        ! DON'T TRY TO MOVE ZERO CHARACTERS!!
      IF(IQUOTE.ne.1) GO TO 110                                         ! THE CURRENT CHARCTER IS A BLANK WITHIN QUOTES
      jchar=jchar+1
      nchars=nchars+1
      go to 40
c****  end the returned string with a null character so that c rcognizes
c**** the end of sting!!
  110 IF( nchars .GT. 0 ) CBUFO(1:NCHARS)=CBUFIN(ISTART:jchar)
      cbufo(nchars+1:nchars+1) = null
      jchar=jchar+1                                                     ! STRIP OFF THE BLANK
      RETURN
      END


      SUBROUTINE GETOKEC(CBUFO,NCHARS)
c     This is the same as GETOKE, except COMMA is also a delimiter.
c   ( I don't know what will break if I just add it to geoke, so make
c   it a unique entry point).
c  mod 25 Aug 98 - Make a NULL be a delimiter
c  mod 2 May 98 - Make a COMMA be a delimiter
c               - Don't honor the comment convention.
c mod 22 July 2007 - Look out for ,, - vacuous field
c
      CHARACTER*1 CDELIM,QUOTE,tab, null, comma
      CHARACTER*200 CBUFIN
      COMMON /SIOLN1/ CBUFIN
      COMMON /sioln2/ jchar,NCBUF
      CHARACTER* (*) CBUFO
      DATA CDELIM/' '/, QUOTE/''''/, comma/','/

      cbufo = ' '
      NCHARS=0                                                          ! COUNT THE NON BLANK CHARACTERS IN THE TOKEN
c****   look out for vacuous field ( ,, )
      IF( cbufin(jchar:jchar) .EQ. comma ) GOTO 110
      IQUOTE=0                                                          ! COUNT THE QUOTES IN THE TOKEN
      tab = CHAR(9)                                                     ! the tab character
      null = CHAR(0)                                                    ! NULL
      IF(jchar.LT.1) jchar=1
   10 CONTINUE
      IF( CBUFIN(jchar:jchar) .NE. CDELIM .AND. 
     *   cbufin(jchar:jchar) .NE. null .AND.
     *   cbufin(jchar:jchar) .NE. comma .AND.
     *   cbufin(jchar:jchar) .NE. tab ) GOTO 20                         ! STRIP OFF LEADING BLANKS
      jchar=jchar+1
      IF(jchar.GT.NCBUF) RETURN
      GO TO 10
   20 IF(CBUFIN(jchar:jchar).NE.QUOTE) GO TO 30                         ! IS IT A QUOTE?
      ISTART=jchar+1                                                    ! STRIP OF THE LEADING QUOTE
      IQUOTE=1                                                          ! SIGNAL THAT THE STRING STARTED WITH A QUOTE
      GO TO 40
   30 CONTINUE
c**** toss out non ASCII characters
      IF( ICHAR(cbufin(jchar:jchar)) .LT. 32 ) THEN
          jchar = jchar + 1
          GOTO 10
      ENDIF
      ISTART=jchar                                                      ! THE FIRST CHARACTER OF THE TOKEN TO BE RETURNED
   40 CONTINUE                                                          ! NOW FIND THE END OF THE TOKEN
      IF(jchar.GT.NCBUF) GO TO 110                                      ! ARE WE AT THE END OF THE BUFFER?
      IF(CBUFIN(jchar:jchar).EQ.CDELIM.AND.IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. tab .AND. IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. null .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A NULL?
      IF(CBUFIN(jchar:jchar).EQ. comma .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A COMMA?
      jchar=jchar+1
      IF( CBUFIN(jchar:jchar) .EQ. QUOTE .AND. IQUOTE .EQ. 1 ) THEN
          IQUOTE = 2
          GOTO 100
      ENDIF
      NCHARS=NCHARS+1                                                   ! THE CURREN CHARACTER IS NOT A BLANK OR A QUOTE
      GO TO 40                                                          ! GO LOOK AT THE NEXT CHARACTER
  100 CONTINUE
      IF( NCHARS .EQ. 0 ) RETURN                                        ! DON'T TRY TO MOVE ZERO CHARACTERS!!
      IF(IQUOTE.ne.1) GO TO 110                                         ! THE CURRENT CHARCTER IS A BLANK WITHIN QUOTES
      jchar=jchar+1
      nchars=nchars+1
      go to 40
c****  end the returned string with a null character so that c rcognizes
c**** the end of sting!!
  110 IF( nchars .GT. 0 ) CBUFO(1:NCHARS)=CBUFIN(ISTART:jchar)
      cbufo(nchars+1:nchars+1) = null
      jchar=jchar+1                                                     ! STRIP OFF THE BLANK
      RETURN
      END
c
c
      SUBROUTINE GETOKE1C(CBUFO,NCHARS)
c     This is the same as GETOKE, except COMMA is also a delimiter.
c   ( I don't know what will break if I just add it to geoke, so make
c   it a unique entry point).
c  mod 25 Aug 98 - Make a NULL be a delimiter
c  mod 2 May 98 - Make a COMMA be a delimiter
c               - Don't honor the comment convention.
c mod 22 July 2007 - Look out for ,, - vacuous field
c
      CHARACTER*1 CDELIM,QUOTE,tab, null, comma
      CHARACTER*200 CBUFIN
      COMMON /SIOLN3/ CBUFIN
      COMMON /sioln4/ jchar,NCBUF
      CHARACTER* (*) CBUFO
      DATA CDELIM/' '/, QUOTE/''''/, comma/','/

      cbufo = ' '
      NCHARS=0                                                          ! COUNT THE NON BLANK CHARACTERS IN THE TOKEN
c****   look out for vacuous field ( ,, )
      IF( cbufin(jchar:jchar) .EQ. comma ) GOTO 110
      IQUOTE=0                                                          ! COUNT THE QUOTES IN THE TOKEN
      tab = CHAR(9)                                                     ! the tab character
      null = CHAR(0)                                                    ! NULL
      IF(jchar.LT.1) jchar=1
   10 CONTINUE
      IF( CBUFIN(jchar:jchar) .NE. CDELIM .AND. 
     *   cbufin(jchar:jchar) .NE. null .AND.
     *   cbufin(jchar:jchar) .NE. comma .AND.
     *   cbufin(jchar:jchar) .NE. tab ) GOTO 20                         ! STRIP OFF LEADING BLANKS
      jchar=jchar+1
      IF(jchar.GT.NCBUF) RETURN
      GO TO 10
   20 IF(CBUFIN(jchar:jchar).NE.QUOTE) GO TO 30                         ! IS IT A QUOTE?
      ISTART=jchar+1                                                    ! STRIP OF THE LEADING QUOTE
      IQUOTE=1                                                          ! SIGNAL THAT THE STRING STARTED WITH A QUOTE
      GO TO 40
   30 CONTINUE
c**** toss out non ASCII characters
      IF( ICHAR(cbufin(jchar:jchar)) .LT. 32 ) THEN
          jchar = jchar + 1
          GOTO 10
      ENDIF
      ISTART=jchar                                                      ! THE FIRST CHARACTER OF THE TOKEN TO BE RETURNED
   40 CONTINUE                                                          ! NOW FIND THE END OF THE TOKEN
      IF(jchar.GT.NCBUF) GO TO 110                                      ! ARE WE AT THE END OF THE BUFFER?
      IF(CBUFIN(jchar:jchar).EQ.CDELIM.AND.IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. tab .AND. IQUOTE.NE.1) GOTO 100        ! WAS IT A BLANK?
      IF(CBUFIN(jchar:jchar).EQ. null .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A NULL?
      IF(CBUFIN(jchar:jchar).EQ. comma .AND. IQUOTE.NE.1) GOTO 100       ! WAS IT A COMMA?
      jchar=jchar+1
      IF( CBUFIN(jchar:jchar) .EQ. QUOTE .AND. IQUOTE .EQ. 1 ) THEN
          IQUOTE = 2
          GOTO 100
      ENDIF
      NCHARS=NCHARS+1                                                   ! THE CURREN CHARACTER IS NOT A BLANK OR A QUOTE
      GO TO 40                                                          ! GO LOOK AT THE NEXT CHARACTER
  100 CONTINUE
      IF( NCHARS .EQ. 0 ) RETURN                                        ! DON'T TRY TO MOVE ZERO CHARACTERS!!
      IF(IQUOTE.ne.1) GO TO 110                                         ! THE CURRENT CHARCTER IS A BLANK WITHIN QUOTES
      jchar=jchar+1
      nchars=nchars+1
      go to 40
c****  end the returned string with a null character so that c rcognizes
c**** the end of sting!!
  110 IF( nchars .GT. 0 ) CBUFO(1:NCHARS)=CBUFIN(ISTART:jchar)
      cbufo(nchars+1:nchars+1) = null
      jchar=jchar+1                                                     ! STRIP OFF THE BLANK
      RETURN
      END
