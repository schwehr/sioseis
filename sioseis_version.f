      SUBROUTINE version
      COMMON /ver/ver
      CHARACTER*80 ver
      INTEGER iarray(3)
      EQUIVALENCE (iarray(1),iday2), (iarray(2),imon2),
     &            (iarray(3),iyear2)
      DATA ver
     &/' SIOSEIS ver 2018.2.2 (26 July 2018)  (C) Regents of U.C.'/

      PRINT *,ver
c      CALL idate( iarray )
c     above is Sun,  below is SGI and VMS
c      CALL idate( imon2, iday2, iyear2 )
c      IF( iyear2 .GT. 1999 ) THEN
c      IF( imon2 .GT. 6 .OR. iyear2 .GT. 1998 ) THEN
c          PRINT *,' Your complimentary SIOSEIS license expired.'
C          STOP
c      ENDIF
      RETURN
      END
