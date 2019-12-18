      SUBROUTINE reltap( lun, itype )
c    A stupid routine to release a tape drive using the various
c  idiosyncracies of sioseis.  I.E. This writes 2 EOFs if it's an
c  output tape, because process output doesn't know when the last shot,
c  so output doesn't write the eofs (which it used to do, and then back
c  over them so the next write writes over the eof, but that's no
c  longer done for a variety of reasons).
c
c  lun is the tape drive number, if there is one.
c  itype - the way drive was used.
c        = 1, input
c        = 2, output
c  do I have to sign this thing?
c
c  mod ? sept 96 - Only 1 EOF and don't rewind output tapes if rewindo is 0
c  mod 6 Oct 97 - Don't rewind input tape if rewindi is 0
c  mod March 99 - Freetp (Unix close) doesn't work in mag*.c for some
c               reason and the next astape gets another file descriptor.
c               On long jobs, with more than 50 tape changes, we'd then
c               exceed the number of open files (50). F!@#$%^&*(.
c               If the unit number is the same as before, magtap doesn't
c               ask for a new descriptor and uses the old one.c   mod 25 Sept 00 - The SGI (hatteras) won't allow a manual eject
c       of the tape, so let's close it.  try again!

      COMMON /outdev/outdevice
      CHARACTER*80 outdevice, inputdev
      COMMON /inputdev/ inputdev
      COMMON /readt/ idum(20), irewindi
      COMMON /WRITET/OUNIT,NSAMPS,OREEL,POSAFT,OFMT,NTRCS,LFOR,ONUMTR,
     &       nfskipo, rewindo
      INTEGER*4 OFMT,OUNIT,POSAFT,ONUMTR,OREEL, rewindo

c
      IF( itype .EQ. 1 ) THEN
          IF( irewindi .EQ. 1 ) CALL magtap( lun, idum, 0, 10,istat)    ! rewind
          IF( inputdev .NE. ' ' ) THEN
              CALL freetp( lun, inputdev )
          ELSE
              CALL untape( lun )
          ENDIF
      ENDIF
      IF( itype .EQ. 2 ) THEN
          CALL magtap( lun, idum, 0, 32, istat )                        ! write an EOF
          IF( rewindo .EQ. 0 ) THEN
              CALL magtap( lun, idum, 0, 32, istat )                    ! write an EOF
c              CALL magtap( lun, idum, 0, 10, istat )                    ! rewind
          ENDIF
          IF( outdevice .NE. ' ' ) THEN
              CALL freetp( lun, outdevice )
          ELSE
              CALL untape( lun )
          ENDIF
      ENDIF
      RETURN
      END
