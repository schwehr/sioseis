      SUBROUTINE NMOAPP(DATIN,DATOUT,ITX,IS,IE,ISTRET,MUTE,itype)
C     NMOAPP APPLIES THE NMO CORRECTIONS COMPUTED BY NMOTAB AND TRANSFERRED VIA
C  ARRAY ITX.  THE CORRECTIONS ARE APPLIED TO ARRAY DATIN AND ARE WRITTEN TO
C  ARRAY DATOUT.  THE CORRECTIONS IN ARRAY ITX ARE REALLY JUST INDICES
C  CORRESPONDING TO TX.  THE T0'S START WITH INDEX IS AND INCREMENT BY 1 UNTIL
C  INDEX IE IS REACHED.
C     MUTING IS PERFORMED ON ALL DATA WITH NMO STRETCH EXCEEDING ISTRET.
C THE INDEX OF THE FIRST UNMUTED VALUE IS RETURNED BY NMOAPP VIA MUTE.
C
C ARGUMENTS:
C     DATIN  - AN ARRAY OF INPUT DATA TO BE MOVED OUT.
C     DATOUT - AN ARRAY TO RECEIVE THE OUTPUT OR MOVED OUT DATA.
C              THE OUTPUT ARRAY MAY BE THE SAME AS THE INPUT ARRAY SINCE T0 IS
C              ALWAYS LESS THAN TX.
C     ITX    - THE INTEGER ARRAY OF TX INDEXES THAT POINT TO THE DATA TO BE MOVED
C              OUT
C     IS     - THE INTEGER START INDEX.  THE FIRST T0 VALUE (IN BLOCKS).  THE
C              FIRST VALUE OF THE ITX ARRAY.
C     IE     - THE INTEGER END INDEX.  THE LAST T0 VALUE (IN BLOCKS) TO MOVE.
C     ISTRET - THE STRETCH OR MAXIMUM AMOUNT OF NMO (IN BLOCKS) ALLOWED WITHOUT
C              KILLING THE DATA.  ANY DATA WHOSE NMO IS GREATER THAN ISTRET WILL
C              BE KILLED.
C     MUTE   - THE NEW OR UPDATED IST BECAUSE OF MUTING DUE TO NMO EXCEEDING THE
C              STRETCH.  SET BY NMOAPP.
c     itype  = 1, NMO or MoveOut
c            = 2, MoveIn
C
C     PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1979
C
c  modified by pch aug 89 to kill data before the start index
c  mod Oct 91 by pch - add itype so that movein works (data before the current is moved)
c  mod Nov 91 by pch - Zero data before the start when MoveIn
c  mod Feb 99 - Zero the sample if it comes from a time that has already
c               been past.  i.e. inverted nmo?
c  mod 27 Jul 99 - Zero it only if time is decreasing from the previous
c               sample.  Once it starts increasing again, it's ok maybe.
c               This just kills the part when the waveform becomes
c               reversed in time.
c  mod 9 May 12 - kbig was set wrong and may explain "NMO caused wavelet inversion"
c  mod 5 Jul 12 - nmoex sets the index to 99999 if sample killed by DSTRETCH
c  mod 12 Jul 12 - The first kbig was set to itx(is-1) or itx(0)
c                - Drop the wavelet inversion BS because datin is now different from datout.
c                - kbig is no longer needed.
c
      DIMENSION DATIN(1),DATOUT(1),ITX(1)
      DATA ierr/0/
      SAVE ierr
c
      MUTE = 0
      ISTR = ISTRET
      I = IS - 1
c      kbig = itx(is)
      M = 0
      IF( itype .EQ. 2 ) datin(1) = 0.
   10 I = I + 1
      IF( I .LE. IE ) THEN
          K=ITX(I)                                                      !  PICK UP THE INDEX OF DATAIN
          IF( K .LT. I .AND. itype .NE. 2 ) K = I                       !  DON'T ALLOW INVERTED NMO
          IF( IABS(K-I) .LE. ISTR .OR. itype .NE. 2 ) THEN
              DATOUT(I)=DATIN(K)                                        !  MOVE 1 SAMPLE
              IF( IABS(K-I) .GT. istr .AND. itype .EQ. 1 ) datout(i)=0.
              IF( K .LT. IS ) DATOUT(I) = 0.                            ! KILL THE DATA BEFORE THE START OF DATA
              IF( K .GT. IE ) DATOUT(I) = 0.                            ! KILL THE DATA AFTER THE END OF DATA
c              IF( k .EQ. itx(i-1) ) datout(i) = datout(i-1)             ! watch out for the last sample zeroed by us
c              IF( k .LT. kbig ) THEN
c                  datout(i) = 0.
c                  IF( ierr .EQ. 0 ) ierr = 1
c              ELSE
c                  kbig = k
c              ENDIF
c              kbig = k
          ELSE
              M = I
          ENDIF
          GO TO 10
      ENDIF
      IF( ierr .EQ. 1 ) THEN
          ierr = 2
          PRINT *,
     &' ***  WARNING  ***  Data zeroed - NMO caused wavelet inversion.'
      ENDIF
C****
C****   KILL THE DATA IF THE NMO EXCEEDS THE STRETCH FACTOR
C****
      IF( M .EQ. 0 ) RETURN
      DO 400 K = 1, M
         DATOUT(K) = 0.
  400 CONTINUE
      MUTE = M + 1
      RETURN
      END
