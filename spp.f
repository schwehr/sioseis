      SUBROUTINE spp( c, ibuf )
c     SPP simulates the Versatec Simultaneous Print/Plot mode by ORing a
c  character arry into a set of raster lines.
c     The way this program works is that it has only numeric fonts.  When
c  SPP gets a non blank annotation (c), it moves 16 lines of digitized
c  font into a storage buffer.  Each call to SPP then ORs the top line of
c  the storage buffer into the users ibuf array.  After the OR, the storage
c  array is moved up one raster so that the next call to SPP gets in essence
c  the next line of the character.  Thus, it takes 16 calls to SPP to flush
c  a whole character.  A new non blank character in c before the 16th call
c  will reset the SPP OR array.
c     This version of SPP assumes c is 8 characters big.
c
c  Copyright (C) 1987 by Paul Henkart, Seismic Reflection Processors, San Diego
c  ALL RIGHTS RESERVED
c
c   mod 8 Aug 1991 by pch - add minus
c mod 22 May 92 - add color
c mod 8 feb 93 - add rgb (rgb=0 means CYM, where white and black are
c                opposite to RGB.  ie in CYM white is no planes and
c                black is all planes - sorta like a B&W plot.
c mod 10 Jul 02 - Add reverse direction annotation by using kfonts and lfonts,
c                which is upsidedown and backwards.
c mod 29 Jan 03 - nibs 75 annotation on PC was bad
c mod 13 Jan 04 - Change common defs and colors from 8 to 9
c mod 10 Oct 06 - Change ltr font (kfonts) for 1 and 3
c mod 20 Aug 08 - Added i255 for g95
c
      INTEGER*2 ibuf(1),itemp,itemp1
      INTEGER*2 ifonts(160), jfonts(80), minus(16), kfonts(160),
     *          lfonts(80)
      CHARACTER*8 c, cc
      COMMON /versat/ nibs, rnibs
      COMMON /edits/ ierror, iwarn, irun, now, icompt
      INTEGER*2 RSHIFT, LSHIFT, i255
      INTEGER*2 iorbuf(8,16),jorbuf(8,8)                                ! this holds 8 rasterized characters
      COMMON /colors/ defs(9), colors(9), ncolors, bcolor, rgb
      INTEGER colors, bcolor, rgb
      COMMON /PLOT/munit,nlists,ftag,taginc,fspace,nspace,spacei,idir,
     &    irecsp, lunhead, nraster
      LOGICAL first
      SAVE
      DATA i255/255/
      DATA iorbuf/128*0/, jorbuf/64*0/, first/.TRUE./
      DATA minus/8*0,1023,7*0/
      DATA ifonts/ 0,1008,1032,10*2052,1032,1008,0,                     ! zero
     1 0,32,96,160,288,544,32,32,32,32,32,32,32,32,2044,0,              ! one
     2 0,4064,16,4*8,16,32,64,128,256,512,1024,4092,0,                  ! two
     3 0,4080,8,8,8,8,8,1008,8,8,8,8,8,8,4080,0,                        ! three
     4 0,32,64,128,256,512,1024,2080,4128,8176,5*32,0,                  ! four
     5 0,4092,5*2048,4080,8,4*4,8,4080,0,                               ! five
     6 0,508,512,1024,4*2048,4092,5*2052,4092,0,                        ! six
     7 0,4092,4,8,16,32,64,128,256,256,512,512,2*1024,2048,0,           ! seven
     8 0,2040,5*2052,2040,6*2052,2040,0,                                ! eight
     9 0,1008,1032,3*2052,1036,1012,7*4,0 /                             ! nine
      DATA jfonts/28,34,34,34,34,34,28,0,                               ! zero
     1 8,24,40,8,8,8,24,0,                                              ! one
     2 28,2,2,4,8,16,62,0,                                              ! two
     3 60,2,2,28,2,2,60,0,                                              ! three
     4 4,12,20,36,126,4,4,0,                                            ! four
     5 62,32,32,60,2,2,60,0,                                            ! five
     6 28,32,32,62,34,34,62,0,                                          ! six
     7 62,2,4,8,16,32,32,0,                                             ! seven
     8 28,34,34,28,34,34,28,0,                                          ! eight
     9 28,34,34,28,2,2,2,0/                                             ! nine
      DATA kfonts/0,4032,4128,10*8208,4128,4032,0,                      ! zero
c     1 0,16352,8*1024,64,1152,1280,1536,1024,0,                         ! one
     1 0,16352,8*1024,1088,1152,1280,1536,1024,0,                         ! one
     2 0,16368,32,64,128,256,512,1024,2048,4*4096,2048,2032,0,          ! two
c     3 0,4080,6*4096,-16448,5*4096,4080,0,                              ! three
     3 0,4080,6*4096,4032,5*4096,4080,0,                                ! three
     4 0,5*1024,4088,1032,1040,32,64,128,256,512,1024,0,                ! four
     5 0,4080,4096,4*8192,4096,4080,5*16,16368,0,                       ! five
     6 0,16368,5*8208,16368,4*16,32,64,16256,0,                         ! six
     7 0,16,2*32,2*64,128,128,256,512,1024,2048,4096,8192,16368,0,      ! seven
     8 0,16368,6*8208,8160,5*8208,12256,0,                              ! eight
     9 0,7*4096,12224,12320,3*8208,4128,4032,0/
      DATA lfonts/0,14336,5*17408,14336,
     1 0,6144,3*4096, 5120, 6144, 4096,
     2 0, 31744, 2048, 4096, 8192, 2*16384, 14336,
     3 0, 15360, 2*16384, 14336, 2*16384, 15360,
     4 0, 2*8192, 32256, 9216, 6144, 12288, 8192,
     5 0, 15360, 2*16384, 15360, 2*1024, 31744,
     6 0, 31744, 2*17408, 31744, 2*1024, 14336,
     7 0, 2*1024, 2048, 4096, 8192, 16384, 31744,
     8 0, 14336, 2*17408, 14336, 2*17408, 14336,
     9 0, 3*16384, 14336, 2*17408, 14336/
       IF( first ) THEN
          first = .FALSE.
          IF( idir .LT. 0 ) THEN
              DO i = 1, 160
                 ifonts(i) = kfonts(i)
              ENDDO
              DO i = 1, 80
                 jfonts(i) = lfonts(i)
              ENDDO
          ENDIF
          IF( icompt .EQ. 2 .OR. icompt .EQ. 4 ) THEN
              CALL swap16( ifonts, 160 )
              CALL swap16( jfonts, 80 )
              CALL swap16( minus, 16 )
          ENDIF
          IF( ncolors .NE. 0 .AND. rgb .EQ. 1) THEN
              DO 10 i = 1, 160
   10         ifonts(i) = NOT( ifonts(i) )
              DO 11 i = 1, 80
   11         jfonts(i) = NOT( jfonts(i) )
              DO 13 i = 1, 8
                 DO 12 j = 1, 8
                    jorbuf(i,j) = -1
   12            CONTINUE
   13         CONTINUE
              DO 20 i = 1, 16
                 DO 15 j = 1, 8
   15            iorbuf(j,i) = -1
   20         minus(i) = NOT( minus(i) )
          ENDIF
      ENDIF
      cc(1:8) = c(1:8)
      IF( cc(1:8) .NE. ' ') THEN
          IF( idir .LT. 0 ) THEN
              DO i = 1, 8
                 cc(i:i) = c(9-i:9-i)
              ENDDO
          ENDIF
          DO 190 i=1,8                                                  ! do each character
              IF( cc(i:i) .NE. ' ') THEN
c                                   ICHAR('0') is 48 (decimal or 30 hex)
                  j = (ICHAR(cc(i:i))-48)*16                             ! get the index of the char
                  IF( j .LT. 0 .OR. j .GT. 144 ) GOTO 120
                  IF( rnibs .GT. 100. ) THEN
                      DO 110, k=1,16                                    ! store each character raster line 
                         iorbuf(i,k) = ifonts(j+k)
  110                 CONTINUE
                  ELSE
                      j = j / 2
                      DO 115, k=1,8                                     ! store each character raster line 
                         jorbuf(i,k) = jfonts(j+k)
  115                CONTINUE
                  ENDIF
               ENDIF
  120          CONTINUE
               IF( cc(i:i) .EQ. '-' ) THEN
                   IF( rnibs .GT. 100. ) THEN
                       DO 140 k = 1, 16
                          iorbuf(i,k) = minus(k)
  140                  CONTINUE
                   ELSE
                       DO 150 k = 1, 8
                          jorbuf(i,k) = minus(k*2-1)
  150                  CONTINUE
                   ENDIF
               ENDIF
  190     CONTINUE
      ENDIF
      IF( rnibs .GT. 100. ) THEN
          DO 200 i=1,8
c             ibuf(i) = IOR( ibuf(i), iorbuf(i,1) )                       ! only 1 raster line!
             ibuf(i) = iorbuf(i,1)
  200     CONTINUE
      ELSE
          j = 1
          DO 300 i = 1, 4
             IF( idir .GE. 0 ) THEN
                 IF( icompt .NE. 2 .AND. icompt .NE. 4 ) THEN
                     itemp = LSHIFT( jorbuf(j,1), 8 )
                     j = j + 1
                     itemp1 = IOR( itemp, IAND(jorbuf(j,1),i255) )
                 ELSE
                     itemp = RSHIFT(jorbuf(j,1),8)
                     j = j + 1
                     itemp1 = IOR( itemp, jorbuf(j,1))
                 ENDIF
             ELSE
                 IF( icompt .NE. 2 .AND. icompt .NE. 4 ) THEN
                     itemp = jorbuf(j,1)
                     j = j + 1
                     itemp1 = IOR( itemp, RSHIFT(jorbuf(j,1),8) )
                 ELSE
                     itemp = jorbuf(j,1)
                     j = j + 1
                     itemp1 = IOR( itemp, LSHIFT(jorbuf(j,1),8))
                 ENDIF
             ENDIF
c             ibuf(i) = IOR( ibuf(i), itemp1 ) 
             ibuf(i) = itemp1
             j = j + 1
  300     CONTINUE
      ENDIF
  
      IF( rnibs .GT. 100. ) THEN
          DO 320 j=1,15                                                 ! move the or buffer up 1 raster line
             DO 310 i=1,8                                               ! do each character
                 iorbuf(i,j) = iorbuf(i,j+1)
  310        CONTINUE
  320     CONTINUE
      ELSE
          DO 340 j = 1, 7
             DO 330 i = 1, 8
                jorbuf(i,j) = jorbuf(i,j+1)
  330        CONTINUE
  340     CONTINUE
      ENDIF

      DO 400 i=1,8                                                      ! zero out the next rastered annotation
          IF( ncolors .NE. 0 .AND. rgb .EQ. 1 ) THEN
              iorbuf(i,16) = -1
              jorbuf(i,8) = -1
          ELSE
              iorbuf(i,16)=0
              jorbuf(i,8)=0
          ENDIF
  400 CONTINUE
    
      RETURN
      END
