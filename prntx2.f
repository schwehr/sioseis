      SUBROUTINE PRNTX2(IN,IOUT,NIN,NOUT)
C      PRNTX CONVERTS A STRING OF RASTERS (A CONTINUOUS STRING OF BITS, EITHER
C  ON OR OFF, EACH BIT REPRESENTING A DOT) TO A PRINTRONIX PLOT MODE
C  CHARACTER STRING.  THE PRINTRONIX EXPECTS an UPPER BIT ON to indicate
C  THE PLOT MODE).  THIS ROUTINE THUS TAKES 6 BIT BYTES FROM THE INPUT,
C  CONVERTS IT TO AN 8 BIT BYTE, AND PUTS ON THE appropriate HIGH ORDER BIT.
c  PRNTX1 does not swap the bit order as PRNTX2 does. 
c     Use INTEGER*2 rather than CHARACTER*1 because ANSII standard character
c handling may be different on different machines.
C
C  ARGUMENTS:
C  IN    - INPUT RASTER STRING TO BE CONVERTED.
C  IOUT  - THE OUTPUT CHARCTER STRING.  IOUT MUST BE DIFFERENT FROM IN. IOUT
C          WILL BE LONGER THAN IN!!!
C  NIN   - THE NUMBER OF 16 BIT INTEGERS IS IN.  NIN*2 BYTES WILL BE CONVERTED.
C  NOUT  - THE NUMBER OF INTEGER*2 WORDS RETURNED IN IOUT.  NOUT*2 BYTES WILL
C          BE IN IOUT.  ***  NOTE  *** THIS IS A RETURN ARGUMENT!!
C
C  COPYRIGHT (C) BY:
C  PAUL HENKART, SEISMIC REFLECTION PROCESSORS, SAN DIEGO, 13 August 1987 
C  ALL RIGHTS RESERVED.
c mod 14 Aug 07 - g95 IAND requires arguments to be same type and kind.
C
      INTEGER*2 RSHIFT, LSHIFT
      INTEGER*4 NIN,NOUT
      INTEGER*2 IN(1),IOUT(1)
      INTEGER*2 itemp,jtemp, itemp1, itemp2, i63, i60, i3, i48, i15, i10
      DATA i63/63/, i60/60/, i3/3/, i48/48/, i15/15/, i10/10/
      INTEGER*2 iflip(64), idec(64)
C      DATA IFLIP /:100,:140,:120,:160,:110,:150,:130,:170,
C     1            :104,:144,:124,:164,:114,:154,:134,:174,
C     2            :102,:142,:122,:162,:112,:152,:132,:172,
C     3            :106,:146,:126,:166,:116,:156,:136,:176,
C     4            :101,:141,:121,:161,:111,:151,:131,:171,
C     5            :105,:145,:125,:165,:115,:155,:135,:175,
C     6            :103,:143,:123,:163,:113,:153,:133,:173,
C     7            :107,:147,:127,:167,:117,:157,:137,:177/
      DATA IFLIP /64,96,80,112,72,104,88,120,
     *            68,100,84,116,76,108,92,124,
     *            66,98,82,114,74,106,90,122,
     *            70,102,86,118,78,110,94,126,
     *            65,97,81,113,73,105,89,121,
     *            69,101,85,117,77,109,93,125,
     *            67,99,83,115,75,107,91,123,
     *            71,103,87,119,79,111,95,127/
      DATA IDEC/64,65,66,67,68,69,70,71,
     *          72,73,74,75,76,77,78,79,
     *          80,81,82,83,84,85,86,87,
     *          88,89,90,91,92,93,94,95,
     *          96,97,98,99,100,101,102,103,
     *          104,105,106,107,108,109,110,111,
     *          112,113,114,115,116,117,118,119,
     *          120,121,122,123,124,125,126,127/


      
      i = 1
c  start the string with a 5 - put the printronix in plot mode
      itemp = 5
      j = 1
   10 CONTINUE
      jtemp = iflip( IAND( rshift(in(i),10), i63) + 1 )   ! 63=hex(3f)
      iout(j) = IOR( lshift(itemp,8), jtemp )
      j = j + 1
      itemp = iflip( IAND( rshift(in(i),4), i63 ) + 1 )
      itemp1 = IAND( lshift(in(i),2), i60)                ! 003c
      i = i + 1
      itemp2 = IAND( rshift(in(i),14), i3)
      jtemp = iflip( IOR(itemp1,itemp2) + 1 )
      iout(j) = IOR( lshift(itemp,8), jtemp )
      j = j + 1
      itemp = iflip( IAND( rshift(in(i),8), i63) + 1 )
      jtemp = iflip( IAND( rshift(in(i),2), i63) + 1 )
      iout(j) = IOR( lshift(itemp,8), jtemp )
      j = j + 1
      itemp1 = IAND( lshift(in(i),4), i48)
      i = i + 1
      itemp2 = IAND( rshift(in(i),12), i15)
      itemp = iflip( IOR(itemp1,itemp2) + 1 )
      jtemp = iflip( IAND( rshift(in(i),6), i63) + 1 )
      iout(j) = IOR( lshift(itemp,8), jtemp )
      j = j + 1
      itemp = iflip( IAND(in(i),i63) + 1 )
      i = i + 1
      IF( i .LT. nin ) GO TO 10
c****
      iout(j) = IOR( lshift(itemp,8), i10 )
      nout = j
      RETURN
      END
