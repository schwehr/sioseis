      SUBROUTINE PRNTX1(IN,IOUT,NIN,NOUT)
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
C
      INTEGER*4 NIN,NOUT
      INTEGER*2 IN(1),IOUT(1)
      INTEGER*2 itemp,jtemp, itemp1, itemp2
      
      i = 1
c  start the string with a 5 - put the printronix in plot mode
      itemp = 1280                                                      /* 1280 = hex(0500)
      j = 1
   10 CONTINUE
      jtemp = IAND( rshift(in(i),10), 63)                                /* 63=hex(3f)
      iout(j) = IOR(itemp,jtemp)
      j = j + 1
      itemp = IAND( lshift(in(i),4), 16128)                              /* 3f00
      itemp1 = IAND( lshift(in(i),2), 60)                                /* 003c
      i = i + 1
      itemp2 = IAND( rshift(in(i),14), 3)
      jtemp = IOR(itemp1,itemp2)
      iout(j) = IOR(itemp,jtemp)
      j = j + 1
      itemp = IAND(in(i),16128)
      jtemp = IAND( rshift(in(i),2), 127)                                /* hex(7F)
      iout(j) = IOR(itemp,jtemp)
      j = j + 1
      itemp1 = IAND( lshift(in(i),12), 12288)                            /* hex(3000)
      i = i + 1
      itemp2 = IAND( rshift(in(i),4), 3840)                              /* hex(0f00)
      itemp = IOR(itemp1,itemp2)
      jtemp = IAND( rshift(in(i),6), 63)                                 /* 003f
      iout(j) = IOR(itemp,jtemp)
      j = j + 1
      itemp = IAND( lshift(in(i),8), 16128)
      i = i + 1
      IF( i .LT. nin ) GO TO 10
c****
c****   Turn on the plot mode bit on the plot bytes!
c****
      iout(1) = IOR( iout(1),64 )
      DO 100 i = 2, j-1
  100 iout(i) = IOR( iout(i),16448 )
      iout(j) = IOR(itemp,10)
      iout(j) = IOR( iout(j),16384 )
      nout = j
      RETURN
      END
