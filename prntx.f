      SUBROUTINE PRNTX(IN,IOUT,N,NOUT)                                  /* Vax Unix 4.2 BSD Version
C      PRNTX CONVERTS A STRING OF RASTERS (A CONTINUOUS STRING OF BITS, EITHER
C  ON OR OFF, EACH BIT REPRESENTING A DOT) TO A PRINTRONIX PLOT MODE
C  CHARACTER STRING.  THE PRINTRONIX EXPECTS THE UPPER 2 BITS ON (THE PRIME
C  NEEDS THE PARITY BIT ON AND THE PRINTONIX NEEDS THE OTHER TO INDICATE
C  THE PLOT MODE).  THIS ROUTINE THUS TAKES 6 BIT BYTES FROM THE INPUT,
C  CONVERTS IT TO AN 8 BIT BYTE, AND PUTS ON THE 2 HIGH ORDER BITS.
C  THE PRINTRONIX ALSO NEEDS THE BITS WITHIN THE BYTE FLIPPED IN ORDER.
C
C  ARGUMENTS:
C  IN    - INPUT RASTER STRING TO BE CONVERTED.
C  IOUT  - THE OUTPUT CHARCTER STRING.  IOUT MUST BE DIFFERENT FROM IN. IOUT
C          WILL BE LONGER THAN IN!!!
C  NIN   - THE NUMBER OF 16 BIT INTEGERS IS IN.  NIN*2 BYTES WILL BE CONVERTED.
C  NOUT  - THE NUMBER OF INTEGER*2 WORDS RETURNED IN IOUT.  NOUT*2 BYTES WILL
C          BE IN IOUT.  ***  NOTE  *** THIS IS A RETURN ARGUMENT!!
C
C  COPYRIGHTED BY:
C  PAUL HENKART, SEISMIC REFLECTION PROCESSORS, SAN DIEGO, 7 FEBRUARY 1983
C  ALL RIGHTS RESERVED.
C
c    Prime computers go 2**15,2**14,2**13,...,2**0, whereas DEC compters go
c  2**0,2**1,2**2,...,2**15
c   Printronix wants 2**0,2**1,2**2,2**3,2**4,2**5
c Therefore, Prime need the bit order flipped as well as made into 6 bit byrtes.
c  DEC only needs to be made into 6 bit bytes (with plot and parity bits)

      INTEGER RSHIFT
      INTEGER*4 NIN,NOUT
      CHARacter*1 IN(1),IOUT(1)
      DIMENSION IFLIP(64),IDEC(64)
      SAVE
      LOGICAL FIRST
      DATA FIRST /.TRUE./
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


      nin=n*2
      I=1
c  start the string with a 5 - put the printronix in plot mode
      iout(1)=CHAR(5)
      J=2
   10 CONTINUE
      k=iCHAR(in(i))
      l=IAND(k,63)
      iout(j)=CHAR(idec(l+1))
      j=j+1
      l=rshift(k,6)
      l=IAND(l,3)
      i=i+1
      if(i.gt.nin) GOTO 100
      k=iCHAR(in(i))
      m=IAND(k,15)
      m=lshift(m,2)
      m=IOR(m,l)
      iout(j)=CHAR(idec(m+1))
      j=j+1
      l=rshift(k,4)
      l=IAND(l,15)
      i=i+1
      if(i.gt.nin) GOTO 100
      k=iCHAR(in(i))
      m=IAND(k,3)
      m=lshift(m,4)
      m=IOR(m,l)
      iout(j)=CHAR(idec(m+1))
      j=j+1
      l=rshift(k,2)
      l=IAND(l,63)
      iout(j)=CHAR(idec(l+1))
      j=j+1
      i=i+1
      if(i.lt.nin) GOTO 10
  100 continue
      m=j/4*4
      iout(m)=CHAR(10)
      nout=m/2
      return
      end
