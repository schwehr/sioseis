      SUBROUTINE pseuex( buf, lbuf, ibuf, scr )
c     The execution phase of the USGS pseudo reflection coeffiecient
c  process.  The original code was written by Myung Lee ((303)236-5753))
c  The conversion to F77 and the addition of some documentation and
c  indentation was done by pch, who doesn't fully understand what the
c  process does.
C	SCALE :
C	SCALE = 0, COMPUTE ONLY SCALE FACTOR TO CONVERT SEISMIC SECTION TO 
C		   EQUIVALENT PSEDO-REFLECTION COEFFICIENT SECTION.
C		  
C	SCALE <> 0, CONVERT SEISMIC SECTION INTO REFLECTION COEFFICIENT
C		   SECTION INCLUDING WATER BOTTOM DEPTH COREECTION TO THE
C		   REFERENCE TIME (3 SECONDS) AND COMPUTE MEDIAN REFLECTANCE
C                  WITHIN A GIVEN TIME WINDOW.
C	
C	ITYPE: TYPE OF AMPLITUDE IN DETECTING WATER BOTTOM REFLECTION 
C	       TIME AND AMPLITUDE.
C		ITYPE = 1:  USE TRACE AMPLITUDE 
C		ITYPE = 2:  USE PEAK    AMPLITUDE 
C		ITYPE = 3:  USE TROUGH  AMPLITUDE 
C
C	BULK : TIME DELAY IN SECONDS.
C	NMAX : NUMBER OF KEY-VALUE AND APPROXIMATE WATER BOTTOM TIME PAIR.
C	NWIND: SEARCH WINDOW FOR THE WATER BOTTOM DETECTION PREFERABLY LESS 
C	       THAN 100 MSEC.
C	PWIND: SUBBOTTOM REFLECTION TIME WINDOW IN MSEC FOR THE 
C	       MEDIAN REFLECTANCE COMPUTATION.
C	NGROP : NUMBER OF DIVISIONS WITHIN PWIND.
C	ISEC  : OUTPUT WINOW NUMBER (SEQUENCED FROM TOP TO BOTTOM)
C	FOR EXAMPLE, IF PWIND = 500, NGROP = 2, ISEC = 2, THEN
C	             MEDIAN REFLECTANCE IS COMPUTED WITHIN LOWER 250 MS OF
C		     SUBBOTTOM REFLECTIONS, THAT IS 
C		     BETWEEN WATER BOTTOM  TIME (WBT) + 250 MS AND WBT + 500MS.
C		     IF PWIND = 600, NFROP = 4, ISEC = 3, THEN
C		     MEDIAN REFLECTANCE IS COMPUTED WITHIN WBT + 300MS
C		     AND WBT + 450 MSEC.
C
C		
C 	YOU HAVE TO RUN THIS PROGRAM TWICE. FIRST WITH JTYPE = 1  AND USING
C	ABOUT 100 CDP'S HAVING NON-DIFFRACTIVE DEEP WATER BOTTOM REFLECTIONS.
C	IN SECOND TIME, RUN WITH SFACT EQUAL TO THE ONE COMPUTED IN THE 
C	PREVIOUS RUN (when sfact=0). THE SCALE FACTOR IS THE printed to
c       STDOUT (The printer output file) when sfact=0.
c
c
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /HYDBK1/ ITYPE, JTYPE, BULK, SCALE, DTT, NGROP, ISEC,  
     &     pwind, nwind, slope, bb, wbcref, lprint, fno, lno, winlen,
     &     iunit
      INTEGER ngrop, isec, nwind, itype, fno
      REAL pwind, slope, bb, wbcref, bulk
        DIMENSION lbuf(111), buf(111), scr(111), ibuf(111)
        INTEGER*2 ibuf
c
c****   get the shot/rp number
        IF( lbuf(7) .EQ. 0 ) THEN
            no = lbuf(3)
        ELSE
            no = lbuf(6)
        ENDIF
c****   If we're finding the scalar and the shot/rp is outside, forget it
        IF( scale .EQ. 0 ) THEN
        IF( no .LT. fno .OR. no .GT. lno ) RETURN
        ENDIF
        IF( ibuf(15) .EQ. 2 ) RETURN
        nsamp = ibuf(58)
        delay = buf(46)
        bulk = delay
        si = buf(49)
        dtt = si * 1000.
        nwind = NINT( winlen * 1000. )
c****   hardwire NFLT
        nflt = 1000 / dtt + 1
c****   iwbt corresponds to Lee's LS
        iwbt = (buf(50) - delay) / si + 1
        iwbtindx = iwbt + numhdr                                        ! The index of the water bottom
c****   assume scr is at least 2 traces long
        iscr = nsamp*2 + 1
	IF( scale .EQ. 0. ) THEN
            jtype = 1
	ELSE
	    jtype = 2
        ENDIF
	LW	= IFIX(NWIND / DTT) + 1 
	LPW	= IFIX(PWIND / DTT) + 1
C	COMPUTE TRACE AMPLITUDE VIA HLIBERT TRANSFORM
c       buf(iwbtindx) is the water bottom 
c       scr is the return array, nflt long
c       nflt is the wondow length
c       scr(iscr) is a scratch array, a power of 2 long.
C
	CALL HAMP(buf(iwbtindx), scr(iwbtindx), NFLT, scr(iscr) )
C
C   	COMPUTATION OF WATER BOTTOM ARRIVAL TIME AND  AMPLITUDE.
c       arg 1 is the data array
c       arg 2 is the time (in samples) of the start of the window
c       arg 3 is the returned time in mils of the picked water bottom
c       arg 4 is the returned amplitude of the pick
c       arg 5 is the the window length in number of samples
	IF( ITYPE .EQ. 1) THEN
	    CALL WBCOMP(scr(iwbtindx), iwbt,  TIMP, AMPP, LW, ITYPE, DTT)
	ELSE
	    CALL WBCOMP(buf(iwbtindx), iwbt,TIMP,AMPP,LW,ITYPE, DTT)
	ENDIF
	IF(JTYPE .EQ. 1) THEN
	   TIMP	= TIMP / 1000 + delay
           IF( IAND(lprint,2) .NE. 0 ) WRITE(6,100) no, timp, ampp
	   WRITE(IUNIT, 100) no, TIMP, AMPP
100	   FORMAT(I10, F10.3, 2E16.5)
	   IF( no .EQ. lno ) THEN
C              IF THE LAST TRACE FOR THE ANALYSIS, COMPUTE SCALE FACTOR TO CONVERT
C              SEISMIC SECTION INTO REFLECTION COEFFICIENTS SECTION
C
	      CALL DSCALE(IUNIT, SFACTOR, REFC, scr, scr(iscr) )
		  WRITE(6,200) refc, scale
	      WRITE(IUNIT, 200) REFC, scale
200           FORMAT( 4X, '****  REFLECTION COEFFICIENT ****', F10.5, /,
     1                4X, '****  SCALE FACTOR           ****', E20.5)
	   ENDIF
	   RETURN
	END IF
	LST	= IFIX(TIMP / DTT) + 1 
	LCOMP	= LPW / NGROP
	ISHIFT	= LCOMP * (ISEC - 1) + LST
c****   WBCORT - 
c       arg 1 is the data trace, which WBCORT modifies.
c       arg 2 is the returned time, in seconds, of the water bottom
c       arg 3 is the user given scale factor
c       arg 4 is the deep water delay in seconds
c       arg 5 is the number of samples in the data trace
c       arg 6 is 
	CALL WBCORT(buf(numhdr+1), TIMP, SCALE, delay, NSAMP, DMPP)
	CALL WBCORT(scr(numhdr+1), TIMP, SCALE, delay, NSAMP, AMPP)
	CALL MEDREF(scr(ISHIFT+numhdr), scr(iscr), LCOMP, AMID)
	TIMPB	= TIMP / 1000 + delay
	WRITE(IUNIT,100) no, TIMPB, AMPP, AMID
	IF( IAND(lprint,2) .NE. 0 ) WRITE(6,100) no, TIMPB, AMPP, AMID
	RETURN
	END
c********************************************************************
	SUBROUTINE HAMP(A, B, NSAMP, CA)
	DIMENSION A(1), B(1)
 	COMPLEX CA(2)
	PHI	= ACOS( - 1.)
	NFRET	= 2
20		NFRET	= NFRET + NFRET
		IF(NFRET .GE. NSAMP) GO TO 99
		GO TO 20
99	CONTINUE
	CON	= PHI
	CONNEG	= -PHI
	DO 100 k = 1, NFRET
	   CA(K)	= (0., 0.)
  100   CONTINUE
	DO 110 k = 1, NSAMP
	   CA(K)	= CMPLX(A(K), 0.0)
  110   CONTINUE
	CALL HLBTA(CA, NFRET, CON, CONNEG) 
C
C	COMPUTE MODULUS
C
	DO 200 k = 1, NSAMP
	   CC	= AIMAG(CA(K))
	   DD	= A(K) * A(K) + CC * CC
	   B(K)	= SQRT(DD)
  200   CONTINUE
	RETURN
	END
c********************************************************************
	SUBROUTINE FORK(LX, CX, CON)
	COMPLEX CX(LX), CARG, CEXP, CW, CTEMP
	J	= 1
	SC	= SQRT(1.0 /LX)
	DO 30 I	= 1, LX
	IF(I .GT. J) GO TO 10
	CTEMP	= CX(J) * SC
	CX(J)	= CX(I) * SC
	CX(I)	= CTEMP
10	M	= LX / 2
20	IF(J .LE. M) GO TO 30
	J	= J - M
	M	= M / 2
	IF(M .GE. 1) GO TO 20
30	J	= J + M	
	L	= 1
40	ISTEP	= 2*L
	DO 50 M	= 1, L
	CARG	= (0., 1.0) * CON * (M - 1) / L
	CW	= CEXP(CARG)
	DO 50 I	= M, LX, ISTEP
	CTEMP	= CW * CX(I + L)
	CX(I + L) = CX(I) -CTEMP
50	CX(I)	= CX(I) + CTEMP
	L	= ISTEP
	IF(L .LT. LX) GO TO 40
	RETURN
	END
c********************************************************************
	SUBROUTINE HLBTA(CX, LX, CON, CONNEG)
	COMPLEX CX(LX)
	CALL FORK(LX, CX, CON)
	K	= LX / 2
	DO 20 J = 2, K
	CX(J)	= CX(J) + CX(J)
20	CX(LX + 2 -J) = (0.,0.)
	CALL FORK(LX, CX, CONNEG)
	RETURN
	END
c********************************************************************
	SUBROUTINE MEDREF( A, B, NSAMP, AMID)
	DIMENSION A(1), B(1)
	NMID	= NSAMP / 2
	JUMPP	= NSAMP / 2
	JUMP	= JUMPP
	ILAST	= NSAMP- JUMPP
	DO 100 k = 1, NSAMP
	   B(K)	= A(K)
  100   CONTINUE
C
C	MEDIAN  COMPUTATION
C
  110   CONTINUE
	IF (JUMP .GE. 1) THEN
555	   CONTINUE
	   IFLAG	= 0
	   DO 300 K	= 1, ILAST
	      KX	= K + JUMP
  210         CONTINUE
              IF (B(K) .GT. B(KX)) THEN
		TEMP	= B(KX)
		B(KX)	= B(K)
		B(K)	= TEMP
		IFLAG	= 1
                GOTO 210
              ENDIF
  300      CONTINUE
	   IF(IFLAG .GT. 0) GO TO 555
	   JUMP	= JUMP / 2
	   ILAST	= NSAMP - JUMP
           GOTO 110
        ENDIF
C
C	END OF COMPUTATION
C
	AMID	= B(NMID + 1)
	RETURN
	END
c********************************************************************
	SUBROUTINE WBCOMP(A, LSTART, TIM, AMP, IWIND, INP, DT) 
	DIMENSION A(1)
	ST	= (LSTART - 1) * DT
	GO TO (11, 11, 22), INP
11	IST	= 1
		AMAX	=  A(IST)
		JDEX	=  IST
		DO 12   K = IST + 1, IST + IWIND - 1 
			IF(A(K) .GT. AMAX) THEN
		  	   AMAX    = A(K)
			   JDEX	= K
		        END IF
   12           CONTINUE
	AMP	=  AMAX
	TIM	=  (JDEX -1) * DT + ST
	RETURN
22	IST	= 1
		AMIN	=  A(IST)
		JDEX	=  IST
		DO 23 K = IST + 1, IST + IWIND - 1
		IF(A(K) .LT. AMIN) THEN
			AMIN    = A(K)
			JDEX	= K
		END IF
23              CONTINUE
	AMP	=  AMIN
	TIM	=  (JDEX -1) * DT + ST
	RETURN
	END
c********************************************************************
	SUBROUTINE WBCORT( T, TIM, dummy1, dummy2, NSAMP, AMP)
	DIMENSION T(1)
C
C WATER BOTTOM CORRECTION, REFERENCE WATER BOTTOM IS 3 SEC TWO-WAY TIME
C
      COMMON /HYDBK1/ ITYPE, JTYPE, BULK, SCALE, DTT, NGROP, ISEC,  
     &     pwind, nwind, slope, bb, wbcref, lprint, fno, lno, winlen,
     &     iunit
      INTEGER ngrop, isec, nwind, itype, fno
      REAL pwind, slope, bb, wbcref, bulk
c	SLOPE	= - 5.36 / 100.
c	BB	= 0.411
c	REF	= SLOPE * 3. + BB
	REF	= SLOPE * wbcref + BB
	TIMP	= TIM / 1000 + BULK
	CORR	= SLOPE * TIMP + BB
	GAIN	= SCALE * REF / CORR
	AMP	= AMP * GAIN
	DO 100 K = 1, NSAMP
	   T(K)	= T(K) * GAIN
  100   CONTINUE
	RETURN
	END
c********************************************************************
	SUBROUTINE DSCALE(idummy, dummy1, refc, A, B)
	DIMENSION A(1), B(1)
      COMMON /HYDBK1/ ITYPE, JTYPE, BULK, SCALE, DTT, NGROP, ISEC,  
     &     pwind, nwind, slope, bb, wbcref, lprint, fno, lno, winlen,
     &     iunit
      INTEGER ngrop, isec, nwind, itype, fno
      REAL pwind, slope, bb, wbcref, bulk
c	AA	= - 5.36 /100.
c	BB	= 0.411
	I	= 1
	REWIND IUNIT
999	CONTINUE
	READ(IUNIT, 100, END=888) A(I), B(I)
	I	= I + 1
	GO TO 999
888	CONTINUE
100	FORMAT(10X,F10.3,E16.5)
	SUMA	= 0
	SUMB	= 0
	I	= I - 1
	DO 1 K	= 1, I
	SUMA	= SUMA + A(K)
	SUMB	= SUMB + B(K)
1	CONTINUE
	SUMA	= SUMA / I
	SUMB	= SUMB / I
	REFC	= slope * SUMA + BB
	SCALE	= REFC / SUMB
	RETURN
	END
