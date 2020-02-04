      SUBROUTINE F2TEX(BUF,LBUF,IBUF,SCR)
C     F2TEX IS THE EXECUTION PHASE OF THE SEISMIC PROCESS F2T, FREQUENCY TO TIME
C  TRANSFORMATION.  THE TRANSFORMATION IS DONE BY THE FFT IN EITHER THE AP
C  OR HOST MEMORY, DEPENDING ON THE IUSEAP SWITCH.  THE SEGY HEADER SIGNAL FOR
C  DATA TYPE IS SET TO 'T'.
C
C
C  ARGUMENTS:
C    BUF,LBUF,IBUF   - THE TRACE HEADER FOLLOWED BY THE TRACE
C    SCR,LSCR,ISCR   - A SCRATCH ARRAY
C
C  WRITTEN AND COPYRIGHTED BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, 21 JANUARY 1984
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
c
c  mod 13 Apr 00 - Add IOFMT
c                - Add Hilbert and Analytic outputs
c  mod 15 Feb 05 - When analytic output, divide the sample interval by 2.
c  mod 24 Mar 05 - Adjust the integer sample interval by NINT of the real.
c  mod 6 Sep 05 - Redo error/warning messages about large sizes.
c               - Also treat numdat and header like 32 bits
C
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP,
     *     IDECIM
      COMMON /edits/ ierror, iwarn, irun, now, icompt, isite, maxsamps
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,IFMT,NSKIP,
     *   SECS,LRENUM,ISRCF,IDTYPE,
     *               nfskip, jform, itxsi, itxdel, nfktrc, origntr
c idtype = 2, frequency-wavenumber domain in rectangular coordinates
c        = 3, frequency-wavenumber domain in polar   coordinates
c        = 4, frequency domain in rectangular coordinates
c        = 5, frequency domain in polar coordinates
c        = 6, depth domain
c        = 7, tau-p or slant stack domain
c        = 8, F-K "user friendly" polar
c        = 9, complex time domain or real Analytic trace
      COMMON /APMEM/AP(500000)                                          ! /* THE AP SIMULATOR DATA ARRAY
      COMMON /f2t/ osi, lprint, iofmt
c iofmt = 1 means real time domain,
c       = 2 means complex time domain,
c       = 3 means Hilbert (real time domain)
c       = 4 means Analytic (complex time domain)
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr,lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr, ilagaptr,
     *                 ilagbptr
      DIMENSION BUF(1111),IBUF(1111),LBUF(1111),SCR(1111)
      INTEGER*2 IBUF
      DATA pio2/1.57080/, iprint1/0/, pi/3.14159/, pi2/6.28319/
c     1 rad = 57.29578deg,    1 deg = .0174532rad
      DATA rad/57.29578/, deg/.0174532/
      SAVE
C
      IF(IBUF(15).EQ.2) RETURN                                          ! /* DON'T PROCESS DEAD TRACES
      IF( idtype .NE. 4 .AND. idtype .NE. 5 ) THEN
          PRINT *,' ***  ERROR  ***  Frequency domain input required.'
          STOP
      ENDIF
      N2 = numdat                                                       ! /* THE NUMBER OF SAMPLES IN THE DATA TRACE
      N=N2/2
      no2 = n / 2
C
      CALL INAP(BUF(NUMHDR+1),N2)                                       ! /* PUT THE DATA IN THE AP OR ARRAY A
      IF(IUSEAP.EQ.1) THEN
         PRINT *,' F2T does not support the AP'
         STOP
      ENDIF
      ITEMP = 4
      ipowr2 = 2
      DO I = 2, 16
         IF( N .EQ. ITEMP ) GO TO 30
         IPOWR2 = IPOWR2 + 1
         ITEMP = ITEMP + ITEMP
      ENDDO
      PRINT *,' ***  ERROR  ***  THE INPUT TO F2T MUST BE A POWER OF 2.'
      STOP
   30 CONTINUE
c     iofmt = 3 means Hilbert, 4 means analytic
      IF( iofmt .EQ. 3 .OR. iofmt .EQ. 4 ) THEN
c****     Do Hilbert in nextad, save the input!
          IF( idtype .EQ. 4 ) THEN                                      ! if the input is in rectangular
              CALL polarc( n, ap(in), ap(nextad), ap(nextad+n) )
          ELSE
              DO i = 0, n2-1
                 ap(nextad+i) = ap(in+i)
              ENDDO
c             make the input rectanular coordinates in ap(in)
              CALL rectc( n, ap(in), ap(nextad), ap(nextad+n) )
              idtype = 4
          ENDIF
c**** the input trace is in rectangular coordinates in ap(in)
c**** the amplitude is in ap(nextad)
c**** The phase is in ap(nextad+n), subtract or add 90 deg or pi/2.
c**** depending on the sign.  Remember fftfwd put it in
c**** -nyqust to 0 to nyqust-deltaf order.  Leave 0 alone.
c**** See Bracewell, pg 267
          DO i = 0, no2-1
             ap(nextad+n+i) = ap(nextad+n+i) - pio2
          ENDDO
          DO i = no2, n2-1
             ap(nextad+n+i) = ap(nextad+n+i) + pio2
          ENDDO
c**** convert the Hilbert to rectangular in ap(nextad+n2)
          CALL rectc( n, ap(nextad+n2), ap(nextad), ap(nextad+n) )
          CALL fftinv( ap(nextad+n2), ipowr2 )
c**** the time domain quadrature is in ap(nextad+n2)
c**** the input frequency domain trace is in rectangular coordinates in ap(in)
          buf(49) = buf(49) / 2.
          ibuf(59) = NINT(buf(49) * 1000000. )
      ENDIF
c****
c****  Convert the input frequency domain trace to the time domain
c****
      IF( IDTYPE .EQ. 5 ) THEN                                          ! 5 means in polar coordinates
          CALL RECTC(n,AP(NEXTAD),AP(IN),AP(IN+N+1))                    ! CONVERT N FREQS TO RECTANGULAR
          DO I = 0, N2-1                                                !  MOVE THE RECTANGULAR COORDINATES BACK TO IN
             AP(IN+I) = AP(NEXTAD+I)
          ENDDO
      ENDIF
      CALL FFTINV(AP(IN),IPOWR2)
c****
c****
      numdat = n
      IF( iofmt .EQ. 2 .OR. iofmt .EQ. 4 ) numdat = n2
      IF( numdat .GT. maxsamps .AND. iprint1 .EQ. 0 ) THEN
          PRINT *,' ***  WARNING  ***  F2T number of ',numdat,
     &        ' samples exceeds sioseis max size of ',maxsamps,'.'
          PRINT *,' The trace is overwriting a sioseis scratch buffer.'
          PRINT *,' The results may or may not be good.'
          PRINT *,' Writing the SEGY number of samples as unsigned int.'
          iprint1 = 1
      ENDIF
      IF( numdat .LT. 32768 ) THEN
          ibuf(58) = numdat
      ELSE
          lbuf(29) = numdat
      ENDIF
c****
c****  OFMT 1 = real time domain
c****
      IF( iofmt .EQ. 1 ) THEN
          j = 0
          DO i = 0, n
             ap(in+i) = ap(in+j)
             j = j + 2
          ENDDO
          idtype = 1
          RETURN
      ENDIF
c****
c****  OFMT 2 = complex time domain
c****
      IF( iofmt .EQ. 2 ) THEN
          idtype = 9
          RETURN
      ENDIF
c****
c****  OFMT 3 = real time domain Hilbert or quadrature
c****
      IF( iofmt .EQ. 3 ) THEN
          j = 0
          DO i = 0, n
             ap(in+i) = ap(nextad+n2+j)
             j = j + 2
          ENDDO
          idtype = 1
          RETURN
      ENDIF
c****
c****  OFMT 4 = real time domain Analytic (input, Hilbert pairs)
c****
      IF( iofmt .EQ. 4 ) THEN
c****    the real is in the evens (see type 1) and the imaginaries
c****    are in the odd.  Write over the odds with the Hilbert.
          DO i = 0, n2-1, 2
             ap(in+i+1) = ap(nextad+n2+i)
             j = j + 2
          ENDDO
          idtype = 9
          RETURN
      ENDIF

      END
