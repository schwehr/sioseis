      SUBROUTINE filters( nt, rvector, srate, f1, f2, idb, ifiltyp,
     &                    minphase, iii )
c     FILTERS produces a frequency domain filter which is multiplied
c  with the transformed time series.  The time series is then inverse
c  transformed back to the time domain.
c
c  ARGUMENTS:
c  nt      - The number of time points in rvector.
c  rvector - The time series to be filtered.  The filtered output is
c            returned in rvector.  10000 max
c  srate   - The sample rate in samples per second.
c  f1      - The low corner frequency.
c  f2      - The high corner frequency.
c  idb     - The INTEGER attenuation in dB at first octave (6dB for each
c            pole)
c  ifiltyp - filter type
c          =0, a combination of low and high pass
c          =1, low pass Butterworth, where f1 is the corner frequency
c          =2, high pass Butterworth, where f1 is the corner frequency
c          =3, notch filter, where f1 is the lower frequency and
c               f2 is the upper one.
c  minphase =1 means minimum phase filter, otherwise it is zero phase.
c  iii     =1, generate a new filter, otherwise no filter is generated
c
c  Author: John Shay, Oregon State Univbersity, April 8, 1988
c  mod 31 jan 92 - change array sizes to 5000 from 10000 so that it
c         compiles on the Apollo!
c                - Also commented out an unused factor = statement.
c                - Correct high pass (type 2)
c  mod 1 July 93 - zero from nt+1 to nfft rather with data!
c  mod 20 Jun 95 by mwh - check trace length and make array lengths a
c                  parameter and increase it to a power of 2!
c mod 16 July 95 by Gail Christeson
c       - reduce output amplitudes by nfft
c       - Fix notch filters
c mod 18 Nov 96 - Alistair - Redo minimum phase so that it works in all cases
c  mod 15 Apr. 97 - Make some of filters.f arrays COMMON.
c  mod 16 Oct 03 - Alistair - Add factor of 2 to minimum phase dropoff
c
      INTEGER MAXLEN
c   IF you change MAXLEN, change contro also!
      PARAMETER( MAXLEN = 16384 * 2 )
      REAL maxabsval, rvector(1)
      LOGICAL firsttime
      COMMON /filtersc/ filt1(MAXLEN), refw(MAXLEN),
     &    filtl(MAXLEN), filth(MAXLEN)
      COMPLEX filt1, refw, filtl, filth
      COMPLEX filt(MAXLEN), filtt(1)
      EQUIVALENCE (filt,filtt)
      SAVE filt, firsttime, nfft, nw
      PARAMETER( pi = 3.14159265 )
      DATA nfftold/0/, f1old/0./, f2old/0./, idbold/0/
c
c  mod. June 95 - Change MAXLEN to 8192
c  mod. 23 Oct 95 - Change wording of error message

c      print *,' filters( ',nt, srate,f1,f2,idb,ifiltyp, minphase,iii
      nfft = 0
c     find the next power of 2 larger than nt
c     find the power of 2 of nfft
      npower2 = 1
      nfft = 2
   10 CONTINUE
      IF( nfft .LT. nt ) THEN
          nfft = nfft + nfft
          npower2 = npower2 + 1
          GOTO 10
      ENDIF
      rnfft = REAL(nfft)
c              force a new filter if this filter length is different from the last
      IF( nfft .NE. nfftold ) firsttime = .TRUE.
      nfftold = nfft

c     force a new filter if the corners are different from the last
      IF( f1 .NE. f1old .OR. f2 .NE. f2old ) firsttime = .TRUE.
      f1old = f1
      f2old = f2
      IF( idb .NE. idbold) firsttime = .TRUE.
      idbold = idb

c      IF( iii .EQ. 1 ) firsttime = .TRUE.

      IF( firsttime ) THEN
          deltafreq = 0.
          tsec = nfft / srate
          nw = nfft / 2

          IF( nfft .GT. MAXLEN .OR. nt .GT. MAXLEN ) THEN
              PRINT *,' ***  ERROR  ***  Too much data for freq filt.'
              PRINT *,' Use fewer samples per trace or change filters.f'
              PRINT *,' nt = ',nt,' nw = ',nw
              STOP
          ENDIF

          DO 500 i = 1, nw+1
             filt(i)  = CMPLX(1.0, 0.0)
             filtl(i) = CMPLX(1.0, 0.0)
             filth(i) = CMPLX(1.0, 0.0)
  500     CONTINUE

          IF( minphase .EQ. 1 ) deltafreq = 2. * pi / tsec

          IF( ifiltyp .EQ. 0 ) THEN
              poles = FLOAT(idb) / 6. + 1.
              tr = 1. / ((f2-deltafreq) * tsec)

              DO 501 j = 1, nw + 1
  501            filtl(j) = filtl(j) / SQRT(1.+(REAL(j-1)*tr)**poles)

              tr = 1. / ((f1 + deltafreq) * tsec)

              DO 502 j = 1, nw+1
                 factor = (REAL(j-1) * tr) ** poles
                 filth(j) = filth(j) * SQRT(factor / (1. + factor))
  502         CONTINUE

              DO 503 j = 1, nw+1
  503            filt(j) = filtl(j) * filth(j)

          ELSEIF (ifiltyp .EQ. 1 ) THEN
              poles = FLOAT(idb) / 6. + 1.
              tr = 1. / ((f1-deltafreq) * tsec)
              DO 504 j = 1, nw + 1
  504            filt(j) = filt(j) / SQRT(1.+(REAL(j-1)*tr)**poles)

          ELSEIF (ifiltyp .EQ. 2 ) THEN
              poles = FLOAT(idb) / 6. + 1.
              tr = 1. / ((f1+deltafreq) * tsec)
              DO 505 j = 1, nw + 1
                 factor = (REAL(j-1) * tr) ** poles
  505         filt(j) = filt(j) * SQRT(factor / (1. + factor))

          ELSEIF (ifiltyp .EQ. 3 ) THEN
              index1 = IFIX((f1-deltafreq) * tsec) + 1
              index2 = IFIX((f2-deltafreq) * tsec) + 2
              IF( index2 - index1 .LT. 16 ) THEN
                  PRINT *, ' ***  ERROR  ***  Notch too narrow'
                  STOP
              ENDIF
              tr = 1. - 10. ** (-FLOAT(idb) / 20.)
              DO 506 j = index1, index2
  506            filt(j) = filt(j) *
     &       (1.+tr*(COS(2.*pi*REAL((j-index1)/(index2-index1)))-1.)/2.)
          ENDIF

          IF( minphase .EQ. 1 ) THEN
c   This is the "efficient" version that reduces the number
c of operations - I have checked it on a simple test case, and believe if it
c doesn't work then the original will not work. However if you want to play
c safe the only real change need is for the rnfft scaling
c  Alistair

              DO 507 i = 1, nw+1
                 IF( REAL(filt(i)) .EQ. 0. ) THEN
                     filt(i) = CMPLX(-30.0,0.0)
                 ELSE
                     filt(i) = CMPLX(ALOG(REAL(filt(i))),0.0)
                 ENDIF
  507         CONTINUE

              DO 508 i = nw+2, nw*2
  508            filt(i) = CONJG(filt(nw*2-i+2))

              CALL fftinv( filt, npower2 )

              DO 509 i = 2, nw+1
                 r = (1. + COS(pi * REAL(i-1) / nw)) / 2.
                 filt(i) = r * filt(i)
  509         continue
c  509            filt(nw*2+2-i) = r * filt(nw*2+2-i)

c              DO 510 i = 1, nw
c  510            filt1(i) = filt(i)

c              DO 511 i = nw+1, nw*2
c  511            filt1(i) = -CONJG(filt(i))
              DO 511 i = nw+1, nfft
                  filt(i) = cmplx(0., 0.)
  511         continue

              CALL fftfwd( filt, npower2 )
c              CALL fftfwd( filt1 , npower2 )

              maxabsval = 0.

c   straight implementation of the analytic Kramers-Kronig 
c   relationship - ajh
              DO 512 i = 1, nw+1
c                 filt(i) = CEXP((filt(i) + filt1(i))/2./rnfft)
c                 filt(i) = CEXP(filt(i)/rnfft)
c..                                     Add factor of 2 here ajh.
                 filt(i) = CEXP(2.*filt(i)/rnfft)
                 maxabsval = AMAX1(CABS(filt(i)),maxabsval)
  512         CONTINUE

              IF( maxabsval .EQ. 0. ) THEN
                  PRINT *,' Filter is zero, data not filtered.'
                  RETURN
              ENDIF

              DO 513 i = 1, nw+1
  513            filt(i) = filt(i) / maxabsval
         ENDIF
         firsttime = .FALSE.
      ENDIF

c     the actual filtering follows
      DO 519 i = 1, nt
  519    refw(i) = CMPLX( rvector(i),0. )
c     zero fill the backend of the data!
      IF( nt+1 .LT. nfft ) THEN
          DO 520 i = nt+1, nfft
c  520    refw(i) = CMPLX( rvector(i),0. )
  520     refw(i) = CMPLX( 0.,0. )
      ENDIF
      CALL fftfwd( refw, npower2 )

      DO 521 i = 1, nw+1
c       if( filt(i) .ne. 0. ) print *,' i=',i,' filt=',filt(i)
  521    refw(i) = refw(i) * filt(i)
      DO 522 i = nw+2, nw*2
  522    refw(i) = CONJG(refw(nw*2-i+2))

      CALL fftinv( refw, npower2 )

      DO 523 i = 1, nt
  523    rvector(i) = REAL(refw(i)) / nfft

      RETURN
      END



