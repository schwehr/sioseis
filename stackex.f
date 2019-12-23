      SUBROUTINE STACKEX(ITRACE,TRACE,LTRACE,NREADY)
C      STACK SUMS ALL TRACES SENT IT UNTIL TRACE HEADER WORD 51 (LTRACE(51) IS
C  NEGATIVE. THE STACKED TRACE IS DIVIDED BY THE NUMBER OF TRACES CONTRIBUTING
C  AT EVERY SAMPLE, THUS MUTE TIME ARE ACCOUNTED FOR AND THE STACK IS PROPERLY
C  SCALED. A TRACE IS READY TO BE OUTPUT ONLY WHEN SUBROUTINE STACK SETS ARGUMENT
C  NREADY TO 1.  THE FIRST VALUE RETURNED (IN AP LOCATION IN) HAS A TIME OF THE
C  SMALLEST DELAY OF ANY TRACE IN THE STACK.
C     STACK IS NOT NECESSARILY ONLY A CDP STACK PROGRAM, ANY KIND OF STACK CAN
C  BE DONE PROVIDING THAT THE LAST TRACE TO BE STACKED HAS INTEGER*4 WORD 51 OF
C  THE TRACE HEADER SET TO -1.
C
C   ARGUMENTS:
C      ITRACE - THE INPUT ARRAY WITH TRACE HEADER.  ITRACE(1) MUST BE
C               THE FIRST WORD OF THE HEADER.  16 BIT INTEGER.
C      TRACE  - THE INPUT TRACE ARRAY, WITH DEEP WATER DELAYS AND TRACE HEADER
C               INCLUDED.  THIS IS THE SAME AS ITRACE, BUT A FLOATING POINT
C               ARRAY. (PRIME DOESN'T ALLOW EQUIVALENCING OF ARGUMENTS!)
C      LTRACE - THE INPUT TRACE ARRAY WITH TRACE HEADER, BUT FOR LONG INTEGERS.
C      NREADY - THE NUMBER OF TRACES READY TO OUTPUT.  THE NUMBER OF STACKED
C               TRACES.
C             =0,  NO TRACES ARE READY FOR THE NEXT PROCESS.
C             =1,  1 TRACE IS READY, THE NEXT PROCESS MAY BE CALLED WITH TRACE
C
C   COMMON NEEDED:
      COMMON /APMEM/ A(32767)                                           ! THE AP SIMULATOR MEMORY
C
C
C    PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JANUARY 1980
C
C
c MODIFICATIONS: (a little late!)
c  13 Apr 89 - remove renumbering of output rps. Why was it renumbered?
c     pch      The document says it uses the rp number associated with
c              the first rp.
c  12 Apr 89 - Change the check for live/dead trace from .NE. 1 to GT 1
c     pch      because some data came around with the trace id flag = 0
c  14 Apr 89 - Put the number of live traces into header word 17 rather
c     pch      than 51.  The multi use of 51, end of gather flag, confused
c              other processes, besides word 17 is defined as the cdp fold.
c              Now 51 = -1 on every stacked trace!
c  March 1991 - ss@utig - Use the shot number of the first trace in the rp
c              for the shot number of the stacked trace
c  May 91 - pch - do the fold (word 17) on live traces, not dead traces!
c  Oct 91 by pch - set header word 51 to 0 rather than -1.
c  22 Dec 92 - Add lheader (how to fill parts of the output trace header)
c  mod 22 Dec 97 - Change MIN0 to MIN and MAX0 to MAX
c  mod 29 Jun 98 - Make sure the end-of-gather flag is set (undo Oct 91!
c  mod 1 May 00 - Ditto.  Every stack trace needs the EOG flag, o.w.
c                diskin can't use fno/lno.
c  mod 30 July 02 - remove zeroing of trace header words 19 & 21 (x-coords)
c  mod 30 July 04 - Save the entire first trace header
c                 - Change lheader so that 1 = use entire first trace header
c  mod 29 Jul 06 - Change mindel from 16000 to 9999999 for bug delay and smal
c                  sample interval like on Knudsen!
c  mod 19 Dec 06 - Reallocate buffers if the input trace is bigger than expected.
c  mod 12 Jan 07 - Compute the coordinates of the RP and put them in the
c                header loaction of the shot and zero the receiver (x,y)
c  mod 28 Jul 08 - Put the shot number (word 3) into the energy source
c            number (word 5) before zeroing it.  Hope ProMax is ok with that.
c  mod 12 Apr 10 - Eliminate mute times in seconds (header words 47 & 48)
c  mod 20 Mar 13 - Use an average of rp source x&y rather than just one.
c  mod 17 Jun 14 - If all are dead, then don't try to average the x & y coordinates (divide by 0)
c
      DIMENSION ITRACE(1111),TRACE(1111),LTRACE(1111)
      INTEGER*2 ITRACE
      LOGICAL FIRST
      DIMENSION ltrhdr(60)
      INTEGER x_src, y_src, x_rcv, y_rcv
      SAVE
      COMMON /stack/ lheader, lprint, newstack
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ,IFREE,IUSEAP
      COMMON /READT/ILUN,NUMHDR,NUMDAT,LUNHDR,IREELN,INTRCS,IFMT,NSKIP
      COMMON /WRITET/ IOUNIT,NSAMPS,NOREEL,IPOSAF,IOFMT,NOTRCS
      DATA FIRST/.TRUE./
C
      NREADY=0                                                          !  SET IT TO NO OUTPUT READY
      NSAMPS = numdat                                                   ! DATA LENGTH IN SAMPLES
      SR=TRACE(49)                                                      ! THE SAMPLE INTERVAL IN SECONDS
      IDELAY=NINT(TRACE(46)/SR)                                         !  THE DELAY IN SAMPLES
      IF( .NOT. FIRST ) THEN
c****     redo the buffering if this trace is too long
          IF( nsamps .LT. isum ) GO TO 100
          PRINT *,' ***  WARNING  ***  Increased trace length causing
     & STACK buffers to be reallocated.'
      ENDIF
      FIRST=.FALSE.
      maxwds = nsamps + nsamps + idelay                                 ! the maximum output sample length
      CALL INAP(TRACE(NUMHDR+1),NSAMPS)                                 ! MAKE SURE THE AP IS ASSIGNED ETC.
      ISUM=NEXTAD                                                       ! THE AP ADDRESS OF THE STACKED TRACE
      IDENOM=ISUM+MAXWDS                                                !  THE AP ADDRESS OF THE STACK GRACE DIVISOR
      NEXTAD=IDENOM+MAXWDS                                              ! THE NEXT AVAILABLE AP ADDRESS - SAVES ISUM AND IDENOM
      IF(NEXTAD.LT.LAPSIZ) GO TO 90
      PRINT 80,NEXTAD
   80 FORMAT(' *****   TOO MUCH AP BEING USED -',I6,' STACK.')
      STOP
   90 CONTINUE
      SAVESI=TRACE(49)
      TAPER=.020                                                        ! MUTING'S TAPER LENGTH IS 20 MILS
      NDONE=0                                                           !   THE NUMBER OF TRACES STACKED
  100 CONTINUE
      IF(MINDEL.NE.9999999) ISAVDE=MINDEL                                 ! SAVE MINDEL IN CASE THIS TRACE IS A NULL TRACE
c      IS=TRACE(47)/SR+1                                                 !  THE INDEX OF THE START OF THE MUTE
      is = FLOAT(itrace(56)) * 1000. / sr + 1.
      ITAPER=TAPER/SR                                                   ! THE TAPER LENGTH IN SAMPLES
      IF(IS.NE.1) IS=IS+ITAPER                                          ! ACCOUNT FOR MUTE TAPER
c      IE=TRACE(48)/SR+1                                                 ! THE INDEX OF THE END OF THE MUTE
      ie = FLOAT(itrace(57)) * 1000. / sr + 1.
      IF(IE.LT.NSAMPS+IDELAY.AND.IE.GT.1) IE=IE-ITAPER
      IF(IDELAY.GT.IE) IE=IDELAY
      CALL INAP(TRACE(NUMHDR+1),NSAMPS)                                 ! INITIALIZE THE AP AND SEND DATA TO AP
      IF(NDONE.NE.0) GO TO 200
C****
C****  CLEAR THE STACKED TRACE TO ZERO AND THE DIVISOR TO ONE ON THE FIRST TRACE
C****  OF EACH STACKED TRACE.  DO MAXWDS BECAUSE NOT ALL TRACES WITHIN A GATHER
C****  HAVE TO BE THE SAME LENGTH OR EVEN START AT THE SAME PLACE.
C****
      IF( IUSEAP .EQ. 1 ) THEN                                          ! DON'T USE THE AP IF IUSEAP=0
          CALL VCLR(ISUM,1,MAXWDS)                                      ! SET THE WHOLE STACKED TRACE TO ZERO
          CALL APPUT(1.,NEXTAD,1,2)                                     !  PUT 1. IN AP LOCATION NEXTAD
      ELSE
          J = ISUM-1                                                    ! SET THE STACKED TRACE TO ZERO
          K = IDENOM-1                                                  ! SET THE DENOMINATOR (THE DIVISOR) TO 1.
          konstant = 1.
          IF( newstack .EQ. 1 ) konstant = 0.
          DO I = 1, MAXWDS
             A(J+I) = 0.
             a(k+i) = konstant
          ENDDO
      ENDIF
c      IF( newstack .EQ. 1 ) THEN
cc**** sorry, this is ugly.  We need to set the divisor on the first
cc**** trace on the "newstack" algorithm
c          idivis = idenom + idelay - 1
c          input = in - 1
c          DO i = 1, nsamps
c             IF( a(input+i) .NE. 0. ) a(idivis+i) = 1.
c          ENDDO
c      ENDIF
      IFREE=IFREE+1                                                     ! SIGNAL THAT THE AP CONTENTS MUST BE SAVED FROM PROCESS TO
C                PROCESS (THE AP CAN'T BE RELEASED) BECAUSE THE PARTIAL SUM AND
C                THE DIVISOR ARE IN THE AP
      DO i = 1, 60
         ltrhdr(i) = ltrace(i)
      ENDDO
      lfor = ltrace(6)
      MINDEL = 9999999                                                  ! NOW SET THE VARIABLES FOR FINDING THE SMALLEST DELAY
      IEND=0
      MINMUT=0
      IF( IUSEAP .EQ. 1 ) THEN                                          ! IF NO AP, THEN WE ALREADY DID THE DENOMINATOR
          CALL APWD                                                     !  WAIT FOR THE 1. TO GET TO THE AP
          CALL VFILL(NEXTAD,IDENOM,1,MAXWDS)                            !  SET THE DENOMINATOR TO 1.'S
      ENDIF
      IF( ITRACE(15) .GT. 1 ) GO TO 300                                 !  IS IT A LIVE TRACE
      GO TO 280
C****
C****  ADD A 1. TO THE DENOMINATOR FOR THE PART OF THE TRACE NOT MUTED.
C****  (WE CAN'T DO THAT ON THE FIRST TRACE BECAUSE WE'D DIVIDE BY ZERO)
C****  REMEMBER THAT THE MUTE MIGHT BE A SURGICAL MUTE RATHER THAN A FRONTAL MUTE.
  200 CONTINUE                                                          !  NOT THE FIRST TRACE OF A STACK
      IF( ITRACE(15) .GT. 1 ) GO TO 300                                 !  IS IT A LIVE TRACE
      IF( IUSEAP .NE. 0 ) THEN                                          ! DON'T NEED THIS IF NO AP
          CALL APPUT(1.,NEXTAD,1,2)                                     !  PUT A 1. IN THE AP.
          CALL APWD                                                     ! WAIT
      ENDIF
      IF( newstack .EQ. 0 ) THEN
          IF( IS .GT. 1 ) THEN
              INDX=IDENOM+IDELAY                                        ! ADD 1. TO THE DENOM ON DATA PRIOR TO MUTING
              IF( IUSEAP .EQ. 1 ) THEN
                  CALL VSADD(INDX,1,NEXTAD,INDX,1,IS)
              ELSE
                  J = INDX-1
                  DO I = 1,IS
                     A(J+I)=A(J+I)+1.
                  ENDDO
              ENDIF
          ENDIF
          IF( IE .LT. NSAMPS+IDELAY-1 ) THEN
              INDX = IDENOM+MAX(IDELAY,IE)-1                            ! GO FROM THE MUTE OR THE DELAY!
              N = NSAMPS-IE+1+IDELAY
              IF( IUSEAP .EQ. 1 ) THEN
                  CALL VSADD(INDX,1,NEXTAD,INDX,1,N)                    ! ADD 1. FOR THE DATA AFTER THE MUTE
              ELSE
                  J = INDX-1
                  DO I = 1,N
                     A(J+I)=A(J+I)+1.
                  ENDDO
              ENDIF
          ENDIF
      ENDIF
c  280 MINDEL=MIN0(MINDEL,IDELAY)
  280 MINDEL=MIN(MINDEL,IDELAY)
c      IEND=MAX0(IEND,IDELAY+NSAMPS)                                     ! THE LARGEST STACKED SAMPLE
      IEND=MAX(IEND,IDELAY+NSAMPS)                                     ! THE LARGEST STACKED SAMPLE
c      MINMUT=MIN0(MINMUT,IE)
      MINMUT=MIN(MINMUT,IE)
      INDX=ISUM+IDELAY                                                  !  THE ADDRESS IN THE AP OF WHERE TO STACK
      IF( newstack .EQ. 0 ) THEN
          IF( IUSEAP .EQ. 1) THEN
              CALL VADD(IN,1,INDX,1,INDX,1,NSAMPS)                          ! ADD IT IN
          ELSE
             J=INDX-1
             K=IN-1
             DO i = 1, nsamps
                A(J+I) = A(J+I) + A(K+I)
             ENDDO
          ENDIF
      ELSE
c**** The "new" stack simply adds nonzero amplitudes to the stacked
c**** trace and increases the divisor by 1, thus eliminating mute,
c**** smute, despike kill times.
          istack = isum + idelay - 1
          idivis = idenom + idelay - 1
          input = in - 1
          DO i = 1, nsamps
             IF( a(input+i) .NE. 0. ) THEN
                 a(istack+i) = a(istack+i) + a(input+i)
                 a(idivis+i) = a(idivis+i) + 1.
             ENDIF
          ENDDO
      ENDIF
      NDONE=NDONE+1                                                     ! THE NUMBER OF TRACES STACKED
      IF( ndone .EQ. 1 ) THEN
          x_src = 0
          y_src = 0
          x_rcv = 0
          y_rcv = 0
      ENDIF
      IF( itrace(15) .EQ. 1 ) THEN
          x_src = x_src + ltrace(19)
          y_src = y_src + ltrace(20)
          x_rcv = x_rcv + ltrace(21)
          y_rcv = y_rcv + ltrace(22)
      ENDIF
c****
c****    Done adding
c****
  300 CONTINUE                                                          !   ARE WE DONE
      IF(LTRACE(51).GE.0) RETURN                                        ! CHECK FOR END OF GATHER FLAG
      IF( NDONE .GT. 1 ) THEN
          IF( IUSEAP .EQ. 1) THEN
              CALL VDIV(IDENOM,1,ISUM,1,ISUM,1,MAXWDS)                  ! DIVIDE BY THE MUTING PATTERN
          ELSE
              J=ISUM-1
              K=IDENOM-1
              IF( newstack .EQ. 1 ) THEN
                  DO i = 1, maxwds
                     IF( a(k+i) .GT. 0 ) a(j+i) = a(j+i) / a(k+i)
                  ENDDO
              ELSE
                  DO 306 I=1,MAXWDS
  306             A(J+I)=A(J+I)/A(K+I)
              ENDIF
          ENDIF
      ENDIF
      NSAMPS=IEND-MINDEL                                                ! THE STACKED TRACE HAS NSAMPS IN IT
      IF( IUSEAP .EQ. 1 ) THEN
          CALL VMOV(ISUM+MINDEL,1,IN,1,NSAMPS)                          ! MOVE THE TRACE SO THAT MINDEL IS FIRST
      ELSE
          J = IN - 1
          K = ISUM+MINDEL-1
          DO I=1,NSAMPS
             A(J+I)=A(K+I)
          ENDDO
      ENDIF
c****
c****  Set the trace header.  1 = use info from the first gather trace.
c****                         2 = Use first and zero some of it
c****                         3 = use info from the last gather trace.
c****
      IF( lheader .EQ. 1 .OR. lheader .EQ. 2 ) THEN
          DO i = 1, 60
             ltrace(i) = ltrhdr(i)
          ENDDO
      ENDIF
      IF( lheader .EQ. 2 ) THEN
          ltrace(5) = ltrace(3)
          ltrace(3) = 0
          ltrace(4) = 0
          ltrace(10) = 0
      ENDIF
      ltrace(7) = 1                                                     ! rp trace number
      ltrace(51) = -1                                                   ! end-of-gather
      ITRACE(15)=1                                                      ! SET HEADER TO LIVE TRACE
      itrace(17) = ndone                                                ! THE NUMBER OF TRACES STACKED IN (THE CDP)
c      ltrace(19) = ( ltrace(19) + ltrace(21) ) / 2
c      ltrace(20) = ( ltrace(20) + ltrace(22) ) / 2
c**** I'm a bit worried about overflow
c          x_src will be the average x_src.   x_src is now the sum of all the x_src in the gather.
      IF( ndone .NE. 0 ) THEN
          ltrace(19) = ( x_src/ndone + x_rcv/ndone ) / 2
          ltrace(20) = ( y_src/ndone + y_rcv/ndone ) / 2
      ENDIF
      ltrace(21) = 0
      ltrace(22) = 0
      IF( NDONE .EQ. 0 ) THEN                                           ! IS IT DEAD?
          ITRACE(15) = 2
          MINDEL = ISAVDE
          IF( MINDEL .EQ. 9999999 ) MINDEL = 0                            ! THIS OCURRS WHEN THE FIRST STACKED TRACE IS DEAD
          NSAMPS = NUMDAT
          SR = SAVESI
          TRACE(49) = SR
          IF( iuseap .EQ. 0) THEN                                       ! zero out the dead trace
              DO 330 i = 1, nsamps
  330         a(in+i-1) = 0.
          ELSE
              CALL vclr(in,1,nsamps)
          ENDIF
      ENDIF
      ITRACE(55)=MINDEL*SR*1000.+.5                                     ! THE DELAY, IN MS., AFTER STACK
      ITRACE(57)=MINMUT*SR*1000.+.5                                     ! THE POST STACK END MUTE TIME
      ITRACE(58)=NSAMPS                                                 ! NUMBER OF SAMPLES IN THE OUTPUT
      NUMDAT=NSAMPS                                                     ! MAKE SURE THAT OUTPUT OUTPUTS NSAMPS!
c      TRACE(48)=MINMUT*SR                                               ! END MUTE
      TRACE(46)=MINDEL*SR                                               ! DELAY
      delay = FLOAT(mindel) * sr
      NOTRCS=1                                                          ! THE NUMBER OF TRACES PER OUPUT RECORD (RP)
      NDONE=0
      NREADY=1                                                          !  TELL THE WORLD THERE IS A STACKED TRACE FINISHED
      IFREE=IFREE-1                                                     ! STACK NO LONGER NEEDS TO SAVE THE AP MEMORY
      RETURN
      END
