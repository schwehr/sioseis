      SUBROUTINE scalet( trace, nsamps, def, scalar, rnibs )
c     SCALET converts trace amplitudes into trace rasters for a raster plotter.
c  This routine finds the maximum amplitude (if scalar .LE. 0 ) and finds a
c  multiplier to converts it to be RNIBS*DEF.  If SCALAR is .GT. 0, it simply
c  multiplies the trace by SCALAR * RNIBS.
c
c  ARGUMENTS:
c  trace  - the seismic trace (a set of amplitudes).
c  nsamps - the number of samples in the trace.
c  def    - the deflection, in inches, between peak and trough.
c  scalar - the multiplier to use in converting to rasters.
c  rnibs  - the number of nibs or dots per inch.
c
c  Copyright (C) and written by:
c  Paul Henkart, Scripps Institution of Oceanography, January 1987
c  ALL RIGHTS RESERVED.
c
      DIMENSION trace(nsamps)
      COMMON /SIOAP/ iasgnd,irelse,in,iout,nextad,lapsiz,ifree,iuseap,
     *       idecimf
      SAVE scalr
      DATA scalr/-1/
c
      IF( scalar .GT. 0. ) scalr = scalar
      IF( scalar .EQ. 0. ) scalr = 0.
      IF( iuseap .EQ. 0 .OR. iasgnd .EQ. 0 ) THEN
          IF( scalr .LE. 0. ) THEN                                       ! do we need to pick a scalar?
              xmax = 0.                                                 ! set the min and max
              xmin = 0.
              DO 100 i = 1, nsamps
                 IF( trace(i) .GT. xmax ) xmax = trace(i)
                 IF( trace(i) .LT. xmin ) xmin = trace(i)
  100         CONTINUE
              IF( xmax .LT. -xmin ) xmax = -xmin                        ! use the absolute value
              IF( xmax .EQ. 0 ) THEN                                    ! is the trace all zeroes?
                  PRINT *,' ***  WARNING  ***  Trace is zero or NaN.'
                  RETURN
              ENDIF
              smult = def / xmax * 2                                    ! find the scalar
              IF( scalar .LT. 0. .AND. scalr .LT. 0 )
     &            PRINT *,' The computed PLOT SCALAR is ',smult
              scalr = smult
          ENDIF
          IF( scalr .LT. 1.0E-30) THEN
              PRINT *,' ***  ERROR  *** Plot scalar too small.',scalar
              STOP
          ENDIF
          IF( scalr .GT. 1.0E+30) THEN
              PRINT *,' ***  ERROR  *** Plot scalar too large.'
              STOP
          ENDIF
          x = scalr * rnibs
          DO 110 i = 1,nsamps
             trace(i) = trace(i) * x
  110     CONTINUE
      ELSE
          CALL APWAIT
          IF( scalr .LE. 0. ) THEN                                      ! pick a multiplier
              CALL MAXMGV( in, 1, nextad, nsamps )                      ! find the max magnitude value
              next = nextad + 2
              next1 = next + 1
              def2 = def * 2.
              CALL APPUT( def2, next, 1, 2 )
              CALL APWAIT
              CALL VDIV( nextad, 1, next, 1, nextad, 1, 1 )             ! def/max
              CALL APPUT( rnibs, next1, 1, 2 )                          ! put rnibs in the ap
              IF( scalar .LT. 0. ) THEN                                 ! do we print and save
                  CALL APWR
                  CALL APGET( scalar, nextad, 1, 2 )
                  CALL APWD
                  PRINT *,' the computed plot SCALAR is ',scalar
              ENDIF
              CALL APWD
              CALL VSMUL( nextad, 1, next1, nextad, 1, 1 )
          ELSE
              x = scalr * rnibs
              CALL APPUT( x, nextad, 1, 2 )
          ENDIF
          CALL VSMUL( in, 1, nextad, in, 1, nsamps )
          iout = 0                                                      ! signal that the data is out of the ap
          CALL APWR
          CALL APGET( trace, in, nsamps, 2 )                            ! ge the data out of the ap
          CALL APWD
      ENDIF
      RETURN
      END
