      SUBROUTINE dbt( wdepth )
c     Parse a NMEA $DBT (depth below transducer) string and return
c  it as the water depth in meters.
c    e.g.  $DBT,0017.6,f,0005.4,M
c          $SDDBT,,,3807.,M,,*2A
c
c   Paul Henkart, Scripps Institution of Oceanography, October 2006
c
c
      CHARACTER*82 out
      SAVE wdepth_last
      DATA wdepth_last/0./

      wdepth = 0.
c**** token 1 is $DBT or $dbt
      CALL getokec( out, nchars )
      CALL getokec( out, nchars )
      CALL getokec( out, nchars )
      CALL getokec( out, nchars )
      IF( nchars .GT. 0 ) CALL dcode( out, nchars, wdepth, istat )
      IF( wdepth .LE. 0 ) wdepth = wdepth_last
      wdepth_last = wdepth
c**** Don't call getoke for the last field because it may not be terminated
      RETURN
      END

