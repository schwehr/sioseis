      SUBROUTINE swp_trhdr( ibuf, lbuf )
c**** Do the segy trace header byte swap.
c**** This routine does not care what endian the header is nor what endian the computer is.
c**** This routine does take into account what segy rev the header is though.
c
c Rev 0 had bytes 181-240 as unassigned and sioseis used it for REAL entries, thus used swap32
c Knudsen Rev 0 (that's all I've seen) use 181-240 for INTEGER*2, but sioseis doesn'r use them.
c Rev 1 bytes 181-240 are mixed.  Sioseis only uses the deep water delay scalar
c     in bytes 215-216 (ibuf(108))
c
c   Written April 2014 and thus not used before sioseis ver 2014.4
c
      INTEGER*2 ibuf(*)
      INTEGER*4 lbuf(*)
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr,
     *               nrskip, nfiles, irewind, delay, segyrev, si
c
      CALL swap32( lbuf(1), 7 )
      CALL swap16( ibuf(15), 4 )           ! bytes 29 - 36
      CALL swap32( lbuf(10), 8 )           ! bytes 37 - 68
      CALL swap16( ibuf(35), 2 )           ! bytes 69 - 70
      CALL swap32( lbuf(19), 4 )           ! bytes 73 - 88
      CALL swap16( ibuf(45), 5 )           ! bytes 89 - 98
      CALL swap16( ibuf(53), 7 )           ! bytes 105 - 118
      CALL swap16( ibuf(79), 6 )           ! bytes 157 - 168
      IF( segyrev .EQ. 0 ) THEN
          CALL swap32( lbuf(46), 15 )  ! bytes 181 - 240 - unassigned
      ELSE
          CALL swap32( lbuf(46), 8 )          ! bytes 181-212 (long 46-53)
          CALL swap16( ibuf(107), 2 )         ! bytes 213 - 216  (215-216 is scalar for delay)
          CALL swap32( lbuf(55), 6 )          ! bytes 217-240 (long 55-60)
      ENDIF
      END
