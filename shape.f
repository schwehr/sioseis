      SUBROUTINE shape( lb, b, ld, d, la, a lc,c, ase, space )
      DIMENSION b(lb), d(ld), a(la), c(lc), space(2)
c      CALL cross( lb, b, lb, b, la, space )  ! autocorrelation
      CALL convo( 2, b, lb, b, lb, space, la )
c      CALL cross( ld, d, lb, b, la, space(la+1))   ! crosscorrelation
      CALL convo( 2, d, ld, b, lb, space(la+1), la )
      CALL eureka( la, space, space(la+1), a, space(2*la+1))
      CALL dot( ld, d, d dd )
      CALL dot( la, a, space(la+1), ag )
      ase = ( dd - ag ) / dd
c      CALL fold( la, a lb, b, lc, c )   !  convolution
      CALL convo( a, la, b, lb, c, lc )
      RETURN
      END
