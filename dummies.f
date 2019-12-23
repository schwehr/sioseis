      SUBROUTINE dummies
c     Entry points to satisfy the loader
c
c      ENTRY fftcc
c      ENTRY fftrc
c           PRINT *,' called IMSL routines.'
c		     STOP
      ENTRY attach_alias
            PRINT *, ' CALLED attach_alias'
            RETURN
      ENTRY attach
            PRINT *, ' CALLED attach'
            RETURN
      ENTRY detach
            PRINT *, ' CALLED detach'
            RETURN
      ENTRY vprint
            PRINT *, ' CALLED vprint'
            RETURN
      ENTRY vplot
            PRINT *, ' CALLED vplot'
            RETURN
      ENTRY vwrite( buf, nbytes )
            PRINT *, ' CALLED vwrite, nbytes=',nbytes
            RETURN
      ENTRY vflush
            PRINT *, ' CALLED vflush'
            RETURN
      ENTRY second
         RETURN
      ENTRY timer
         RETURN
      ENTRY getput
            PRINT *,' Called getput'
            STOP
      ENTRY destroy
            PRINT *,' called destry'
            STOP
      ENTRY p32
            PRINT *,' called p32'
            STOP
      ENTRY pr2ibm
            PRINT *,' called pr2ibm'
            STOP
      ENTRY dr2ibm
            PRINT *,' called dr2ibm'
            STOP
      ENTRY usscti
            PRINT *,' called usscti'
            STOP
      ENTRY ussctc
            PRINT *,' called ussctc'
            STOP
      ENTRY i22i8
            PRINT *,' called i22i8'
            STOP
      ENTRY i42i8
            PRINT *,' called i42i8'
            STOP
      ENTRY i82i4
            PRINT *,' called i82i4'
            STOP
      ENTRY i82i2
            PRINT *,' called i82i2'
            STOP
      ENTRY ifp41
            PRINT *,' called ifp41'
            STOP
      END
