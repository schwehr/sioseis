      subroutine cmpslice(nslice,nx,nrows,big,lunt)
      integer nslice, nx, nrows, lunt
      logical big
c-------------------------------------------------------------------------------
c    This routine compresses a set of nslice time slices which have been
c  multiplexed using a width of nrows down to a true length of nx.
c The dataset may either be contained on disk or in the array T.
c
c   nslice - The number of time slices to compress
c   nx     - The number of ranges used to multiplex a time slice
c   nrow   - The number of traces in the actual dataset
c   lunt   - The file stream for the back up data
c   big    - Is data on disk or in array T ?
c-------------------------------------------------------------------------------
c
      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 65537)                                       ! Size of the AP memory simulator
      parameter (SZTRANP = 262144)                                      ! Transpose array is 512*512
c
      real    apdata(0:SZAPMEM-1)
      real    t(SZTRANP)
      common /APMEM/ apdata
      common /TRANSP/ t
c
      integer POSABS
      parameter (POSABS = 1)
c
      integer ipos                                                      ! The current position in the disk file
      integer maxslic                                                   ! The maximum number of slices that can be held in T
      integer nread, nwrite
      integer nrem, nproc, numslic
      integer istat
c
      if (nx.eq.nrows) return
c
      if (big) then
        maxslic = SZTRANP/nx
        nrem    = nslice                                                ! number of slices left to process
        nproc   = 0                                                     ! number of slices currently processed
  5     continue
        if (nrem.gt.0) then
          numslic = min(nrem,maxslic)
          ipos    = nproc * nx + 1
          nread   = numslic * nx
          call podisc(lunt, POSABS, ipos)
          call rddisc(lunt, t, nread,istat)
          IF( istat .NE. nread ) THEN
            PRINT *,' rddisc error in CMPSLICE',
     *                ' ipos =',ipos,' nread =',nread,
     *                ' istat =',istat,' nx =',nx
            STOP
          ENDIF
c
          call pressary(numslic,nx,nrows)
c
          ipos   = nproc * nrows + 1
          nwrite = numslic * nrows
          call podisc(lunt, POSABS, ipos)
          call wrdisc(lunt, t, nwrite)
c
          nproc = nproc + numslic
          nrem  = nrem - numslic
          go to 5
        endif
      else
        call pressary(nslice,nx,nrows)
      endif
      nx = nrows                                                        ! update nx
      return
      end
c
      subroutine pressary(nslice,nx,nrows)
      integer nslice, nx, nrows
c-------------------------------------------------------------------------------
c This routine compresses a set of nslice time slices which have been
c multiplexed into the transpose array T. They are compressed from a length
c nx down to slices of length nrows, the true length c including pads. The
c first time slice is assumed to start at array position 0
c
c   nslice - The number of time slices to compress
c   nx     - The number of ranges in a time slice
c   nrow   - The length used to multiplex the time slices
c-------------------------------------------------------------------------------
      integer SZTRANP
      parameter (SZTRANP = 262144)                                      ! Transpose array is 512*512
c
      real    t(SZTRANP)
      common /TRANSP/ t
c
      integer indx1, indx2
      integer j, ii
c
      if (nx.eq.nrows) return
c
      do 10 ii = 1, nslice-1
        indx1 = nrows * ii + 1                                          ! base index for compressed slice
        indx2 = nx * ii + 1
        do 5 j = 0, nrows-1
          t(indx1+j) = t(indx2+j)
  5     continue
 10   continue
      return
      end
c
      subroutine getslice(jstart, nslice, nsamp, nx, apn, lunt, big)
      integer jstart, nslice
      integer nsamp, nx, apn, lunt
      logical big
c-------------------------------------------------------------------------------
c getslice gets time slices from back up storage either disk file or the large
c array T  and puts them into the AP array APDATA. The stored data is in time
c slices in reversed time order.
c
c  jstart - The first time slice to transfer to AP mem
c  nslice - The number of slices to transfer
c  nsamp  - The length of the time series to migrate.
c  nx     - The length of an individual time slice including pads.
c  apn    -  The starting index for data in APMEM
c  lunt   - The stream containing the back up data
c  big    - Is data on disk or in array T ?
c-------------------------------------------------------------------------------
c External Variables
      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 65537)                                       ! Size of the AP memory simulator
      parameter (SZTRANP = 262144)                                      ! Transpose array is 512*512
c
      real    apdata(0:SZAPMEM-1)
      real    t(SZTRANP)
      common /APMEM/ apdata
      common /TRANSP/ t
c
      integer index                                                     ! The index into apdata
      integer ipos                                                      ! The current position in the disk file
      integer ipoint                                                    ! The position in the transpose array.
      integer ntrans
      integer istat, jj
c
      ntrans = nx * nslice
      index  = apn
      IF( big ) THEN
         ipos  = (nsamp-jstart) * nx + 1
         CALL podisc( lunt, 1, ipos )
         CALL rddisc( lunt, apdata(index), ntrans,istat)
         IF( istat .NE. ntrans ) THEN
            PRINT *,' rddisc error in GETSLICE',
     *                'ipos=',ipos,' ntrans=',ntrans,
     *               ' istat=',istat,' nsamp',
     *                 nsamp,' jstart=',jstart,' nx=',nx
            STOP
        ENDIF
      ELSE
c
        ipoint = (nsamp-jstart)*nx+1                                    ! pointer to transpose array
           DO 179 jj = 0, ntrans-1
              apdata(index+jj) = t(ipoint+jj)
 179       CONTINUE
      ENDIF
      return
      end


      subroutine putslice(jstart, nslice, nsamp, nx, apn, lunt, big)
      integer jstart, nslice
      integer nsamp, nx, apn, lunt
      logical big
c-------------------------------------------------------------------------------
c putslice puts time slices back into storage either disk file or the large
c array T after they have been migrated in AP memory. The stored data is in time
c slices in reversed time order.
c
c  jstart - The first time slice to transfer to AP mem
c  nslice - The number of slices to transfer to AP mem (Cannot use jend as this
c           may change before slices are written out
c  nsamp  - The length of the time series to migrate.
c  nx     - The length of an individual time slice including pads.
c  apn    -  The starting index for data in APMEM
c  lunt   - The stream containing the back up data
c  big    - Is data on disk or in array T ?
c-------------------------------------------------------------------------------
c External Variables
      integer SZAPMEM, SZTRANP
      parameter (SZAPMEM = 65537)                                       ! Size of the AP memory simulator
      parameter (SZTRANP = 262144)                                      ! Transpose array is 512*512
c
      real    apdata(0:SZAPMEM-1)
      real    t(SZTRANP)
      common /APMEM/ apdata
      common /TRANSP/ t
c
      integer index, ipos, ipoint
      integer ntrans, jj
c
      index  = apn
      ntrans = nslice*nx
      IF ( big ) THEN
        ipos  = (nsamp-jstart) * nx + 1
        CALL podisc( lunt, 1, ipos )
        CALL wrdisc( lunt, apdata(index), ntrans )
      ELSE
        ipoint = (nsamp-jstart)*nx + 1                                  ! pointer to transpose array
        DO 198 jj = 0, ntrans-1
          t(ipoint+jj) = apdata(index+jj)
 198    CONTINUE
      ENDIF
      return
      end
