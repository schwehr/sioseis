      logical function chkpra(inproc)
c-------------------------------------------------------------------------------
c chkproc checks to see if there if the user has specified an FK process in the
c procs list.
c   Outputs:
c      .TRUE.  if an FK process is specified.
c      .FALSE. if no FK process specified
c
c   Last Modified:
c      10/20/88
c-------------------------------------------------------------------------------
c
      character*(*) inproc
c
c.. The following are needed to check the user processes (set in GETPRO)
      parameter ( NAMESS  = 47)
      integer pnum                                                      ! The number of proceeses called
      character*7 pnames(NAMESS)
      common /porder/ pnum, iorder(NAMESS)
      common /pnames/ pnames                                            ! The names of the processes called
c
      parameter ( NSETPROC = 1)                                         ! The number of sets of procs
      parameter ( MAXSETL = 4)                                          ! The max. length of a set
      parameter ( NFKPROC = 4)
c      character*7 setproc(NSETPROC)
      character*7 holdproc(MAXSETL)
      character*7 fkproc(NFKPROC)                                       ! List of known FK processes
c
      character*7 cpproc
      integer     holdnum
      logical     found
c
      save  setproc, fkproc
c
c      data  setproc/'FK'/
      data  fkproc / 'FK2TX ','FKMIGR','FKFILT','TX2FK'/
c
      cpproc = ' '
      cpproc = inproc
      nchars = lenstr(cpproc)
      call upcase(cpproc,nchars)
c
c.. Check to see if it is a process set
      if (cpproc.eq.'FK') then
        do 5 i = 1,NFKPROC
   5      holdproc(i) = fkproc(i)
        holdnum = NFKPROC
      else
        holdproc(1) = cpproc
        holdnum     = 1
      endif
c
c Check the procs list for the given process or set of processes
      i = 1
      found = .FALSE.
 10   if ( (i.le.pnum) .AND. .not.found) then
        j = 1
 15     if ( (j.le.holdnum) .AND. .not.found) then
          if (pnames(i).eq.holdproc(j) ) found = .TRUE.
          j = j + 1
          GO TO 15
        endif
        i = i + 1
        GO TO 10
      endif
c
      chkpra = found
      if (chkpra) then
c        print *,'CHKPRA found  ', inproc
      else
c        print *,'CHKPRA did not find ',inproc
      endif
      return
      end
