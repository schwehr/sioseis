      subroutine avintrp(bufi, bufa, bufb, size, intrp, ctrlin,
     +                     list, inlen, sh, adscr1, adscr2, cursr)
c-------------------------------------------------------------------------------
c  av_____: perform control point (shot or crb) interpolation.
c
c       veritas software ltd.                   calgary, alberta, canada
c
c    bufi       interpolated parameter control points
c    bufa       1st parameter control point buffer
c    bufb       2nd parameter control point buffer
c    size       the size of bufi, bufa, bufb.
c    intrp      index to interpolation option (s|c|e)
c    ctrlin     index to parameter control point #
c                  if ctrlin < 0 then array list contains special parameters
c                  (detailed below), and bufi will be greater than size
c    list       list of indexes for interpolation action. all values are
c               assumed to require integer interpolation, except:
c               1. if an index in list > 0 then the value at index is
c                  assumed to be floating point.
c               2. if an index in list < 0  then no interpolation is done.
c               3. index = 0 is the end of the list. (the list must always
c                  contain at least 1 element = 0.)
c               special case when ctrlin < 0:
c    list(1)    = index of start of function to be interpolated
c    list(2)    = index of times corresponding to the function values
c    list(3)    = index of element counts (# of elements in bufa might not
c                 (bufa and bufb might not be same length)
c    list(4)    = start of list of indices of elements
c                 to move directly from bufa to bufi
c    inlen      length of data (necessary for special interpolation)
c    sh         shot (crb) now being processed.
c    adscr1, adscr2     ap scratch arrays used for spatial interpolation
c    cursr      = current sample rate (ms.)
c
c CALL CHAIN  fdmiex:fdmvel:avbufin:avintrp
c
c EXTERNALS   myspintr
c
c NOTE : This is essentially a do-nothing routine under FDM since ctrlin < 0
c        always and control is passed to myspintr. The case of sh = ctrla or
c        sh = ctrlb is handled by avbufin.
c
c REVISIONS:
c  17 june 1988 by pch to convert to f77, non-ap, non-vms, non-veritas.
c  17 june 1988 by pch to remove the call to avflint
c  29 April 1989 ajh : A revised and smaller version that calls myspintr
c
c-------------------------------------------------------------------------------
c
      integer SZBUF
      parameter (SZBUF = 500)
      integer       bufi(SZBUF), bufa(SZBUF), bufb(SZBUF)
      integer       size, intrp,
     +              ctrlin, list(50), inlen, sh, adscr1, adscr2,
     +              cursr
c
      real          a, shfac
      integer       ctrl, ctrla, ctrlb, li, sz, intrpa, fn,
     +              tm, cnt, scix, inx, i, skip
c
c....     check whether the current shot is one of the control points
      ctrl  = iabs(ctrlin)
      ctrla = bufa(ctrl)
      ctrlb = bufb(ctrl)
c
      if( (ctrla.eq.sh).or.(ctrla.eq.ctrlb) ) then
        do 5 i = 1, iabs(size)
           bufi(i) = bufa(i)
    5    continue
         go to 40
      endif
c
      li     = 1
      sz     = size
      intrpa = bufa(intrp)
      shfac  = float(sh-ctrla)/float(ctrlb-ctrla)
c
c....     shot point interpolation
      if (ctrlin.gt.0) then
         do 10 i = 1, sz
            skip = list(li)
c
c....                            interpolate a floating point value
            if (skip.eq.i) then
              bufi(i) = bufa(i) + (bufb(i)-bufa(i)) * shfac
              li = li + 1
c....                                     Don't interpolate
            else if (skip.eq.-i) then
              bufi(i) = bufa(i)
              li      = li + 1
c
c....                                  interpolate an integer value
            else
              a       = bufa(i)
              bufi(i) = a+(float(bufb(i))-a)*shfac
            end if
   10     continue
c
          bufi(intrp) = intrpa
c
c....                             special interpolation
      else
         call myspintr(bufi, bufi, bufa, bufb, size, intrp, ctrlin,
     $                    list, inlen, sh, cursr)
      endif
   40 bufi(ctrl) = sh
      return
      end
