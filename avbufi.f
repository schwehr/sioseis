      subroutine avbufin(buf, rbuf, rowsz,intrp,ctrlin,list,sh)
      integer rowsz, buf(rowsz,5000), intrp, ctrlin, list(50), sh
      real          rbuf(rowsz,5000)
c-------------------------------------------------------------------------------
c  aurora-vax  interpolate control point parameters from pool buffer
c
c  veritas software ltd.                   calgary, alberta, canada
c
c parameters:
c    buf:       buffer containing parameters to be interpolated by control
c               points.
c               buf(1,1) =      column index of 1st control point
c               buf(2,1) =      column index of last control point
c               buf(1,2)..buf(rowsz,2)          = interpolated control point
c               buf(1,buf(1,1))..buf(rowsz,buf(1,1))    =  1st control point
c               buf(1,buf(2,1))..buf(rowsz,buf(2,1))    = last control point
c    rowsz:     size of control point (ie. row size)
c    intrp      row index of interpolation option (s,c,e) for control points.
c    ctrl       row index of control point # (ie. shot# or crb#)
c               if -ve then flag for special linear interpolation (see avintrp)
c    list       list of indexes to each control point (row elements). if
c               positive then do floating point interpolation, if negative,
c               avoid interpolation. end of list =0.
c    sh         the shot/crb number of the current velocity function.
c
c    To clear up any ambiguities this is how the shot-point interpolation
c option is to be interpreted:
c
c    given shot control points n1 < n2 < n3, and interpolation options:
c         's', 'c', and 'e'. ( 's' = 1, 'c' = 2, 'e' = 3 )
c
case 1    's' n1, 'c' n2, 'e' n3.
c      a  interpolate from n1 to n2
c      b  interpolate from n2+1 to n3
c      c  skip from n3+1 to 9999
c
case 2    's'-n1, 'c'-n2, 's'-n3
c      a  interpolate from n1 to n2
c      b  straight-line from n2+1 to n3-1
c      c  straight-line from n3 to 9999
c
case 3    's'-n1, 'e'-n2, 's'-n3
c      a  interpolate from n1 to n2
c      b  skip from n2+1 to n3-1
c      c  straight-line from n3 to 9999
c
c CALL CHAIN : fdmiex:fdmvel:avbufin
c
c EXTERNALS : avintrp
c             mymove  (at end of source file)
c
c NOTES: In FD migration we should never reach 3a or 3b since the control pts.
c        are defined only by 's','c','c'...,'e' and so there is no straight
c        lining between controls unless to Controls are equal.
c
c revised by:   n.m.m.                          date:   may, 1987
c reason:       add 8192 sample limit with no sample rate or length restriction.
c  17 june 1988 by pch to make f77, non-vms, non-ap, non-veritas
c  17 june 1988 by pch to make f77 - changing 's','c','e' to 1,2,3 respectively.
c  17 june 1988 by pch to make sh an argument rather that who knows - like
c     how is sh set overwise?
c
c  4/25/89 a.j.h. - Added rbuf to the subroutine arguments. This array is a
c                   real array equivalent to buf. This allows velocities to be
c                   interpolated as reals and not integers.
c                 - Added routine mymove to force conversion of control pt
c                   velocities to real.
c-------------------------------------------------------------------------------
c
      integer  inlen,    outlen,   xoutle,   youtle,   zoutle,
     +         insr,     outsr,    xoutsr,   youtsr,   zoutsr,
     +         inntr,    outntr,   xoutnt,   youtnt,   zoutnt,
     +         insamp,   outsam,   xoutsa,   youtsa,   zoutsa,
     +         cursam,   cursr,    crb,      tr,       endsh,    taper
c
      logical  gather,   stacke,   dead
      real     dist
      common   /avdata/
     +         inlen,    outlen,   xoutle,   youtle,   zoutle,
     +         insr,     outsr,    xoutsr,   youtsr,   zoutsr,
     +         inntr,    outntr,   xoutnt,   youtnt,   zoutnt,
     +         insamp,   outsam,   xoutsa,   youtsa,   zoutsa,
     +         cursam,   cursr,    crb,      tr,       endsh,
     +         taper,    gather,   dist,     stacke,   dead
c
      integer
     +       adtrin,   adtrot,   adwin1,   adwin2,   adwin3,   adwin4,
     +       adnliv,   adstak,   adscra
      common /avap/
     +       adtrin,   adtrot,   adwin1,   adwin2,   adwin3,   adwin4,
     +       adnliv,   adstak,   adscra
c
      integer sz, ctrl, ixa, ixb, ixend, ctrla, ctrlb, intrpa,
     +        intrpb, savsh, savix
c
      integer rwvbas, rwnpair                                           ! corresponding row index
c
      integer IXVBAS, IXNPAIR                                           ! Index into list for row pointer
      parameter (IXVBAS  = 1)                                           ! Index to row base of velocity
      parameter (IXNPAIR = 3)                                           ! Index to row containing num. times/velocities
c
      rwvbas  = list(IXVBAS)
      rwnpair = list(IXNPAIR)
c
c....   find out at which control point to start.
    1   continue
        sz    = iabs (rowsz)
        ctrl  = iabs(ctrlin)
        ixa   = buf(1,1)
        ixend = buf(2,1)
        if (ixa .ge. ixend) then
                buf(1,1) = ixend
                ixa = ixend
                ixb = ixend
        else
                ixb = ixa+1
        end if
        ctrla   = buf(ctrl,ixa)
        intrpa  = buf(intrp,ixa)
        ctrlb   = buf(ctrl,ixb)
        intrpb  = buf(intrp,ixb)
c
c....   check all cases:
c
c....   1. current control point >= [s|c|e] ctrla  (sh = current control point)
        if (ctrla .ge. sh) then
          nv = buf(rwnpair,ixa)
          call mymove (buf(1,ixa), buf(1,2),rbuf(1,2), sz, nv, rwvbas)
          buf(ctrl, 2) = sh                                             ! ajh Added update of cdp no.
c
c....     2. current control point = [s|c|e] ctrlb
        else if (ctrlb .eq. sh) then
          nv = buf(rwnpair,ixb)
          call mymove (buf(1,ixb), buf(1,2),rbuf(1,2), sz, nv, rwvbas)
          buf(1,1) = ixb                                                ! Increment start control point
c
c....     3. [s|c|e] ctrla  < current control point <  [s|c|e] ctrlb
        else if (ctrla.lt.sh .and. sh.lt.ctrlb) then
c
c....          3.a intrpa = e, intrpb = s|c|e
          if (intrpa .eq. 3 ) then                                      ! Only one control for job
            nv = buf(rwnpair,ixb)
            call mymove (buf(1,ixb), buf(1,2),rbuf(1,2), sz, nv, rwvbas)
            buf(1,1) = ixb
c
c....           3.b intrpa = s|c, intrpb = s
          else if (intrpb .eq. 1) then
            nv = buf(rwnpair,ixa)
            call mymove (buf(1,ixa), buf(1,2),rbuf(1,2), sz, nv, rwvbas)
            buf(ctrl,2) = sh
c
c....           3.c intrpa = s|c, intrpb = c|e
          else
            call avintrp (buf(1,2), buf(1,ixa), buf(1,ixb),
     +                    rowsz, intrp, ctrlin, list, inlen, sh,
     +                    adwin1, adwin3, cursr)
            buf(ctrl,2) = sh
          end if
c
c....   4. [s|c|e] ctrla <= [s|c|e] ctrlb < current control point
        else
c
c....             4.a get next parameter control point.
         if (ixb .lt. ixend) then
           buf(1,1) = buf(1,1) + 1
           go to 1
c
c....             4.b no more parameter control points. use the last one.
c....       (if possible)
        else
          buf(1,1) = ixend
          nv = buf(rwnpair,ixb)
          call mymove (buf(1,ixb), buf(1,2),rbuf(1,2), sz, nv, rwvbas)
c
          if (intrpb .ne. 3) buf(ctrl,2) = sh
        end if
      end if
c
      return
      end
c
      subroutine mymove(bufa, bufi,rbufi, sz, nv, rwvbas)
      integer sz, nv, rwvbas
      integer bufa(sz), bufi(sz)
      real    rbufi(sz)
c-------------------------------------------------------------------------------
c   mymove transfers the velocity-time function for the control point from the
c input buffer bufa to the output buffer bufi/rbufi while at the same time
c forcing conversion of velocities to real values.
c  bufa       - The buffer to copy from
c  bufi/rbufi - The buffer to copy to
c  sz         - Length of buffers
c  nv         - The number of velocities in the buffer
c  rwvbas     - The base index for velocities
c-------------------------------------------------------------------------------
c
      do 10 i = 1, rwvbas-1
        bufi(i) = bufa(i)
  10  continue
c
      do 20 i = rwvbas, rwvbas+nv-1
        rbufi(i) = bufa(i)
  20  continue
      return
      end
