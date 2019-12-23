      subroutine txfkinit
C-------------------------------------------------------------------------------
C This routine initializes the global common blocks for tx2fk/fk2tx and is
C called as the beginning of the respective edit stages
C-------------------------------------------------------------------------------
C INCLUDE FILES
c INCLUDE txfkgbl.inc
c.. This is the global common block for the FK routines. At present it is used
c.. by TX2FEX, FK2TEX & their associated edit routines to indicate whether they
c.. have been called & to pass file unit numbers.
c
c  Last Modified 10/18/88
c
      logical txinit, fkinit                                            ! Execution initialization ?
      logical txed, fked                                                ! Edit initialization ?
      integer ltxunt1, ltxunt2                                          ! File Stream ID for tx2fex scratch file
      integer lfkunt1, lfkunt2                                          ! File Stream ID for fk2tex scratch file
      integer txunit                                                    ! Used Stream ID for tx2fex scratch file
      integer ohdrtxfk                                                  ! File unit holding tx headers
      logical tmptxhdr                                                  ! Indicates if tx header file is temporary
      common /TXFKE/ txinit, fkinit, txed, fked, txunit,
     $               ltxunt1, ltxunt2, lfkunt1, lfkunt2,
     $               ohdrtxfk, tmptxhdr
C PROGRAM
      logical first
c
      data first /.TRUE./
c
      if (first) then
        first  = .FALSE.
        txinit = .FALSE.
        fkinit = .FALSE.
        fked   = .FALSE.
        ohdrtxfk = 0
        tmptxhdr = .FALSE.
      endif
C
      return
      end
c
c===============================================================================
c
      integer function Pow2(size)
C-------------------------------------------------------------------------------
C  This function calculates the nearest power of 2 above the input variable size
C Returns -1 if size <= 0
C-------------------------------------------------------------------------------
      integer    DEBUG
      parameter (DEBUG = 1)
      integer    MAXTR
      parameter (MAXTR = 32769)
c
      integer size, length, tempow
c
      if (size.le.0) then
        pow2   = -1
        if (DEBUG.gt.0) then
          print *,' *** POW2 error *** size < 0'
          call EXIT
        endif
      else if (size.ge.MAXTR) then
        if (DEBUG.gt.0) then
          print *,' *** POW2 error *** size > MAXTR'
          call EXIT
        endif
      else
        tempow = 0
        length = 1
    5   if(length.lt.size) then
          length = length + length
          tempow = tempow + 1
          go to 5
        endif
        pow2 = tempow
      endif
      return
      end
