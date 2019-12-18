      SUBROUTINE APPUT(BUF,IADR,N,IDUM)
      DIMENSION BUF(N)
      COMMON /APMEM/ AP(65537)
C      DO 10 I=1,N
C10    AP(IADR+I-1)=BUF(I)
C      MAX=MIN0(N,8)
C      PRINT 20,(BUF(I),I=1,MAX)
C20    FORMAT(' APPUT',8(1X,F7.2))
      PRINT 30,BUF(1),IADR,N,IDUM
30    FORMAT(' CALLED APPUT(',g10.5,3I10)
      RETURN
      END
      SUBROUTINE APGET(BUF,IADR,N,IDUM)
      DIMENSION BUF(N)
      COMMON /APMEM/ AP(65537)
C      DO 10 I=1,N
C10    BUF(I)=AP(IADR+I-1)
C      MAX=MIN0(N,8)
C      PRINT 20,(BUF(I),I=1,MAX)
C20    FORMAT(' APGET',8(1X,F7.2))
      PRINT 30,BUF(1),IADR,N,IDUM
30    FORMAT(' CALLED APGET(',g10.5,3I10)
      RETURN
      END
      SUBROUTINE APWD
      PRINT 10
10    FORMAT(' CALLED APWD')
      RETURN
      END
      SUBROUTINE APWR
      PRINT 10
10    FORMAT(' CALLED APWR')
      RETURN
      END
      SUBROUTINE RFFTB(IN,IOUT,N,MODE)
      COMMON /APMEM/ AP(65537)
      PRINT 10,IN,IOUT,N,MODE
10    FORMAT(' CALLED RFFTB(',4I10)
      ITEMP=2
      DO 20 I=1,20
      IF(N.EQ.ITEMP) RETURN
      ITEMP=ITEMP*2
20    CONTINUE
      PRINT 30,N
30    FORMAT(I10,' IS NOT A POWER OF 2.')
      STOP
      END
      SUBROUTINE RFFTSC(IN,N,MODE,ITYPE)
      COMMON /APMEM/ AP(65537)
      PRINT 10,IN,N,MODE,ITYPE
10    FORMAT(' CALLED RFFTSC(',4I10)
      ITEMP=2
      DO 20 I=1,20
      IF(N.EQ.ITEMP) RETURN
      ITEMP=ITEMP*2
20    CONTINUE
      PRINT 30,N
30    FORMAT(I10,' IS NOT A POWER OF 2.')
      STOP
      END
      SUBROUTINE CVNEG(IN,ININC,IOUT,IOUTIN,N)
      COMMON /APMEM/ AP(65537)
      PRINT 10,IN,ININC,IOUT,IOUTIN,N
10    FORMAT(' CALLED CVNEG(',5I10)
      RETURN
      END
      SUBROUTINE CFFTB(IN,IOUT,N,MODE)
      PRINT 10,IN,IOUT,N,MODE
10    FORMAT(' CALLED CFFTB(',4I10)
      ITEMP=2
      DO 20 I=1,20
      IF(N.EQ.ITEMP) RETURN
      ITEMP=ITEMP*2
20    CONTINUE
      PRINT 30,N
30    FORMAT(I10,' IS NOT A POWER OF 2.')
      STOP
      END
      SUBROUTINE CFFT(IN,IOUT,N,MODE)
      PRINT 10,IN,IOUT,N,MODE
10    FORMAT(' CALLED CFFT(',4I10)
      ITEMP=2
      DO 20 I=1,20
      IF(N.EQ.ITEMP) RETURN
      ITEMP=ITEMP*2
20    CONTINUE
      PRINT 30,N
30    FORMAT(I10,' IS NOT A POWER OF 2.')
      STOP
      END
      SUBROUTINE POLAR(IN,INC,IOUT,IOUTIN,N)
      COMMON /APMEM/ AP(65537)
      PRINT 10,IN,INC,IOUT,IOUTIN,N
10    FORMAT(' CALLED POLAR(',5I10)
      RETURN
      END
      SUBROUTINE APINIT(IAPNUM,IWAIT,ISTAT)
      PRINT 10,IAPNUM,IWAIT,ISTAT
10    FORMAT(' CALLED APINIT(',3I10)
      ISTAT=1
      RETURN
      END
      SUBROUTINE APRLSE
      PRINT 10
10    FORMAT(' CALLED APRLSE')
      RETURN
      END
      SUBROUTINE SIZEMD(ISIZE)
      PRINT 10
10    FORMAT(' CALLED SIZEMD')
      ISIZE=8182*3
      RETURN
      END
      SUBROUTINE APSTAT(IERR,ISTAT)
      PRINT 10
10    FORMAT(' CALLED APSTAT')
      ISTAT=0
      RETURN
      END
      SUBROUTINE CFFTSC(IN,N)
      PRINT 10,IN,N
10    FORMAT(' CALLED CFFTSC',2I10)
      ITEMP=2
      DO 20 I=1,20
      IF(N.EQ.ITEMP) RETURN
      ITEMP=ITEMP*2
20    CONTINUE
      PRINT 30,N
30    FORMAT(I10,' IS NOT A POWER OF 2.')
      STOP
      END
      SUBROUTINE APWAIT
      PRINT 10
10    FORMAT(' CALLED APWAIT')
      RETURN
      END
      SUBROUTINE VMUL(IN,ININC,IIN,IINC,IOUT,IOUINC,N)
      PRINT 10,IN,ININC,IIN,IINC,IOUT,IOUINC,N
10    FORMAT(' CALLED VMUL ',7I10)
      RETURN
      END
      SUBROUTINE VCLR(IN,INC,N)
      PRINT 10,IN,INC,N
10    FORMAT(' CALLED VCLR ',3I10)
      RETURN
      END
      SUBROUTINE VMOV(IN,INC,IOUT,IOINC,N)
      PRINT 10,IN,INC,IOUT,IOINC,N
10    FORMAT(' CALLED VMOV ',5I10)
      RETURN
      END
      SUBROUTINE RECT(IN,INC,IOUT,IOUINC,N)
      PRINT 10,IN,INC,IOUT,IOUINC,N
10    FORMAT(' CALLED RECT(',10I10)
      RETURN
      END
      SUBROUTINE CVMOV(IN,INC,IOUT,IOUINC,N)
      PRINT 10,IN,INC,IOUT,IOUINC,N
10    FORMAT(' CALLED CVMOV(',10I10)
      RETURN
      END
      SUBROUTINE VRAMP(I1,I2,I3,I4,I5)
      PRINT 10,I1,I2,I3,I4,I5
   10 FORMAT(' CALLED VRAMP ',10I6)
      RETURN
      END
      SUBROUTINE VCLIP(I1,I2,I3,I4,I5,I6,I7)
      PRINT 10,I1,I2,I3,I4,I5,I6,I7
   10 FORMAT(' CALLED VCLIP ',10I6)
      RETURN
      END
      SUBROUTINE VINDEX(I1,I2,I3,I4,I5,I6)
      PRINT 10,I1,I2,I3,I4,I5,I6
   10 FORMAT(' CALLED VINDEX ',10I7)
      RETURN
      END
      SUBROUTINE VSMUL(I1,I2,I3,I4,I5)
      PRINT 10,I1,I2,I3,I4,I5
10    FORMAT(' CALLED VSMUL(',10I7)
      RETURN
      END
      SUBROUTINE VSADD(I1,I2,I3,I4,I5)
      PRINT 10,I1,I2,I3,I4,I5
10    FORMAT(' CALLED VSADD(',10I7)
      RETURN
      END
      SUBROUTINE VNEG(I1,I2,I3,I4,I5)
      PRINT 10,I1,I2,I3,I4,I5
10    FORMAT(' CALLED VNEG(',5I6)
      RETURN
      END
      SUBROUTINE VFILL(I1,I2,I3,I4)
      PRINT 10,I1,I2,I3,I4
10    FORMAT(' CALLED VFILL(',4I6)
      RETURN
      END
      subroutine vfix32(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' called vfix32(',5i6)
      return
      end
      subroutine vfix(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' called vfix(',5i6)
      return
      end
      subroutine vpk16(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' called vpk16(',5i6)
      return
      end
      subroutine vflt32(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' called vflt32(',5i6)
      return
      end
      subroutine vflt(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' call vflt(',5i6)
      return
      end
      subroutine maxv(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call maxv(',4i6)
      return
      end
      subroutine maxmgv(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call maxmgv(',4i6)
      return
      end
      subroutine vdiv(i1,i2,i3,i4,i5,i6,i7)
      print 10,i1,i2,i3,i4,i5,i6,i7
   10 format(' call vdiv(',7i6)
      return
      end
      subroutine conv(i1,i2,i3,i4,i5,i6,i7)
      print 10,i1,i2,i3,i4,i5,i6,i7
   10 format(' call conv(',7i6)
      return
      end
      subroutine svemg(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call svemg(',4i6)
      return
      end
      subroutine minv(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call minv',4i6)
      return
      end
      subroutine lvnot(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' call lvnot(',5i6)
      return
      end
      subroutine vsma(i1,i2,i3,i4,i5,i6,i7,i8)
      print 10,i1,i2,i3,i4,i5,i6,i7,i8
   10 format(' call vsma(',8i6)
      return
      end
      subroutine apgsp(i1,i2)
      print 10,i1,i2
   10 format(' call apgsp(',2i6)
      return
      end
      subroutine vma(i1,i2,i3,i4,i5,i6,i7,i8,i9)
      print 10,i1,i2,i3,i4,i5,i6,i7,i8,i9
   10 format(' call apgsp(',9i6)
      return
      end
      subroutine meamgv(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call meamgv(',4i10)
      return
      end
      subroutine vsub(i1,i2,i3,i4,i5,i6,i7)
      print 10,i1,i2,i3,i4,i5,i6,i7
   10 format(' call vsub(',7i7)
      return
      end
      subroutine meanv(i1,i2,i3,i4)
      print 10,i1,i2,i3,i4
   10 format(' call meanv(',4i7)
      return
      end
      subroutine vadd(i1,i2,i3,i4,i5,i6,i7)
      print 10,i1,i2,i3,i4,i5,i6,i7
   10 format(' call vadd(',7i7)
      return
      end
      subroutine vsqrt(i1,i2,i3,i4,i5)
      print 10,i1,i2,i3,i4,i5
   10 format(' call vsqrt(',5i7)
      return
      end
      SUBROUTINE svesq(i1,i2,i3,i4)
      PRINT *,' call svesq(',i1,i2,i3,i4
      RETURN
      END
