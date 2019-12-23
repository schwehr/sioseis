        SUBROUTINE AVSPINTR (APA, APB, AFN, ATM, SZA, BFN, BTM, SZB,
     +      SHFAC, LEN, RES, RTM, INXR, SR)
C$R  AV_____: Perform spatial interpolation of 2 piecewise linear functions.
C
C-------------------------------------------------------------------------------
C       VERITAS SOFTWARE LTD.                   CALGARY, ALBERTA, CANADA
C-------------------------------------------------------------------------------
C
C    Allow linear control point interpolation on the merged set of times.
C
C    See "AV_____" routine "AVINTRP".
C
C    This routine computes:
C         RES(RTM(I)/SR) = AFN(ATM(I)/SR)
C                        + (BFN(BTM(I)/SR) - AFN(ATM(I)/SR)) * SHFAC,
C                   for I = 0, LEN/SR, where the below definitions apply:
C
C    APA,APB = AP-120B scratch arrays.
C    AFN,BFN = functions to be spatially interpolated.
C    ATM,BTM = times associated with the functions.
C    SZA,SZB = sizes of functions (ie. # of elements in arrays AFN/ATM & BFN/BTM
C    SHFAC   = multiplier for shot point interpolation
C    LEN     = length of data + 1.
C    RES     = result function
C    RTM     = merged set of times associated with "RES".
C    INXR    = returned # of elements in RES and RTM.
C    SR      = sample rate of data.
C-------------------------------------------------------------------------------
C
C Revised by:   N.M.M.                          Date:   May, 1987
C Reason:       Modifications for 8192 sample limit. (This change amounts to
C               interpolating the functions at the sample rate of the data,
C               rather than at the data length.)
C-------------------------------------------------------------------------------
C
c        IMPLICIT NONE
        INTEGER  APA, APB, SZA, SZB, LEN, INXR, SR
        INTEGER  AFN(SZA), ATM(SZA), BFN(SZB), BTM(SZB),
     +           RES(200), RTM(200)
        REAL     SHFAC
C
        INTEGER  INF, I, AFNADR, BFNADR, ATMADR, BTMADR, SHFADR, NSAMP,
     +           MAP, ISW
C
        REAL     TRACSC(0:1000)
        INTEGER  ITRCSC(0:1000)
        COMMON   /TRSCRBUF/ TRACSC
        EQUIVALENCE (ITRCSC,TRACSC)
C******************************************************************************
C
C Define a common block which is used to simulates the AP120-B data memory
C which has the size of 32K 32-bit floating point words.
C
        REAL    APDATA(0:5000000)
        INTEGER IAPDATA(0:5000000)
        COMMON /apmem/ apdata                                           ! Veritas called this ap120bmd
        EQUIVALENCE (APDATA,IAPDATA)
C
C Define another common block which simulates the 16 S_PAD in the AP120-B
C
        INTEGER APSP(0:15)
        COMMON/AP120BSP/APSP
C
C******************************************************************************
        DATA     INF /0/, bignum/0./
C-------------------------------------------------------------------------------
C
C
C....   Put the 2 piecewise functions and some constants into AP scratch
C....   location 0 - N.
       IF( len .EQ. 0 .OR. sr .EQ. 0 ) CALL EXIT
        NSAMP  = LEN / SR + 1
c        IF   (INF  .EQ.  0)      INF = MAP ('INF ')
        IF( bignum .EQ. 0. ) bignum = 2147483647.                       ! (=0x7FFFFFFF)
        AFNADR = 0
        BFNADR = SZA
        ATMADR = SZA + SZB
        BTMADR = ATMADR + SZA
        SHFADR = BTMADR + SZB
        DO 100 I = 1, SZA
                TRACSC(AFNADR+I-1) = AFN(I)
                TRACSC(ATMADR+I-1) = ATM(I) / SR
  100   CONTINUE
        DO 200 I = 1, SZB
                TRACSC(BFNADR+I-1) = BFN(I)
                TRACSC(BTMADR+I-1) = BTM(I) / SR
  200   CONTINUE
        TRACSC(SHFADR) = SHFAC
c        CALL MAPAPRA (AFNADR, TRACSC(AFNADR), 2*(SZA+SZB)+1)
        DO 210 i = 0, (sza+szb)*2
  210   apdata(afnadr+i) = tracsc(afnadr+i)
        IF (ATM(SZA) .GE. BTM(SZB)) ISW = 1
        IF (ATM(SZA) .LT. BTM(SZB)) ISW = 2
C
C....   Now call the VFC routine to do the spatial interpolation of these 2
C....   functions.
c        CALL SPINTR (AFNADR, ATMADR, SZA, BFNADR, BTMADR, SZB, INF,
c     +          SHFADR, APA, APB, ISW, NSAMP)
c****  fill both ap scratch arrays with "INF" - changed by pch
       DO 220 i = 0,nsamp-1
          apdata(apa+i) = bignum
          apdata(apb+i) = bignum
  220  CONTINUE
        CALL VSSPINTR (AFNADR, ATMADR, SZA, BFNADR, BTMADR, SZB, INF,
     +          SHFADR, APA, APB, ISW, NSAMP)                             ! this is the ap simulator
C
C....   Now get the necessary control points from AP array APB
c        CALL MAPCPUIA (APB, ITRCSC, 2*(SZA+SZB)+1)
        DO 290 i = 0, (sza+szb)*2
  290   itrcsc(i) = iapdata(apb+i)
c        INXR = ITRCSC(2*(SZA+SZB))
        INXR = tracsc(2*(SZA+SZB))
        DO 300 I = 1, INXR
c                RES(I) = ITRCSC(I-1)
                RES(I) = TRACSC(I-1)
c                RTM(I) = ITRCSC(SZA+SZB+I-1) * SR
                RTM(I) = TRACSC(SZA+SZB+I-1) * SR
  300   CONTINUE
        RETURN
        END
