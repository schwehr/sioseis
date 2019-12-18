       SUBROUTINE GATHED(SCR,LSCR,ISCR)
C                              PROCESS GATHER
C                              ------- ------
C
C  DOCUMENT DATE: 15 March 1994
C
C    A GATHER IS A COLLECTION OR REARRANGEMENT OF TRACES ACCORDING TO SOME
C  CRITERIA.  PROCESS GATHER COLLECTS OR REARRANGES THE INPUT TRACES ACCORDING
C  TO THE REFLECTION POINT (RP) NUMBER CALCULATED BY PROCESS GEOM.  THE GEOM
C  PARAMETERS MAY BE MANIPULATED BY THE USER TO GATHER THE INPUT TRACES ACCORDING
C  TO ANY CRITERIA BY FUDGING THE GEOM PARAMETERS.  A CONSTANT OFFSET GATHER
C  OF A UNIFORM MARINE LINE MAY BE MADE BY OMITTING TRACES VIA PROCESS INPUT.
C      PROCESS GATHER SORTS EACH GATHER BY THE ABSOLUTE VALUE OF THE SHOT-
C  RECEIVER DISTANCE, SO THAT THE SHORTEST RANGE TRACE IS FIRST WITHIN THE
C  GATHER.  EACH GATHER IS IS TERMINATED BY SETTING A SPECIAL FLAG IN THE
C  TRACE HEADER.  A GATHER RECORD IS THE COLLECTION OF ALL THESE TRACES.
C     SEE PROCESS GEOM FOR THE METHOD OF CALCULATING RP NUMBERS.
c     Gather creates a disk file to store the partial gathers while
c  the data is being read.  Gather assumes that the geometry of the data
c  does not skip around very much. i.e. the geometry doesn't go backwards
c  nor does it skip more than a cable length forward.  The temporary
c  disk file can hold MAXRPS rps (preset to 5 plus the number of traces 
c  per shot from the SEGY binary header), with each each rp able to hold
c  a maximum of MAXTRS traces (also preset to the number of traces per
c  shot in the SEGY binary header), with each traces having a maximum
c  of NWRDS samples, with each sample being 4 bytes long (except on the
c  Cray).  The temporary disk file size will be: 
c      maxtrs * maxrps * (nwrds+240) * 4
c  The preset values of maxtrs, maxrps, and nwrds are designed for the
c  marine geometry of advancing .5 groups between shots (96 cdp from
c  a 96 trace streamer).
C      A NULL SET OF GATHER PARAMETERS MUST BE GIVEN EVEN IF ALL THE
C  PARAMETERS ARE THE PRESETS.  E.G.  GATHER
C                                          END
C                                     END
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C  FRP    - THE FIRST RP NUMBER TO GATHER.  TRACES WITH RP NUMBERS LESS THAN FRP
C           ARE NOT GATHERED.  RP NUMBERS ARE CALCULATED BY PROCESS GEOM.
C           THIS PARAMETER SHOULD BE USED WHEN RESTARTING A PARTIALLY
C           COMPLETED GATHER RUN.  THE USE OF FRP WILL CAUSE PROCESS GATHER TO
C           REWRITE THE PROCESS INPUT PARAMETERS SO THAT THE UNNEEDED INPUT
C           WILL NOT BE PROCESSED.  BOTH THE PROCESS INPUT AND PROCESS GEOM
C           PARAMETERS MUST BE GIVEN PRIOR TO THE PROCESS GATHER PARAMETERS.
c           FRP only works with PROCESS INPUT.
C           PRESET=RP NUMBER OF THE FIRST TRACE.
C  RPINC  - THE INCREMENT OF RP NUMBERS BETWEEN THE RPS TO GATHER.  THE ONLY
C           TRACES GATHERED WILL HAVE BIN NUMBERS FRP, FRP+RPINC, FRP+2*RPINC,
C           FRP+3*RPINC, . . . . ETC.
C           PRESET=1.
C  NWRDS  - THE LARGEST NUMBER OF SAMPLES PER TRACE IN THE JOB.  THIS SHOULD BE
C           TRACE LENGTH PLUS THE TRACE HEADER LENGTH.
C           PRESET=FROM FIRST INPUT TRACE.
C  MAXRPS - THE MAXIMUM NUMBER OF BINS (OR RP'S) THAT ARE NEEDED ON
C           THE DISC AT ANY ONE TIME.  IN MARINE WORK THE NUMBER OF TRACES
C           PER SHOT WILL SUFFICE SINCE NO TWO UNGATHERED TRACES WITH THE SAME
C           RP NUMBER ARE MORE THAN A CABLE LENGTH AWAY.
C           PRESET=THE NUMBER OF TRACES FROM THE SEGY BINARY HEADER PLUS 5
C  MAXTRS - THE MAXIMUM NUMBER OF TRACES ANY ONE GATHER CAN HAVE.  IN RP GATHERS
C           THIS IS THE MAXIMUM CDP ALLOWED.
C           PRESET=THE NUMBER OF TRACES FROM THE SEGY BINARY HEADER.
C  MINTRS - THE MINIMUM NUMBER OF TRACES EACH GATHER CAN HAVE.  IF MINTRS=0 AND
C           NO INPUT TRACES CONTRIBUTE TO A GIVEN GATHER, THAT GATHER WILL NOT
C           BE OUTPUT.
C           PRESET=1    e.g. mintrs 24
C
C  WRITTEN AND COPYRIGHTED BY:
C   PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, MARCH 1980
C
C
C   MODIFIED ON 3 MAY 1981 FOR THE RESTART CAPABILITY.  P.C.H.
c   modified on 24 May 1989 so the restart will work under Version 2.0
c     ( open the new input parameter file correctly and read/write
c      the correct items for the Version 2 process input parameters).
c   mod 3 Dec 1993.  Preset nwrds to 0 and let the execute determine the
c     length from the first GOOD trace it gets.
c   mod 20 Apr 96 - Skip the restart frp if geom isn't there and the cdp
c      number is is in the trace already (like geom was done in a 
c      different run)
c  mod 18 Nov 98 - Allow frp without process input, in case the first
c      trace read is not the first trace of the subsurface.
c  mod March 1999 - Change maxrps preset from intrcs + 5 to intrcs + 20
c  mod May 2006 - Change MAXTRS preset to 100
c
C
      PARAMETER (NPARS=7)                                               /* THE NUMBER OF USER PARAMETERS
      DIMENSION SCR(111),LSCR(111),ISCR(111)
      INTEGER*2 ISCR
      INTEGER FIS,SINC,FTR,TRINC,forgat,decimf,order
      COMMON /INPUT/ IPARUN,NLISTS
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPES(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
      DIMENSION VALS(NPARS),LVALS(NPARS)
      EQUIVALENCE (VALS(1),LVALS(1))
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      COMMON /READT/ILUN,NUMHDR,NUMDAT,I1,I2,INTRCS
      COMMON /GEOM/ IGUNIT
      COMMON /TCOL/ LFRP,LRPINC,MWRDS,IOUNIT,NAXRPS,NAXTRS,NINTRS
      INTEGER FRP,RPINC
C
C
      EQUIVALENCE (FRP,LVALS(1)),
     2            (RPINC,LVALS(2)),
     3            (NWRDS,LVALS(3)),
     4            (MAXRPS,LVALS(4)),
     5            (MAXTRS,LVALS(5)),
     6            (MINTRS,LVALS(6)),
     7            (LPRINT,LVALS(7))
      DATA NAMES/'FRP   ','RPINC ','NWRDS ','MAXRPS',
     *           'MAXTRS','MINTRS','LPRINT'/
      DATA LENGTH/3,5,5,4*6/
      DATA TYPES/7*'L'/
C
C
C       SET THE PRESETS
C
      FRP=32767
      RPINC=1
      NWRDS = 0
      MAXRPS=INTRCS+20
      MAXTRS = 100
      MINTRS=1
      NLISTS=0
      NS=0
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      CALL GETOKE(TOKEN,NCHARS)                                          /* GET A TOKEN FROM THE USER PARAMETER LINE
      CALL UPCASE(TOKEN,NCHARS)                                          /* CONVERT THE TOKEN TO UPPERCASE
      IF(NCHARS.GT.0) GO TO 150
      IF(NOW.EQ.1) PRINT 140
  140 FORMAT(' <  ENTER PARAMETERS  >')
      CALL RDLINE                                                        /* GET ANOTHER USER PARAMETER LINE
      NTOKES=0
      GO TO 100
  150 CONTINUE
      NTOKES=NTOKES+1
      DO 190 I=1,NPARS                                                  /* SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                      /* GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          /* SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          /* STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000            /* END OF PARAM LIST?
      IF(NS.NE.0) GO TO 230
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** GATHER DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A10)
      IERROR=IERROR+1
      GO TO 100
C****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                           /*  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 230                                         /* END OF LINE?
      IF(NOW.EQ.1) PRINT 140                                            /* THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
      CALL RDLINE                                                        /* GET ANOTHER LINE
      NTOKES=0
      GO TO 210
  230 CONTINUE
      IF(TYPES(NPARAM).NE.'A') GO TO 240
      IF(NAMES(NPARAM).EQ.'ADDWB'.AND.TOKEN(1:NCHARS).EQ.'YES')
     *    IADDWB=1
      GO TO 100
  240 CONTINUE
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              /* TRY AND DECODE IT
      IF(ISTAT.EQ.2) GO TO 420                                          /* =2 MEANS IT IS A NUMERIC
      IERROR=IERROR+1                                                    /* DCODE PRINTED AN ERROR
      GO TO 100
  420 IF(TYPES(NPARAM).EQ.'L') GO TO 500
      GO TO 100
  500 CONTINUE                                                          /*  32 BIT INTEGER VALUES
      LVALS(NPARAM)=AREAL
      GO TO 100
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
      LFRP=FRP                                                           /* FORTRAN DOESN'T ALLOW EQUIVALENCING TO COMMON!!
      LRPINC=RPINC
      MWRDS=NWRDS
      NAXRPS=MAXRPS
      NAXTRS=MAXTRS
      NINTRS=MINTRS
C****
      NS=0
 2020 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2030                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2020
 2030 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
      RETURN                                                            !  FINISHED ALL OF THE PARAMETERS!!!
      END
