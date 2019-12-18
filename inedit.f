       SUBROUTINE INEDIT(BUF1,LBUF1,IBUF,ISCR)
c                                                                     
C                             PROCESS INPUT (READS MAGNETIC TAPE)
C                             ------- -----
C
C  DOCUMENT DATE:
C
C     PROCESS INPUT READS SEISMIC DATA FROM MAGNETIC TAPE.  THE DATA MUST BE IN
C  SEGY FORMAT.  INPUT IS TRACE ORIENTED RATHER THAN SHOT OR RP ORIENTED AND
C  FEW LOGICAL BOUNDARIES EXIST.  PROCESS INPUT READS A SINGLE TRACE FROM TAPE
C  AND IMMEDIATELY PASSES IT ON TO THE NEXT PROCESS IN THE PROC LIST.
C      THE DATA MAY BE READ FROM TAPES USING ONE OF THREE DIFFERENT CONTROLS.
C  FIELD TAPES MAY BE CONTROLLED BY EITHER SHOT NUMBERS OR GMT TIME.  TAPES THAT
C  HAVE BEEN PROCESSED THROUGH PROCESS GATHER MUST BE CONTROLLED BY RP NUMBERS
C  STACKED TAPES MAY BE READ USING EITHER RP NUMBERS OR BY GMT TIME.
C      INPUT REEL CHANGES ARE HANDLED BY PROCESS INPUT THROUGH OPERATOR
C  INTEVENTION WHEN EITHER END OF TAPE IS REACHED (A FILE MARK) OR WHEN THE
C  USER SPECIFIES A NEW REEL NUMBER VIA THE IREELN PARAMETER.  OPERATOR REEL
C  CHANGES ARE ACCOMPLISHED BY THE COMPUTER OPERATOR SPECIFYING THE NEW TAPE
C  UNIT NUMBER VIA FILE 'IN'.  THE FILE 'IN' MUST BE IN THE SAME DIRECTORY
c  THAT THE JOB IS RUNNING IN.  A -1 UNIT NUMBER TERMINATES THE JOB.
c      SIOSEIS allows the user to break" the SEGY standard by putting more
c  than one file on a tape (some Exabyte tapes hold 5GB of data!).  In order
c  to read multiple files, INPUT parameter NFILES MUST be used.  Remember 
c  that tapes are terminated by a file mark; in order to read multiple
c  tapes NFILES must be used.
C      SIOSEIS ASSUMES THAT SHOT NUMBERS INCREASE ON THE TAPE unless parameter
c  order is used.  IF THE SHOT NUMBERS DO NOT INCREASE, OR IF THE SHOTS ARE
c  NOT TO BE PROCESSED IN THE ORDER THEY ARE ON TAPE, MULTIPLE PARAMETER LIST
c  MAY BE USED.  E.G. IF THE SHOTS ON TAPE ARE ORDERED 10001 TO 11000 FOLLOWED
c  BY SHOTS 500 TO 600, USE EITHER FIS 99999     OR USE TWO LISTS - 
c                                  FIS 10001 LIS 11000 END
C                                  FIS 500 LIS 600 END
C
C  THE PARAMETER DICTIONARY
C  --- --------- ----------
C
C  REQUIRED PARAMETERS
C  -------- ----------
C  IUNIT  - THE TAPE UNIT NUMBER OF THE FIRST INPUT REEL.  Tape units may be
c           changed by changing iunit in a new fno/lno list or by the above 
c           method (file in) when a file mark is detected on the SEGY input tape.
c           Unix tape assignment is /dev/nrstIUNIT. e.g. iunit 0 is /dev/nrst0.
c           VMS tape assignments are:
c               0<= LUN < 10  tape drive MSA, 10<= LUN < 20  tape drive MSB
c              20<= LUN < 30  tape drive MTA, 30<= LUN < 40  tape drive MTB
c              40<= LUN < 50  tape drives MUA, 50<= LUN < 60  tape drives MUB
c              60<= lun < 70  tape drives XTA, 70<= lun < 80  tape drives XTB
C           Preset 0.        E.G. IUNIT 1
c  DEVICE - The UNIX tape device name to read from.  When given, the 
c           Fortran magnetic tape device independent driver is used to 
c           read the specified device.  The "no rewind" device should be used.
c           Preset = none       e.g.  device  /dev/nrst8
C
C  PARAMETERS FOR PROCESSING BY SHOT
C  ---------- --- ---------- -- ----
C  FIS    - THE FIRST INPUT SHOT NUMBER OF A SET OF SHOTS (OF A FIS-LIS-END LIST).
C           A NEGATIVE FIS MEANS THAT THE FIRST SHOT OF THE SET IS IN A REVERSE
C           OR BACKWARDS DIRECTION ON TAPE FROM THE PREVIOUS SHOT INPUT.
c           The shots will be read from tape in the order specified by:
c           FIS to LIS in increments of SINC.  E.G. fis 1 lis 5 means
c           that the shots must be ordered 1, 2, 3, 4, 5 on tape.  If a
c           shot within FIS - LIS is missing, the program will continue
c           to search for the missing shot and will stop at the end of 
c           tape without finding the shot.
C           A FIS OF 99999 WILL CAUSE THE PROGRAM TO READ ALL SHOTS ON THE
C           INPUT TAPE REGARDLESS OF SHOT NUMBER (THE SHOT NUMBERS DO NOT
C           HAVE TO BE CONSECUTIVE).  USE OF FIS 99999 CAUSES LIS TO BE
C           IGNORED, THUS THE ONLY WAY THE JOB CAN BE STOPPED IS AT THE
C           REEL CHANGE WHEN THE OPERATOR RESPONDS WITH A -1 UNIT NUMBER.
C           PRESET = 99999.  E.G. FIS 41
C  LIS    - THE LAST INPUT SHOT NUMBER OF A SET OF SHOTS (OF A LIST).
C           PRESET = 32767.  E.G. LIS 201
C  NIS    - THE NUMBER OF INPUT SHOTS TO READ WHEN FIS = 99999.
C           PRESET = 32767  E.G. NIS 5
C  SINC   - THE SHOT INCREMENT BETWEEN THE FIRST SHOT (IABS(FIS)) AND THE LAST
C           SHOT (LIS) OF A LIST.  ALTERNATE SHOTS MAY BE PROCESSED BY USING
C           SINC 2.  A NEGATIVE SINC MUST BE USED IF SHOT LIS IS BEFORE
C           SHOT FIS ON TAPE.
C           PRESET = 1.  E.G. SINC 2
C  FTR    - THE FIRST TRACE OF EACH SHOT TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LESS THAN FTR WILL BE IGNORED.  An FTR of 99999
c           indicates that all traces will be read regardless of trace
c           number or trace order.
C           PRESET = 1.  E.G. FTR 23
C  LTR    - THE LAST TRACE OF EACH SHOT TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LARGER THAN LTR WILL BE IGNORED.
C           PRESET = TAPE HEADER.  E.G. LTR 23
C  TRINC  - THE INCREMENT BETWEEN TRACES FTR AND LTR.  ALTERNATE TRACES MAY
C           BE PROCESSED BY USING TRINC 2.
C           PRESET = 1.  E.G. TRINC 24
C
C  PARAMETERS FOR PROCESSING BY RP (CDP GATHERS)
C  ---------- --- ---------- -- -- ---- --------
C  FRP    - THE FIRST INPUT RP NUMBER OF A SET OF RPS (OF A FRP-LRP-END LIST).
C           A NEGATIVE FRP MEANS THAT THE FIRST RP OF THE SET IS IN A REVERSE
C           OR BACKWARDS DIRECTION ON TAPE FROM THE PREVIOUS RP INPUT.
C           PRESET = 99999.  E.G. FRP 41
C  LRP    - THE LAST INPUT RP NUMBER OF A SET OF RPS (OF A LIST).
C           PRESET = 32767.  E.G. LRP 201
C  RPINC  - THE RP INCREMENT BETWEEN THE FIRST RP (IABS(FRP)) AND THE LAST
C           RP (LRP) OF A LIST.  ALTERNATE RPS MAY BE PROCESSED BY USING
C           RPINC 2.  A NEGATIVE RPINC MUST BE USED IF RP LRP IS IN FRONT OF
C           RP FRP
C            PRESET = 1.  E.G. rpinc 2
C  FTR    - THE FIRST TRACE OF EACH RP TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LESS THAN FTR WILL BE IGNORED.  An FTR of 99999
c           indicates that all traces will be read regardless of trace
c           number or trace order.
C            PRESET = 1.  E.G. FTR 23
C  LTR    - THE LAST TRACE OF EACH RP TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LARGER THAN LTR WILL BE IGNORED.
C            PRESET = TAPE HEADER.  E.G. LTR 23
C  TRINC  - THE INCREMENT BETWEEN TRACES FTR AND LTR.  ALTERNATE TRACES MAY
C           BE PROCESSED BY USING TRINC 2.
C            PRESET = 1.  E.G. TRINC 24
C
C  PARAMETERS FOR PROCESSING BY TIME (GMT)
C  ---------- --- ---------- -- ---- -----
C  ALL SHOTS PRIOR TO FDAY, FGMT ARE IGNORED.  ALL SHOTS ARE THEN PROCESSED
C  AS THEY ARE READ FROM TAPE UNTIL LDAY, LGMT IS EXCEEDED.
C
C  FDAY   - THE FIRST JULIAN DAY OF THE DATA TO BE READ FROM TAPE.
C           PRESET=0.  E.G. FDAY 364
C  LDAY   - THE LAST JULIAN DAY OF THE DATA TO BE PROCESSED.
C           PRESET=FDAY
C  FGMT   - THE FIRST TIME OF DAY FDAY OF THE DATA TO BE READ FROM TAPE.
C           GMT IS EXPRESSED AS HHMM OR HOURS AND MINUTES OF THE 24 HOUR
C           CLOCK (0000-2359).
C           PRESET=0.  E.G. FGMT 1400
C  LGMT   - THE LAST TIME OF LDAY TO BE READ FROM TAPE.
C           PRESET=0.  E.G. 0605
C  FTR    - THE FIRST TRACE OF EACH SHOT TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LESS THAN FTR WILL BE IGNORED.  An FTR of 99999
c           indicates that all traces will be read regardless of trace
c           number or trace order.
C           PRESET = 1.  E.G. FTR 2
C  LTR    - THE LAST TRACE OF EACH SHOT TO PROCESS.  A TRACE WITH A TRACE
C           NUMBER ON TAPE LARGER THAN LTR WILL BE IGNORED.
C           PRESET = ALL TRACES.  E.G. LTR 1
C  TRINC  - THE INCREMENT BETWEEN TRACES FTR AND LTR.  ALTERNATE TRACES MAY
C           BE PROCESSED BY USING TRINC 2.
C           PRESET = 1.  E.G. TRINC 24
C
C  USEFUL OPTIONAL PARAMETERS
C  ------ -------- ----------
c  FNO    - First shot/rp number. FNO may be used instead of FIS or FRP.
c           Ordinarily SIOSEIS insists that the users know whether the
c           input tape is sorted by shot or rp and will not run if the 
c           tape is described incorrectly.  The use of FNO and LNO eases
c           this!  As with FIS and LIS, FNO 99999 reads all traces from 
c           tape regardless of their numbers and will stop reading only
c           when a negative tape unit number is given in file in at the
c           end of a tape (see tape change).
c           Preset = none          e.g.   fno 101
c  LNO    - Last shot/rp to read.
c           Preset = 32768          e.g.  lno 200
c  NOINC  - The increment between FNO and LNO.  The shots/rps on tape
c           will be read in the order FNO to LNO in increments of NOINC.
c           IF a shot/rp is missing, PROCESS INPUT will continue searching
c           until it finds it.  E.G.  FNO 1 LNO 5 but the shot/rps on
c           tape are 1, 2, 4, 5  then SIOSEIS will search the entire
c           tape for shot 3, stopping when it finds end of tape, and
c           never reading shots 4 or 5.  Use FNO 99999 when this occurs.
C  RENUM  - RENUMBERS EVERY SHOT/RP SO THAT THE NUMBERS ARE MONOTONICALLY
C           INCREASING BY 1, STARTING WITH THE NUMBER SPECIFIED BY RENUM.
C           RENUMBERING THE SHOT/RP NUMBERS IS HELPFUL WHEN THE INPUT SHOT/RP
C           NUMBERS ARE NONUNIQUE AND SOME PARAMETER NEEDS TO BE SPATIALLY
C           VARIED.  THE RENUMBERING IS DONE AFTER THE SHOT/RP IS READ.
C           DEFAULT = not given. E.G. RENUM 1
C  C      - COMMENT CARD IMAGES TO REPLACE IN THE TAPE HEADER.  THE COMMENT
C           CARD MUST START WITH THE LETTER C AND MUST BE FOLLOWED IMMEDIATELY
C           BY A 2 DIGIT NUMBER, FOLLOWED BY A BLANK.  THE NUMBER IS THE CARD
C           NUMBER WITHIN THE HEADER TO REPLACE.  THE COMMENT ITSELF MUST BE
C           ENCLOSED IN SINGLE QUOTES.
C           PRESET=NONE. EXAMPLE C15 'THIS IS AN EXAMPLE OF A COMMENT'
c  STIME  - Start time of the data to process.  Data prior to STIME will be
c           discarded and the deep water delay will be set to STIME.  If
c           STIME is less than the deep water delay, STIME will be ignored.
c           Preset = delay of each trace     e.g.   stime 3.0
C  SECS   - THE NUMBER OF SECONDS OF DATA TO PROCESS.  SECS IS HELD CONSTANT
C           FOR ALL TRACES WITHIN THE JOB.  THE TOTAL TIME LENGTH OF EACH TRACE
C           IS THE SUM OF THE DELAY AND SECS.  If the data is not SECS
c           long, zeroes are NOT added or padded and the trace will be
c           less tha SECS long.
C           PRESET= ALL THE DATA ON THE TRACE.
c  SET    - The start and end times of the data to read from disk.  SET
c           is a pair of times in seconds.  The use of SET causes the
c           deep water delay and the number of samples to be changed.
c           If either set is outside of the data, the data is padded
c           with zeroes.  The data will always be SET(2) - SET(1) long.
c           Preset = none     e.g.   set 2.0 3.0
C  IREELN - THE INPUT REELN NUMBER.  THIS IS USEFUL WHEN THE INPUT TAPE SHOULD
C           BE CHANGED AND THE PREVIOUS TAPE IS NOT AT THE END OF TAPE (2 FILE
C           MARKS).  E.G.  FIS 1 LIS 10 IREELN 1 END
C                          FIS 11 LIS 20 IREELN 2 END
C           CAUSES A REEL CHANGE TO OCCUR AFTER SHOT 10 IS READ.  THE NORMAL
C           REELN CHANGE PROCEDURES SHOULD BE FOLLOWED (USING FILE IN).
C  IFMT   - THE INPUT TAPE FORMAT.  USED TO OVERRIDE THE FORMAT INDICATOR ON THE
C           TAPE ITSELF.
C         =0, USE THE VALUE FROM THE TAPE.
C         =1, IBM 360 32 BIT HEX BASED FLOATING POINT.
C         =2, 32 BIT INTEGER.
C         =3, 16 BIT INTEGER.
C         =4, 16 BIT FIXED POINT WITH GAIN CODES. (not really SEGY
C         >4, HOST BIT FLOATING POINT
C           PRESET=0
C  SI     - THE INPUT SAMPLE INTERVAL IN SECONDS.  USED TO OVERRIDE THE
C           SAMPLE INTERVAL CONTAINED IN THE TRACE HEADER.
C           PRESET=TAPE
C  DELAY  - THE INPUT DEEP WATER DELAY IN SECONDS.  USED TO OVERRIDE THE
C           DELAY CONTAINED IN THE TRACE HEADER.
C           PRESET=TAPE
C  NTRCS  - THE NUMBER OF TRACES PER INPUT RECORD (SHOT OR RP).  USED TO
C           OVERRIDE THE NUMBER OF TRACES CONTAINED IN THE BINARY TAPE HEADER.
C           PRESET=TAPE HEADER.
C  NTRGAT >0, NUMBER OF TRACES PER GATHER.  NTRGAT CONVERTS A SHOT SORTED TAPE
C             INTO A RP SORTED TAPE, OR CONVERTS A FOREIGN RP SORTED TAPE INTO
C             A SIOSEIS RP SORTED TAPE.  EACH GATHER WILL CONTAIN NTRGAT TRACES.
C             THE RP NUMBERS WILL BE THE SAME AS THE SHOT NUMBERS.  EVERY
C             NTRGAT TRACES WILL BE FLAGGED AS THE END OF GATHER, SO PROCESS
C             STACK WILL STACK NTRGAT TRACES.  THIS PARAMETER IS USEFUL FOR
C             PROCESSING TAPES THAT HAVE BEEN GATHERED OUTSIDE OF SIOSEIS.  THIS
C             IS ALSO USEFUL FOR CONVERTING SINGLE CHANNEL SHOTS INTO "ZERO-
C             OFFSET" STACKED DATA (NEEDED BY MIGRATION IN DIGICON'S DISCO SYSTEM).
c          <0, CONVERTS TAPES SORTED BY RP (IF THE RP HAS A NON ZERO TRACE
C              NUMBER) TO A TAPE SORTED BY SHOT (RP TRACE NUMBER (SEGY TRACE
C              HEADER WORD 7) TO ZERO.
C           PRESET=0.  E.G.  NTRGAT 1
c  FORGAT - Foreign gather tape switch.  The use of FORGAT indicates that the
c           input gather tape was not generated by SIOSEIS and thus does not
c           have the end-of-gather convention used by SIOSEIS (a -1 in SEGY
c           header word 51).  FORGAT is similar to NTRGAT but allows each
c           gather to have a different number of traces.  FORGAT causes process
c           INPUT to wait for the next trace to be read from tape and sets the
c           end-of-gather flag to -1 if the next gather number is different,
c           or the next trace number is greater than LTR.
c           End of gather will also be set if the parameter LTR is used and
c           the trace on tape is equal to LTR.
c           The value of forgat indicates the number of rps to concatenate
c           into a single gather which is terminated by the end-of-gather
c           flag.  The penalty for this is significantly slower throughput on 
c           non-Unix computers (computers that allow asyncronus or overlapped I/O).
c           LDGO gather tapes start with the largest trace number first, which
c           breaks the SIOSEIS monotonicly increasing assumption; LDGO gather
c           tapes may be read using forgat 1 and ftr 99999, in which case
c           SIOSEIS will use all traces within the gather.
c           Preset = 0       e.g. forgat 1
C  DECIMF - DECIMATION FACTOR.  THE DATA WILL BE DECIMATED OR RESAMPLED BY A
C           FACTOR OF DECIMF, THUS REDUCING THE NUMBER OF SAMPLES IN THE TRACE.
C           **  NOTE **  NO ANTI-ALIAS FILTER IS APPLIED PRIOR TO DECIMATION.
C           PRESET=1.  E.G. DECIMF 2   TAKES EVERY SECOND SAMPLE
c  NFSKIP - The number of files to skip before reading the SEGY tape header.
c           Some places (LDGO) sometimes put multiple lines or logical tapes
c           on a single physical tape, a violation of the SEGY standard!
c           Preset = 0.   e.g.   nfskip 1        skips 1 file
c  REWIND - Rewinds the input tape PRIOR to reading the first shot (or
c           doing tape positioning with NFSKIP).  REWIND is set to 1,
c           or TRUE, after the first tape so that subsequent tapes
c           will be rewound when using the "operator" tape change
c           method (file "in").  Likewise, REWIND is set to 1 (TRUE)
c           on every INPUT list (a list is terminated with "END").
c         = 1, (TRUE), rewind.
c         = 0, (FALSE), NO rewind.
c           DEFAULT = 1,      e.g.  rewind 0    # don't rewind
c  NRSKIP - The number of tape records (traces) to skip before reading 
c           the first trace.  This is useful when 2 or more SEGY files
c           are concatenated into 1 file on tape.
c         >0, the record skip is done AFTER the SEGY tape header is read.
c         <0, the record skip is done BEFORE the SEGY tape header is read.
c           Preset = 0.   e.g.   nrskip 3        skips 3 records
c  ORDER  - SIOSEIS normally assumes that the shot/rp numbers are in assending
c           order on tape.  In order to reverse this assumption, set order
c           to -1, which will then force process INPUT to assume that the
c           shot/rp numbers are in decreasing order on tape.  The parameter
c           sinc/rpinc/noinc usually needs to be a negative number when
c           order -1 is given.  e.g. If the order of rps on tape is 9100,
c           9099, 9098, 9097,...  the following parameters would work:
c           frp 9100 lrp 9097 rpinc -1  
c           Preset = 0      e.g. order -1
c  NFILES - The number of files to use.  The tape files must be
c           consecutive on tape.  A filemark on tape is the end of file
c           indicator.  Two consecutive file marks are considered end of
c           information and will cause input to rewind the tape and
c           expect a tape change using the normal SIOSEIS tape change
c           procedures (by creating a file named in).  When used with
c           parameter DEVICE, the "no rewind" device MUST be used.
c           PRESET = 1       e.g.  nfiles 2
C
C  WRITTEN AND COPYRIGHTED (C) BY:
C  PAUL HENKART, SCRIPPS INSTITUTION OF OCEANOGRAPHY, JANUARY 1980
C  ALL RIGHTS ARE RESERVED BY THE AUTHOR.  PERMISSION TO COPY OR REPRODUCE THIS
C  SUBROUTINE, BY COMPUTER OR OTHER MEANS, MAY BE OBTAINED ONLY FROM THE AUTHOR.
C
C
C  ARGUMENTS:
C  BUF1   - AN ARRAY THAT WILL RECEIVE THE FIRST TRACE OF THE JOB WITH THE
C           TRACE HEADER.
C  LBUF1  - THE SAME ARRAY BUT IN LONG INTEGER SINCE ARGUMENTS CAN'T BE
C           EQUIVALENCED.
C  IBUF   - THE SAME ARRAY BUT IN SHORT INTEGER SINCE ARGUMENTS CAN'T BE
C           EQUIVALENCED.
C  ISCR   - AN ARRAY TO BE USED AS SCRATCH - IE. THE CONTENTS WILL BE DESTROYED.
C
c   mod 1 Mar 91 to add nrskip parameter
c   mod 5 Mar 91 to add stime parameter
c   mod 24 July 91 to add nfiles parameter
c   mod 27 Aug 91 to add device
c   mod 17 Nov 91 change nfiles preset to 99999
c   mod 11 Feb 92 to allow fis/lis on rp sorted tapes and clean up
c           fis/lrp. frp/lrp, fno/lno presets and error messages.
c   mod 5 Oct 92 to add parameter set
c   mod 22 Oct 92 to allow renum to change from list to list.
c   mod 24 Nov 92 change nfiles preset to 1
c   mod 10 Jan 94 - reject nfiles to be used with the device parameter.
c   mod 9 June 94 - Don't reject nfiles and device if norewind device.
c   mod 22 Jun 94 - set iunit = 1 when DEVICE is given
c   mod 20 Mar 95 - Add no =   and tr = 
c   mod 29 Mar 95 - Change fis/frp/fno preset to 99999
c   mod 29 Mar 95 - Add parameters allno and alltr
c   mod 15 aug 95 - Allow iunit up to 40
c   mod 13 Nov 95 - Add parameter REWIND.
c   mod 7 Dec 95 - Do some editing on NO and TR parameters.
c   mod 18 Mar 96 - allno 0 and alltr 0 didn't work
c   mod 1 Oct 97 - Use upcase for  allno, alltr, rewind etc 
c   mod mar 99 - Change allno to no if there's an increment between shots etc
c              - Likewise for trinc - set alltr to 0 if trinc is given
c   mod 31 Jul 00 - mar 99 change was bad because sinc preset is 1 not 0!
c   mod 16 May 01 - Allow iunit less than 100 (was 40).
c   mod 31 Jul 02 - Add warning if DEVICE is not 'bn'
c   mod 8 May 03 - The above isn't true on SGI
c   mod 26 Jan 04 - Change nfiles preset to 0 so that nfiles terminates
c                   without the tape change question and file in.
c   mod 17 May 10 - Comment out creating real mute times in header words 47 & 48
c
      PARAMETER (NPARS = 44 )                                           ! THE NUMBER OF PARAMETERS AVAILABLE TO THE USER
      INTEGER FIS,SINC,FRP,RPINC,FTR,TRINC,RENUM,NIS,fno,forgat
      INTEGER allno, alltr, rewind, rewindi
      CHARACTER*3 BYPASS
      CHARACTER*4 LTYPE
      COMMON/INPUT/ IPARUN ,NLISTS
      COMMON /porder/ num, iorder(40)
      LOGICAL FIRST,CFIRST
      COMMON /READT/ILUN,NUMHDR,NUMDAT,IUNHDR,IREELM,INTRCS,JFMT,NSKIP,
     *    SSECS,idummy,ISRCF,IDTYPE,nfskp,jform,itxsi,itxdel,
     &    nfktrc, norigtr, nrskp, nfile, rewindi
      COMMON /SIOAP/ IASGND,IRELSE,IN,IOUT,NEXTAD,LAPSIZ
      INTEGER FDAY,LDAY,FGMT,LGMT,DECIMF,order
      CHARACTER*6 NAMES(NPARS)
      CHARACTER*1 TYPE(NPARS)
      DIMENSION LENGTH(NPARS)
      CHARACTER*80 TOKEN
c****  device is not easy.  Put it in common so rdtrc has access to it.  Use a
c****  separate common because of the word boundary problem with characters.
c****  Write it to disk too, so that it can change from list to list
      COMMON /inputdev/inputdev
      CHARACTER*80 inputdev
      DIMENSION VALS(NPARS),LVALS(NPARS)
      COMMON /EDITS/ IERROR,IWARN,IRUN,NOW,ICOMPT
      CHARACTER*80 CHEADR(40),COMMNT(40)
      DIMENSION BUF1(1000),LBUF1(1000), set(2)
      INTEGER*2 IBUF(1000),ISCR(1000)
C
C
      EQUIVALENCE (IREELN,LVALS(1)),
     2            (IUNIT,LVALS(2)),
     3            (BYPASS,LVALS(3)),
     4            (FIS,LVALS(4)),
     5            (LIS,LVALS(5)),
     6            (SINC,LVALS(6)),
     7            (FTR,LVALS(7)),
     8            (LTR,LVALS(8)),
     9            (TRINC,LVALS(9)),
     *            (FRP,LVALS(10)),
     1            (LRP,LVALS(11)),
     2            (RPINC,LVALS(12)),
     3            (LPRINT,LVALS(13)),
     4            (SECS,VALS(14)),
     5            (IFMT,LVALS(15)),
     6            (SI,VALS(16)),
     7            (DELAY,VALS(17)),
     8            (NTRCS,LVALS(18)),
     9            (NTRGAT,LVALS(19)),
     *            (RENUM,LVALS(20))
      EQUIVALENCE (FDAY,LVALS(21)),                                     ! F77 ALLOW A MAX OF 19 CONTINUATION LINES!!
     2            (LDAY,LVALS(22)),
     3            (FGMT,LVALS(23)),
     4            (LGMT,LVALS(24)),
     5            (DECIMF,LVALS(25)),
     6            (NIS,LVALS(26)),
     7            (nfskip,lvals(27)),
     8            (fno,lvals(28)),
     9            (lno,lvals(29)),
     *            (forgat,lvals(30)),
     1            (noinc,lvals(31)),
     2            (order, lvals(32)),
     3            (nrskip, lvals(33)),
     4            (stime, vals(34)),
     5            (nfiles,lvals(35)),
     6            (device,lvals(36)),
     7            (allno,lvals(42)),
     8            (alltr,lvals(43)),
     9            (rewind,lvals(44))
      DATA NAMES/'IREELN','IUNIT ','BYPASS','FIS   ','LIS   ','SINC  ',
     1  'FTR   ','LTR   ','TRINC ','FRP   ','LRP   ','RPINC ','LPRINT',
     2  'SECS  ','IFMT  ','SI    ','DELAY ','NTRCS ','NTRGAT','RENUM ',
     3  'FDAY  ','LDAY  ','FGMT  ','LGMT  ','DECIMF','NIS   ','NFSKIP',
     4  'FNO   ','LNO   ','FORGAT','NOINC ','ORDER ','NRSKIP','STIME ',
     5  'NFILES','DEVICE','SET   ','NO    ','NO=   ','TR    ','TR=   ',
     6  'ALLNO ','ALLTR ','REWIND' /
      DATA LENGTH/6,5,6,3,3,4,3,3,5,3,3,5,6,4,4,2,5,5,6,5,4,4,4,4,6,3,
     *    6,3,3,6,5,5,6,5,6,6,3,2,3,2,3,5,5,6/
      DATA TYPE/'L','L','A',10*'L','F','L','F','F',16*'L','F','L','A',
     &        'F','A','F','A','F',2*'A','A'/
      DATA FIRST /.TRUE./, CFIRST /.TRUE./
      DATA COMMNT/40*' '/
C****
C****   PRESETS
C****
      IUNIT = 0
      BYPASS='NO'
      FIS=-32767
      LIS = 32767
      SINC=1
      FTR=1
      LTR=-32767
      TRINC=1
      FRP=-32767
      LRP = 32767
      RPINC=1
      LPRINT=0
      SECS=0.
      IFMT=0
      SI=0.
      DELAY=-32.
      NTRCS=0
      NTRGAT=0
      RENUM=-32767
      DECIMF=1
      FDAY=0
      LDAY=0
      FGMT=-1                                                           ! ALLOW A GMT OF 0000
      LGMT=-1
      NIS=-32767
      TIME1=0.
      TIME2=0.
      IREELN=0
      NUMDAT=0                                                          ! SOMEONE HAS TO SET IT!
      nskip = 0
      nfskip = 0
      nrskip = 0
      fno = -32767
      lno = 32767
      forgat = 0
      noinc = 1
      order = 1
      stime = -1
      nfiles = 0
      inputdev = ' '
      iread = 0                                                         ! is input in the procs list?
      set(1) = 0.
      set(2) = 0.
      notype = 0
      noindex = 0
      itrtype = 0
      itrindex = 0
      allno = 1
      alltr = 1
      rewind = 1
      DO 10 i = 1, num
         IF( iorder(i) .EQ. 2 ) iread = 1
   10 CONTINUE
C****
C****   SET UP A DISC FILE TO HOLD THE PARAMETERS
C****
      CALL GETFIL(2,IPARUN,token,ISTAT)                                 ! GET A FREE PRIMOS FILE UNIT NUMBER
      IF(ICOMPT.EQ.1) IPARUN=IPARUN+12                                  ! PRIME COMPUTERS HAVE FILE UNIT IDIOSYNCRASIES
      OPEN( UNIT=iparun, STATUS='SCRATCH', FORM='UNFORMATTED' )
C****
C****   THE CURRENT COMMAND LINE IN THE SYSTEM BUFFER MAY HAVE THE PARAMETERS.
C****   GET A PARAMETER LIST FROM THE USER.
C****
      NTOKES=1
  100 CONTINUE
      ns = 0 
      CALL GETOKE(TOKEN,NCHARS)                                         ! GET A TOKEN FROM THE USER PARAMTER LINE
      CALL UPCASE(TOKEN,NCHARS)
      IF( NCHARS .LE. 0 ) THEN
          IF(NOW.EQ.1) PRINT 140
  140        FORMAT(' <  ENTER PARAMETERS  >')
          CALL RDLINE                                                   ! GET ANOTHER USER PARAMETER LINE
          NTOKES=0
          GO TO 100
      ENDIF
      NTOKES=NTOKES+1
  150 DO 190 I=1,NPARS                                                  ! SEE IF IT IS A PARAMETER NAME
      LEN=LENGTH(I)                                                     ! GET THE LEGAL PARAMETER NAME LENGTH
      IPARAM=I                                                          ! SAVE THE INDEX
      IF(TOKEN(1:NCHARS).EQ.NAMES(I)(1:LEN).AND.NCHARS.EQ.LEN) GO TO 200
  190 CONTINUE                                                          ! STILL LOOKING FOR THE NAME
      IF(TOKEN(1:NCHARS).EQ.'END'.AND.NCHARS.EQ.3) GO TO 1000           ! END OF PARAM LIST?
      IF( TOKEN(1:1) .EQ. 'C' .AND .NCHARS .EQ. 3 ) THEN                ! IS IT A COMMENT CARD
          CFIRST = .FALSE.
          READ( TOKEN, '(1X,I2)')  ITEMP
          COMMNT(ITEMP)(1:4)=TOKEN(1:NCHARS)
          CALL GETOKE(TOKEN,NCHARS)                                     ! GET THE COMMENT (WITHIN THE QUOTES)
          COMMNT(ITEMP) (5:80) = TOKEN(1:NCHARS)
          GO TO 100
      ENDIF
      PRINT 191, TOKEN(1:NCHARS)
  191 FORMAT(' ***  ERROR  *** INPUT DOES NOT HAVE A PARAMETER ',
     *  'NAMED ',A)
      IERROR=IERROR+1
      GO TO 100
****
C****    FOUND THE PARAMETER NAME, NOW FIND THE VALUE
C****
  200 CONTINUE
      NPARAM=IPARAM
  210 CONTINUE                                                          !  NOW FIND THE VALUE
      CALL GETOKE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF( NCHARS .LE. 0 ) THEN                                          ! END OF LINE?
          IF( NOW .EQ. 1 ) PRINT 140                                    ! THIS ALLOWS A PARAMETER TO BE ON A DIFFERENT LINE FROM THE NAME
          CALL RDLINE                                                   ! GET ANOTHER LINE
          NTOKES=0
          GO TO 210
      ENDIF
      IF( TYPE(NPARAM) .EQ. 'A' ) THEN
          IF(NPARAM.EQ.3) BYPASS=TOKEN(1:NCHARS)
          IF( names(nparam) .EQ. 'DEVICE') THEN
              inputdev = token(1:nchars)
              iunit = 1
c****  sgi needs    nrns  (no-rewind, no-swap)
              IF( token(nchars-3:nchars) .EQ. 'nrns' ) GOTO 100
c****  sun needs   bn
              DO i = 1, nchars-1
                 IF( token(i:i+1) .EQ. 'bn' ) GOTO 100
              ENDDO
              PRINT *,
     &    ' ***  WARNING  ***  DEVICE should be BSD and No Rewind (bn).'
              iwarn = iwarn + 1
              GOTO 100
          ENDIF
          IF( names(nparam) .EQ. 'NO' ) THEN
              CALL upcase( token, nchars)
              IF( token(1:1) .EQ. 'I' ) THEN
                  notype = 1
              ELSEIF( token(1:1) .EQ. 'L' ) THEN
                  notype = 2
              ELSEIF( token(1:1) .EQ. 'R' ) THEN
                  notype = 3
              ELSE
                  PRINT *,' ***  ERROR  ***  NO value must start with ',
     &                  ' I or L or R.'
                  ierror = ierror + 1
              ENDIF
              token(1:nchars) = token(2:nchars)
              nchars = nchars - 1
              CALL dcode( token, nchars, areal, istat )
              IF( istat .NE. 2 ) THEN
                  ierror = ierror + 1
                  GOTO 100
              ENDIF
              noindex = NINT(areal)
          ENDIF
          IF( names(nparam) .EQ. 'TR' ) THEN
              CALL upcase( token, nchars)
              IF( token(1:1) .EQ. 'I' ) THEN
                  itrtype = 1
              ELSEIF( token(1:1) .EQ. 'L' ) THEN
                  itrtype = 2
              ELSEIF( token(1:1) .EQ. 'R' ) THEN
                  itrtype = 3
               ELSE
                  PRINT *,' ***  ERROR  ***  TR value must start with ',
     &                  ' I or L or R.'
                  ierror = ierror + 1
              ENDIF
              token(1:nchars) = token(2:nchars)
              nchars = nchars - 1
              CALL dcode( token, nchars, areal, istat )
              IF( istat .NE. 2 ) THEN
                  ierror = ierror + 1
                  GOTO 100
              ENDIF
              itrindex = NINT(areal)
          ENDIF
          IF( names(nparam) .EQ. 'ALLNO' ) THEN
              CALL upcase( token, nchars)
              IF( token(1:1) .EQ. 'Y' ) allno = 1
              IF( token(1:1) .EQ. 'N' ) allno = 0
          ENDIF
          IF( names(nparam) .EQ. 'ALLTR' ) THEN
              CALL upcase( token, nchars)
              IF( token(1:1) .EQ. 'Y' ) alltr = 1
              IF( token(1:1) .EQ. 'N' ) alltr = 0
          ENDIF
          IF( names(nparam) .EQ. 'REWIND' ) THEN
              CALL upcase( token, nchars)
              IF( token(1:nchars) .EQ. 'YES' ) rewind = 1
              IF( token(1:nchars) .EQ. 'NO' ) rewind = 0
              IF( token(1:nchars) .EQ. 'ON' ) rewind = 1
              IF( token(1:nchars) .EQ. 'OFF' ) rewind = 0
              IF( token(1:nchars) .EQ. '1' ) rewind = 1
              IF( token(1:nchars) .EQ. '0' ) rewind = 0
          ENDIF
	     GOTO 100
      ENDIF
      CALL UPCASE(TOKEN,NCHARS)
      CALL DCODE(TOKEN,NCHARS,AREAL,ISTAT)                              ! TRY AND DECODE IT
      IF( ISTAT .NE. 2 ) THEN                                           ! =2 MEANS IT IS A NUMERIC
          IERROR=IERROR+1                                               ! DCODE PRINTED AN ERROR
          GO TO 100
      ENDIF
      ns = ns + 1
      IF( TYPE(NPARAM) .EQ. 'F' ) THEN
          VALS(NPARAM) = AREAL                                          !  FLOATING POINT VALUES
          IF( names(nparam) .EQ. 'SET' ) THEN
              set(ns) = areal
              IF( ns .EQ. 1 ) GOTO 200
          ENDIF
          GOTO 100
      ENDIF
      IF( type(nparam) .EQ. 'L' ) THEN                                  !  32 BIT INTEGER VALUES
          LVALS(NPARAM) = AREAL
          GO TO 100
      ENDIF
C****
C****   FINISHED A LIST, NOW DO THE ERROR AND VALIDITY CHECKS
C****
 1000 CONTINUE
      IF( icompt .NE. 4 ) THEN                                          ! VMS has real screwy tape numbers!
          IF( iunit .LT. 0 .OR. iunit .GT. 100 ) THEN
              PRINT *,' ***  ERROR  ***  Illegal IUNIT.'
              ierror = ierror + 1
          ENDIF
      ELSE
          IF( iunit .LT. 0 .OR. iunit .GT. 100 ) THEN
              PRINT *,' ***  ERROR  ***  Illegal IUNIT.'
              ierror = ierror + 1
          ENDIF
      ENDIF
      IF( noindex .LT. 0 .OR. noindex .GT. 120 ) THEN
          PRINT *,' ***  ERROR  ***  NO index is too big.', noindex
          ierror = ierror + 1
      ENDIF
      IF( itrindex .LT. 0 .OR. itrindex .GT. 120 ) THEN
          PRINT *,' ***  ERROR  ***  TR index is too big.', itrindex
          ierror = ierror + 1
      ENDIF
      IF(IRUN.EQ.0) GO TO 1040                                          ! don't use the tape unless OK
      IF( iread .EQ. 0 ) GOTO 1040
      IF( ierror .NE. 0 ) GOTO 1040
      IF( FIRST ) THEN
          FIRST=.FALSE.
          ILUN=IUNIT
          IF( LPRINT .EQ. 1 ) PRINT *,' INPUT PARAMTERS:'
          IN=0                                                          !  SET THE AP ADDRESS TO 0
          SSECS=SECS
          JFMT=IFMT
          ISRCF=DECIMF
          nfskp = nfskip
          nrskp = nrskip
          nfile = nfiles
          IF(FRP.EQ.99999.AND.NIS.GT.0) LRP=NIS
          IF(FIS.EQ.99999.AND.NIS.GT.0) LIS=NIS                         ! PRESET LIS TO NIS
          IF( iread .EQ. 0 ) GOTO 1040
          rewindi = rewind
          CALL RDTRC(BUF1,BUF1,BUF1,ISTAT,0)                            ! GET THE FIRST TRACE INTO BUF1
          CALL PODISC(IUNHDR,1,0)                                       ! REWIND THE HEADER FILE
          CALL RDDISC(IUNHDR,ISCR,800,ISTAT)                            ! GET BY THE EBCDIC HEADER (VMS CAN NOT PODISC(1,800,)T POSITION!)
          CALL RDDISC(IUNHDR,ISCR,100,ISTAT)                            ! GET THE BINARY HEADER
      ENDIF
      ISI=SI*1000000.                                                   ! CONVERT TO MICROSECONDS (AS IN THE SEGY HEADER)
      IDELAY=DELAY*1000                                                 ! CONVERT TO MILLISECONDS (AS IN THE SEGY HEADER)
      SR=REAL(IBUF(59))/1000000.                                       !  SET UP THE TRACE HEADER!!!
      BUF1(46)=IBUF(55)/1000.                                           ! THE DELAY IN SECONDS
c      BUF1(47)=IBUF(56)/1000.                                           ! THE START MUTE TIME IN SECONDS
c      BUF1(48)=IBUF(57)/1000.                                           ! THE END MUTE TIME IN SECONDS
      BUF1(49)=SR                                                       ! THE SAMPLE INTERVAL IN SECONDS
      IF(ISI.NE.0) IBUF(59)=ISI                                         ! OVERRIDE THE HEADER
      IF(IDELAY.GT.-32000) IBUF(55)=IDELAY                              ! ALLOW A NEW DELAY OF ZERO!
C****
C****   DO THE PRESETS FROM THE TAPE HEADER
C****
      IF(NTRCS.NE.0) INTRCS=NTRCS                                       ! THIS TELL RDTRC TO EXPECT NTRCS PER SHOT
      IF(NTRGAT.GT.0) ISCR(15)=2                                        ! SET THE BINARY TAPE HEADER SORT FLAG TO RP
      IF( NTRGAT.LT.0) THEN
          ISCR(15)=1
          lbuf1(7) = 0
      ENDIF
      IF( fno .NE. -32767 ) GOTO 1040
      IF( FIS .NE. -32767 .AND. LBUF1(7) .NE. 0 ) THEN                  ! FIS GIVEN ON RP SORTED TAPE
          PRINT *,' ***  WARNING  ***  FIS given on an rp sorted tape.'
          iwarn = iwarn + 1
      ENDIF
      IF( FRP .NE. -32767 .AND. LBUF1(7) .EQ. 0 ) THEN                  ! FRP GIVEN ON SHOT SORTED TAPE
          PRINT *,' ***  ERROR  ***  FRP GIVEN ON A SHOT SORTED TAPE'
          IERROR=IERROR+1
      ENDIF
      IF( FIS .EQ. -32767 .AND. frp .EQ. -32767 ) THEN
          IF( lbuf1(7) .EQ. 0 ) THEN
              fis = 99999                                               ! was lbuf1(3)
          ELSE
              frp = 99999                                               ! was lbuf1(6)
          ENDIF
      ENDIF
c      IF( LTR .EQ. -32767 .AND. alltr .EQ. 1 ) LTR = INTRCS               ! PRESET THE NUMBER OF TRACES PER SHOT
 1040 BUF1(49)=SR
      IF(FDAY+LDAY+FGMT+LGMT.LE.0) GO TO 1050                           ! ARE THE PARAMETERS BY TIME?
      IF(LDAY.EQ.0) LDAY=FDAY                                           !  PRESET LDAY TO FDAY
      IF( LDAY .LT. FDAY ) THEN
          PRINT *,' ***  ERROR  ***  BAD DAY OR GMT PARAMETERS.'
          IERROR=IERROR+1
      ENDIF
      IF(FDAY.NE.0.AND.LDAY.NE.0.AND.FGMT.NE.-1.AND.LGMT.NE.-1)
     *    GO TO 1043
          PRINT *,' ***  ERROR  ***  BAD DAY OR GMT PARAMETERS.'
          IERROR=IERROR+1
 1043 CONTINUE
      TIME1=FDAY*10000.+FGMT                                            ! PACK THE DAY AND GMT INTO 1 WORD
      TIME2=LDAY*10000.+LGMT
      IF( TIME2 .LT. TIME1) THEN
          PRINT *,' ***  ERROR  ***  BAD DAY OR GMT PARAMETERS.'
          IERROR=IERROR+1
      ENDIF
      FGMT=-1
      LGMT=-1
 1050 CONTINUE
      IF(SINC.NE.1.AND.TIME1+TIME2.NE.0.) THEN
           PRINT *,' ***  WARNING  ***  SINC WILL BE IGNORED WHEN ',
     *          'PROCESSING BY GMT.'
           IWARN=IWARN+1
      ENDIF
      IF( order .NE. 1 ) order = -1
      IF( stime .LT. 0. .AND. stime .NE. -1. ) THEN
          PRINT *,' ***  ERROR  ***  STIME must be non-negative.'
          ierror = ierror + 1
      ENDIF
C****
C****
C****
      IF(FNO.EQ.99999.AND.LNO.EQ.32767) LNO=999999
      IF(FIS.EQ.99999.AND.LIS.EQ.32767) LIS=999999
      IF(FIS.EQ.-32767.OR.LIS.EQ.32767) GO TO 1150
      IF(IABS(FIS).LE.LIS.OR.FIS.EQ.99999) GO TO 1150
      PRINT *,' ***  WARNING  ***  FIS SHOULD NORMALLY BE SMALLER THAN',
     *    ' LIS'
      IWARN=IWARN+1
 1150 CONTINUE
      IF(FRP.EQ.99999.AND.LRP.EQ.32767) LRP=999999
      IF(FRP.EQ.-32767.OR.LRP.EQ.32767) GO TO 1160
      IF(IABS(FRP).LE.LRP.OR.FRP.EQ.99999) GO TO 1160
      PRINT *,' ***  WARNING  *** FRP SHOULD NORMALLY BE SMALLER THAN',
     *   ' LRP.'
      IWARN=IWARN+1
 1160 CONTINUE
C****
C****    FINISHED EDITING, NOW WRITE THE LIST TO DISC FOR THE EXECUTION PHASE
C****
      NLISTS=NLISTS+1                                                   ! INCREMENT THE LISTS COUNTER
      IF( fis .NE. -32767 ) THEN
          istart = fis
          iend = lis
          ltype = 'SHOT'
      ENDIF
      IF( frp .NE. -32767 ) THEN
          istart = frp
          iend = lrp
          ltype = 'RP'
      ENDIF
      IF( fno .NE. -32767 ) THEN
          istart = fno
          iend = lno
          ltype = 'NONE'
      ENDIF
      IF( rpinc .NE. 1 ) sinc = rpinc
      IF( noinc .NE. 1 ) sinc = noinc
      IF( sinc .NE. 1 ) allno = 0
      IF( trinc .NE. 1 ) alltr = 0
      rewindi = rewind
      WRITE(IPARUN) IREELN,IUNIT,LTYPE,istart,iend,SINC,FTR,LTR,TRINC,
     *  ISI,IDELAY,NTRGAT,TIME1,TIME2,DECIMF,lprint,forgat,order,stime,
     &  inputdev, set, renum, notype, noindex, itrtype, itrindex,
     &  allno, alltr, rewind
      IF( IAND(LPRINT,1) .EQ. 1 ) THEN
          PRINT *, NLISTS,IREELN,IUNIT,LTYPE,istart,iend,SINC,FTR,LTR,
     *       TRINC,ISI,IDELAY,NTRGAT,TIME1,TIME2,DECIMF,forgat,order
          PRINT *, stime, set, renum, notype, noindex, itrtype, 
     &        itrindex, allno, alltr, rewind
	  PRINT *, inputdev
      ENDIF
c****
c****  set the defaults
c****
      rewind = 1
      renum = -32767
 2030 CALL GETOKE(TOKEN,NCHARS)                                         ! GET THE NEXT TOKEN
      CALL UPCASE(TOKEN,NCHARS)
      NTOKES=NTOKES+1
      IF(NCHARS.GT.0) GO TO 2040                                        ! WAS IT THE END OF A LINE?
      IF(NOW.EQ.1) PRINT 140
      CALL RDLINE                                                       ! GET ANOTHER LINE
      NTOKES=0
      GO TO 2030
 2040 IF(TOKEN(1:NCHARS).NE.'END'.OR.NCHARS.NE.3) GO TO 150
C****
C****  PUT THE TAPE HEADER CARD IMAGES IN THE TAPE HEADER FILE
C****
      IF( .NOT. CFIRST ) THEN                                           ! ARE THERE ANY CARDS FOR THE TAPE HEADER?
          IF( IRUN .NE. 0) THEN
              CALL PODISC(IUNHDR,1,0)                                   ! REWIND THE HEADER FILE
              DO 2100 I=1,40
                 CALL RDDISC(IUNHDR,CHEADR(I),20,ISTAT)
                 IF(COMMNT(I).NE.' ') cheadr(i) = COMMNT(I)             ! TRANSLATE AND MOVE IT
 2100         CONTINUE
              CALL PODISC(IUNHDR,1,0)
              DO 2110 I=1,40                                            ! REWRITE THE HEADER TO DISC
 2110         CALL WRDISC(IUNHDR,CHEADR(I),20)
          ENDIF
      ENDIF
      RETURN
      END
