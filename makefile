# REQUIRED: GNU Make.  All other makes are broken

VERSION:=${shell cat VERSION}

FC=gfortran
CC=gcc
#  On Macs  SEE SIOSEIS README - varies with the OSX version and macports
#FC=/usr/local/gfortran/bin/gfortran
#CC=/usr/local/gfortran/bin/gcc


FFLAGS :=

# Fortran optimizations
FFLAGS += -O2
FFLAGS += -funroll-loops
FFLAGS += -fexpensive-optimizations
FFLAGS += -ffast-math

# Fortran debugging and code checks
#FFLAGS += -g
#FFLAGS += -Wall

CFLAGS :=
#CFLAGS += -m32
#          -m32 does not work with gfortran on gcc version 4.1.2 20080704 (Red Hat 4.1.2-46)

# Optimizations
#CFLAGS += -02 
#CFLAGS += -ffast-math
#CFLAGS += -funroll-loops
#CFLAGS += -fexpensive-optimizations
#CFLAGS += -DNDEBUG  # Turn off any asserts

# Debugging and code checks
#CFLAGS += -g  # Can leave on even when optimizing
#CFLAGS += -Wall
#CFLAGS += -Wimplicit-int 
#CFLAGS += -Wimplicit-function-declaration 
#CFLAGS += -Wnested-externs
#CFLAGS += -Wimplicit
#CFLAGS += -W
#CFLAGS += -Wredundant-decls


LDFLAGS:=-lgfortran

SIOSEIS_OBJ:=contro.o setptr.o upcase.o getpro.o dcode.o lcode.o ddcode.o ebcasc.o\
is_big_endian.o tohex.o secsdms.o ascebc.o getoke.o rdline.o rline.o \
magtap.o magmacosx.o syned.o synex.o sseis.o gasdev.o ran1.o \
pouted.o poutex.o ptrlst.o inap.o rlseap.o shifts.o \
apsim.o tpchng.o touted.o wrttrc.o diskio.o rdtrc.o ibm2fp.o died.o diex.o swp_trhdr.o \
sfp2fp.o fp2sfp.o ie2ibm.o swap16.o swap32.o swap64.o inedit.o inputx.o \
ploted.o plotex.o trplot.o gentl.o gentl1.o maxsc.o maxsca.o \
tlann.o sideann.o julcal.o getlanno.o spp.o spp2.o \
filted.o filtex.o bpass.o window.o conv.o dconv.o convol.o tvfilt.o tvfvfc.o nzcros.o \
agced.o  agcex.o agc.o frstnz.o agcap.o decoed.o decoex.o pdecon.o decon.o eureka.o wiener.o\
cross.o zero.o dot.o fold.o \
wbted.o wbtex.o muteed.o muteex.o mute.o muteap.o zcmute.o \
geomed.o geomex.o navgeom.o ukooain.o ldgogeom.o ldeogeom.o calcrp.o rpxy.o \
segyxy.o range.o getdep.o lendeg.o calcrp3d.o sionav2segy.o get_sio_nav.o \
avened.o avenex.o avevfc.o avenor.o debiex.o demean.o debias.o \
mixed.o mixex.o shfted.o shftex.o weiged.o weigex.o moment.o flated.o flatex.o rectc.o \
smuted.o smutex.o fftinv.o udeced.o udecex.o \
t2fed.o t2fex.o ufiled.o ufilex.o fftfwd.o polarc.o gathed.o gather.o \
nmoed.o nmoex.o nmo2ex.o nmo3ex.o nmovfc.o nmonap.o ivelt.o findv.o nmoapp.o int2rms.o \
velaed.o velaex.o \
invplt.o stksem.o stkse.o semstk.o semst.o velplt.o plotvs.o clvplt.o cvnmo.o \
stacked.o stackex.o acored.o acorex.o tx2fed.o tx2fex.o f2ted.o f2tex.o scalet.o \
fkfied.o fkfiex.o shindx.o fkmied.o fkmiex.o fk2ted.o fk2tex.o hale.o mrgfk.o lenstr.o \
spltfk.o gnrfft.o dskpos.o chkbin.o chkpra.o chkprc.o segded.o segdex.o \
ldgo_tr0.o segd20.o prntx2.o doed.o doex.o fdmied.o fdmiex.o fdmvel.o avbufi.o \
avintr.o vapsim.o fdmlin.o vsfdmc.o myspin.o xslice.o \
transp.o dummies.o t2ded.o t2dex.o t2d.o t2dint.o tx2ted.o tx2tex.o fft.o \
tp2ted.o tp2tex.o irisex.o gained.o gainex.o pgain.o filters.o lpbut3p.o \
woodfilt.o \
headed.o headex.o sorted.o sortex.o indexx.o fdfmed.o \
fdfmex.o fdmdif.o fddvel.o sioseis_version.o refplot.o reltap.o \
dmoed.o dmoex.o logsed.o logsex.o resaed.o resaex.o polint.o getdate.o \
despiked.o despikex.o sort.o tredited.o treditex.o \
sadded.o saddex.o cated.o catex.o fkshed.o \
fkshex.o psfk.o ssmied.o ssmiex.o sspost2.o fastf.o slave.o \
uadded.o uaddex.o umulted.o umultex.o histed.o histex.o cfiled.o cfilex.o \
seg2ed.o seg2ex.o caljul.o xcored.o xcorex.o stked.o stkex.o \
grdouted.o grdoutex.o xstared.o xstarex.o segdded.o segddex.o i24i32.o \
bldgname.o leeshdr.o gpgga.o gpggaa.o dbt.o unsigned.o swelled.o swellex.o 

SIO2SUN_OBJ := sio2sun.o get_nbytes_sio.o planes_to_pixels.o swap32c.o

SIO2HP_OBJ= sio2hp.o getparams.o sio2hp_version.o rline.o diskio.o is_big_endian.o \
swap16.o upcase.o dcode.o rdline.o getoke.o

LSD_OBJ=lsd.o secsdms.o is_big_endian.o swap16.o swap32.o diskio.o unsigned.o

LSH_OBJ:=lsh.o secsdms.o is_big_endian.o swap16.o swap32.o diskio.o unsigned.o

DUTIL_OBJ=dutil.o swap64.o dd2ieee.o complement.o diskio.o \
getoke.o rdline.o swap16.o swap32.o dr2iee.o dcode.o tohex.o \
ibm2fp.o ebcasc.o upcase.o shifts.o rline.o mp32ieee.o

JUL2CAL_OBJ=jul2cal.o julcal.o dcode.o getoke.o rdline.o rline.o diskio.o

CAL2JUL_OBJ=cal2jul.o caljul.o dcode.o getoke.o rdline.o rline.o diskio.o

default:
	@echo
	@echo
	@echo "        Welcome to SIOSEIS ${VERSION}"
	@echo
	@echo "Build options:"
	@echo
	@echo "  clean   - remove all objects and executables"
	@echo "  all     - build everything"
	@echo "  tar     - create a release source tar using VERSION"
	@echo
	@echo "  sioseis - build just sioseis"
	@echo "  sio2sun - build just sio2sun"
	@echo "  sio2hp  - build just sio2hp"
	@echo "  lsd     - build just lsd"
	@echo "  lsh     - build just lsd"
	@echo "  dutil   - build just dutil"
	@echo "  cal2jul - build just cal2jul"
	@echo "  jul2cal - build just jul2cal"
	@echo
	@echo "Read the README for more information"

clean:
	rm -f *.o sioseis sio2sun sio2hp lsd lsh dutil cal2jul jul2cal

all: sioseis sio2sun sio2hp lsd lsh dutil cal2jul jul2cal

DIST:=sioseis-${shell cat VERSION}
TAR:=${DIST}.tar
tar:
	rm -rf ${DIST}
	mkdir ${DIST}
	cp -p *.f *.c *.h [A-Z]* make* ${DIST}/
	tar cf ${TAR} ${DIST}
	bzip2 -9 ${TAR}
	rm -rf ${DIST}


sioseis: $(SIOSEIS_OBJ)
	$(FC) $(FFLAGS) $(SIOSEIS_OBJ) $(LDFLAGS) -o sioseis 

sio2sun:$(SIO2SUN_OBJ)
	$(CC) $(CFLAGS) $(SIO2SUN_OBJ) -o sio2sun

sio2hp:$(SIO2HP_OBJ)
	$(FC) $(FFLAGS) $(SIO2HP_OBJ) -o sio2hp

lsd: $(LSD_OBJ)
	$(FC) $(FFLAGS) $(LSD_OBJ) $(LIBS) -o lsd

lsh:$(LSH_OBJ)
	$(FC) $(FFLAGS) $(LSH_OBJ) $(LIBS) -o lsh

dutil:$(DUTIL_OBJ)
	$(FC) $(FFLAGS) $(DUTIL_OBJ) -o dutil

cal2jul:$(CAL2JUL_OBJ)
	$(FC) $(FFLAGS) $(CAL2JUL_OBJ) -o cal2jul

jul2cal:$(JUL2CAL_OBJ)
	$(FC) $(FFLAGS) $(JUL2CAL_OBJ) -o jul2cal
