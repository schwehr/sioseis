# REQUIRED: GNU Make.  All other makes are broken

VERSION:=${shell cat VERSION}

# FC=gfortran
# CC=gcc

FFLAGS :=
FFLAGS += -g
FFLAGS += -O2
# TODO(schwehr): Enable more warnings
# FFLAGS += -Wall
# FFLAGS += -Wextra

CFLAGS :=
CFLAGS += -O2
CFLAGS += -g
# TODO(schwehr): Enable more warnings
# CFLAGS += -Wall
# CFLAGS += -Wextra
# CFLAGS += -DNDEBUG  # Turn off any asserts

LDFLAGS := -lgfortran

SIOSEIS_OBJ :=
SIOSEIS_OBJ += contro.o setptr.o upcase.o getpro.o dcode.o lcode.o ddcode.o ebcasc.o
SIOSEIS_OBJ += is_big_endian.o tohex.o secsdms.o ascebc.o getoke.o rdline.o rline.o
SIOSEIS_OBJ += magtap.o magmacosx.o syned.o synex.o sseis.o gasdev.o ran1.o
SIOSEIS_OBJ += pouted.o poutex.o ptrlst.o inap.o rlseap.o shifts.o
SIOSEIS_OBJ += apsim.o tpchng.o touted.o wrttrc.o diskio.o rdtrc.o ibm2fp.o died.o diex.o swp_trhdr.o
SIOSEIS_OBJ += sfp2fp.o fp2sfp.o ie2ibm.o swap16.o swap32.o swap64.o inedit.o inputx.o
SIOSEIS_OBJ += ploted.o plotex.o trplot.o gentl.o gentl1.o maxsc.o maxsca.o
SIOSEIS_OBJ += tlann.o sideann.o julcal.o getlanno.o spp.o spp2.o
SIOSEIS_OBJ += filted.o filtex.o bpass.o window.o conv.o dconv.o convol.o tvfilt.o tvfvfc.o nzcros.o
SIOSEIS_OBJ += agced.o  agcex.o agc.o frstnz.o agcap.o decoed.o decoex.o pdecon.o decon.o eureka.o wiener.o
SIOSEIS_OBJ += cross.o zero.o dot.o fold.o
SIOSEIS_OBJ += wbted.o wbtex.o muteed.o muteex.o mute.o muteap.o zcmute.o
SIOSEIS_OBJ += geomed.o geomex.o navgeom.o ukooain.o ldgogeom.o ldeogeom.o calcrp.o rpxy.o
SIOSEIS_OBJ += segyxy.o range.o getdep.o lendeg.o calcrp3d.o sionav2segy.o get_sio_nav.o
SIOSEIS_OBJ += avened.o avenex.o avevfc.o avenor.o debiex.o demean.o debias.o
SIOSEIS_OBJ += mixed.o mixex.o shfted.o shftex.o weiged.o weigex.o moment.o flated.o flatex.o rectc.o
SIOSEIS_OBJ += smuted.o smutex.o fftinv.o udeced.o udecex.o
SIOSEIS_OBJ += t2fed.o t2fex.o ufiled.o ufilex.o fftfwd.o polarc.o gathed.o gather.o
SIOSEIS_OBJ += nmoed.o nmoex.o nmo2ex.o nmo3ex.o nmovfc.o nmonap.o ivelt.o findv.o nmoapp.o int2rms.o
SIOSEIS_OBJ += velaed.o velaex.o
SIOSEIS_OBJ += invplt.o stksem.o stkse.o semstk.o semst.o velplt.o plotvs.o clvplt.o cvnmo.o
SIOSEIS_OBJ += stacked.o stackex.o acored.o acorex.o tx2fed.o tx2fex.o f2ted.o f2tex.o scalet.o
SIOSEIS_OBJ += fkfied.o fkfiex.o shindx.o fkmied.o fkmiex.o fk2ted.o fk2tex.o hale.o mrgfk.o lenstr.o
SIOSEIS_OBJ += spltfk.o gnrfft.o dskpos.o chkbin.o chkpra.o chkprc.o segded.o segdex.o
SIOSEIS_OBJ += ldgo_tr0.o segd20.o prntx2.o doed.o doex.o fdmied.o fdmiex.o fdmvel.o avbufi.o
SIOSEIS_OBJ += avintr.o vapsim.o fdmlin.o vsfdmc.o myspin.o xslice.o
SIOSEIS_OBJ += transp.o dummies.o t2ded.o t2dex.o t2d.o t2dint.o tx2ted.o tx2tex.o fft.o
SIOSEIS_OBJ += tp2ted.o tp2tex.o irisex.o gained.o gainex.o pgain.o filters.o lpbut3p.o
SIOSEIS_OBJ += woodfilt.o
SIOSEIS_OBJ += headed.o headex.o sorted.o sortex.o indexx.o fdfmed.o
SIOSEIS_OBJ += fdfmex.o fdmdif.o fddvel.o sioseis_version.o refplot.o reltap.o
SIOSEIS_OBJ += dmoed.o dmoex.o logsed.o logsex.o resaed.o resaex.o polint.o getdate.o
SIOSEIS_OBJ += despiked.o despikex.o sort.o tredited.o treditex.o
SIOSEIS_OBJ += sadded.o saddex.o cated.o catex.o fkshed.o
SIOSEIS_OBJ += fkshex.o psfk.o ssmied.o ssmiex.o sspost2.o fastf.o slave.o
SIOSEIS_OBJ += uadded.o uaddex.o umulted.o umultex.o histed.o histex.o cfiled.o cfilex.o
SIOSEIS_OBJ += seg2ed.o seg2ex.o caljul.o xcored.o xcorex.o stked.o stkex.o
SIOSEIS_OBJ += grdouted.o grdoutex.o xstared.o xstarex.o segdded.o segddex.o i24i32.o
SIOSEIS_OBJ += bldgname.o leeshdr.o gpgga.o gpggaa.o dbt.o unsigned.o swelled.o swellex.o

SIO2SUN_OBJ := sio2sun.o get_nbytes_sio.o planes_to_pixels.o swap32c.o

SIO2HP_OBJ := sio2hp.o getparams.o sio2hp_version.o rline.o diskio.o is_big_endian.o
SIO2HP_OBJ += swap16.o upcase.o dcode.o rdline.o getoke.o

LSD_OBJ := lsd.o secsdms.o is_big_endian.o swap16.o swap32.o diskio.o unsigned.o

LSH_OBJ := lsh.o secsdms.o is_big_endian.o swap16.o swap32.o diskio.o unsigned.o

DUTIL_OBJ := dutil.o swap64.o dd2ieee.o complement.o diskio.o
DUTIL_OBJ += getoke.o rdline.o swap16.o swap32.o dr2iee.o dcode.o tohex.o
DUTIL_OBJ += ibm2fp.o ebcasc.o upcase.o shifts.o rline.o mp32ieee.o

JUL2CAL_OBJ := jul2cal.o julcal.o dcode.o getoke.o rdline.o rline.o diskio.o

CAL2JUL_OBJ := cal2jul.o caljul.o dcode.o getoke.o rdline.o rline.o diskio.o

default:
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
	xz -9 ${TAR}
	rm -rf ${DIST}


sioseis: $(SIOSEIS_OBJ)
	$(FC) $(FFLAGS) $(SIOSEIS_OBJ) $(LDFLAGS) -o $@

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
