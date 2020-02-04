cmake_minimum_required(VERSION 3.13)

enable_language(Fortran C)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON) # Show your work...

# Set maximum processors to use as N_CORES = max(1, <number of CPUs> - 1)
include(ProcessorCount)
ProcessorCount(NCPU)
if (NCPU GREATER 1)
  math(EXPR N_CORES "${NCPU} - 1")
else ()
  set(N_CORES, 1)
endif ()

# Set project name and language
project(sioseis LANGUAGES Fortran C)

# Allow automated retrieval and building of external dependencies
# include(ExternalProject)
# include(FetchContent)

# CTest setup
include(CTest) # Needed for valgrind
enable_testing()

# Set package metadata
set(CPACK_PACKAGE_NAME "sioseis")
set(CPACK_PACKAGE_VERSION_MAJOR "0")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "TBD")
set(CPACK_PACKAGE_VENDOR "TBD")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
# RAA: What is this? Base installation directory for NSIS installer?
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}_${CPACK_PACKAGE_VERSION}")
# set(CPACK_SET_DESTDIR true)
# set(CPACK_INSTALL_PREFIX /opt/${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION})

# ### Build tool discovery
# if (MSVC)
#   find_program(NMAKE_EXECUTABLE NAMES nmake)
# else (MSVC)
#   find_program(MAKE_EXECUTABLE NAMES make gmake)
# endif (MSVC)

# Get the name of the actual Fortran compiler (gfortran, g95, ifort, etc.)
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
# Get the name of the actual C compiler (gcc, icc, etc.)
get_filename_component(C_COMPILER_NAME ${CMAKE_C_COMPILER} NAME)

# Set up Doxygen for documenting library
# include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/SetupDoxygen.cmake")

option(USE_GPROF "Apply compile/link flags for gprof analysis" FALSE)
set(GPROF_FLAGS "-pg")

# Add compiler flags
#! if(MSVC) # MSVC-specific options
#!   message(STATUS "MSVC detected, adding compile flags")
#!   add_compile_options("/W3"
#!                       "/permissive-")
#! #  add_compile_options("/Wall")
#! 
#!   if(CMAKE_BUILD_TYPE MATCHES "Debug")
#!       message("** Windows build configured for Debug")
#!   else()
#!       message("** Windows build configured for Release")
#!   endif()
#! else() # Not MSVC

#OK if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
# Loose
  set(CMAKE_Fortran_FLAGS         "-Wno-argument-mismatch -ffpe-trap=invalid,zero,overflow -fbacktrace -fno-automatic -finit-local-zero")
# Normal
#  set(CMAKE_Fortran_FLAGS         "-ffpe-trap=invalid,zero,overflow -fbacktrace -fno-automatic -finit-local-zero -Wall -pedantic")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
# Loose
  set(CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -Og -g")
# Default (no coverage analysis)
#  set(CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -Og -g")
# Default with underflow trap (no coverage analysis, with gprof)
#    set(CMAKE_Fortran_FLAGS_DEBUG   "-ffpe-trap=underflow -fbounds-check -Og -pg -g")
# Default with underflow trap (no coverage analysis, without gprof [for valgrind analysis])
#   set(CMAKE_Fortran_FLAGS_DEBUG   "-ffpe-trap=underflow -fbounds-check -Og -g")
# Extended numerical trapping (no coverage analysis)
#    set(CMAKE_Fortran_FLAGS_DEBUG   "-ffpe-trap=underflow,denormal -fbounds-check -Og -pg -g")
# (with coverage analysis)
#    set(CMAKE_Fortran_FLAGS_DEBUG   "-fbounds-check -Og -fprofile-arcs -ftest-coverage -pg -g")
  if (${USE_GPROF})
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${GPROF_FLAGS}") 
  endif()
#OK else()
#OK   message(FATAL_ERROR "Only gfortran supported at present")
#OK endif ()

#OK if (C_COMPILER_NAME MATCHES "gcc.*")
  # gcc
  set(CMAKE_C_FLAGS         "-Wall -pedantic")
  set(CMAKE_C_FLAGS_RELEASE "-O2")
# Default (no coverage analysis)
  set(CMAKE_C_FLAGS_DEBUG   "-fbounds-check -Og -g")
# Default with underflow trap (no coverage analysis, with gprof)
#    set(CMAKE_C_FLAGS_DEBUG   "-ffpe-trap=underflow -fbounds-check -Og -pg -g")
# Default with underflow trap (no coverage analysis, without gprof [for valgrind analysis])
#   set(CMAKE_C_FLAGS_DEBUG   "-ffpe-trap=underflow -fbounds-check -Og -g")
# Extended numerical trapping (no coverage analysis)
#    set(CMAKE_C_FLAGS_DEBUG   "-ffpe-trap=underflow,denormal -fbounds-check -Og -pg -g")
# (with coverage analysis)
#    set(CMAKE_C_FLAGS_DEBUG   "-fbounds-check -Og -fprofile-arcs -ftest-coverage -pg -g")
  if (${USE_GPROF})
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${GPROF_FLAGS}") 
  endif()
#OK else()
#OK   message(FATAL_ERROR "Only gcc supported at present")
#OK endif ()

# if (CMAKE_BUILD_TYPE MATCHES "Debug")
#   message("** Linux build configured for Debug")
#   # Configure code test coverage analysis
# #    include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CodeCoverage.cmake")
#   include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/FortranCoverage.cmake")
#   if(LCOV_PATH)
#     APPEND_COVERAGE_COMPILER_FLAGS()
#   else()
#     message(STATUS "lcov not detected; not setting code coverage compilation flags")
#   endif()
# else ()
#   message("** Linux build configured for Release")
# endif ()

# Define Fortran module directory (output location)
#OK set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/finclude)

# Adjust compiled code extensions to simplify test coverage analysis
#OK set(CMAKE_CXX_OUTPUT_EXTENSION_REPLACE ON)
set(CMAKE_C_OUTPUT_EXTENSION_REPLACE ON)
set(CMAKE_Fortran_OUTPUT_EXTENSION_REPLACE ON) #? I believe this is necessary to undo CMake extension weirdness

##### SIOSEIS ####

option(BUILD_SIOSEIS "Compile sioseis executable" TRUE)

if(BUILD_SIOSEIS)
  # Set source and include prefix if needed
  set(SIOSEIS_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(SIOSEIS_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${SIOSEIS_INCLUDE_DIR})

  # Define headers for sioseis executable
#  list(APPEND SIOSEIS_INCLUDES "${SIOSEIS_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for sioseis executable
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/acored.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/acorex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/agcap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/agced.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/agcex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/agc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/apsim.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ascebc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avbufi.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avened.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avenex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avenor.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avevfc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/avintr.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/bpass.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/calcrp3d.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/calcrp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/caljul.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/cated.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/catex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/cfiled.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/cfilex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/chkbin.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/chkpra.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/chkprc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/clvplt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/contro.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/conv.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/convol.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/cross.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/cvnmo.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dbt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dcode.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dconv.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ddcode.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/debias.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/debiex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/decoed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/decoex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/decon.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/demean.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/despiked.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/despikex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/died.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/diex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dmoed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dmoex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/doed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/doex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dot.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dskpos.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/dummies.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ebcasc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/eureka.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/f2ted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/f2tex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fastf.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fddvel.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdfmed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdfmex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdmdif.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdmied.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdmiex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdmlin.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fdmvel.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fft.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fftfwd.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fftinv.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/filted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/filters.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/filtex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/findv.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fk2ted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fk2tex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkfied.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkfiex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkmied.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkmiex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkshed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fkshex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/flated.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/flatex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fold.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/fp2sfp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/frstnz.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gained.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gainex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gasdev.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gathed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gather.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gentl1.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gentl.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/geomed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/geomex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/getlanno.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/getoke.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/getpro.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/get_sio_nav.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gnrfft.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gpggaa.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/gpgga.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/grdouted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/grdoutex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/hale.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/headed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/headex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/histed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/histex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/i24i32.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ibm2fp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ie2ibm.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/inap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/indexx.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/inedit.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/inputx.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/int2rms.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/invplt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/irisex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ivelt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/julcal.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/lcode.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ldeogeom.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ldgogeom.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ldgo_tr0.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/leeshdr.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/lendeg.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/lenstr.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/logsed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/logsex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/lpbut3p.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/magtap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/maxsca.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/maxsc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/mixed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/mixex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/moment.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/mrgfk.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/muteap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/muteed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/muteex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/mute.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/myspin.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/navgeom.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmo2ex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmo3ex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmoapp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmoed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmoex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmonap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nmovfc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/nzcros.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/pdecon.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/pgain.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ploted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/plotex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/plotvs.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/polarc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/polint.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/pouted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/poutex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/prntx2.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/psfk.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ptrlst.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ran1.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/range.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rdline.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rdtrc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rectc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/refplot.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/reltap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/resaed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/resaex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rline.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rlseap.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/rpxy.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sadded.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/saddex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/scalet.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/secsdms.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/seg2ed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/seg2ex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segd20.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segdded.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segddex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segded.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segdex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/segyxy.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/semst.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/semstk.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/setptr.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sfp2fp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/shfted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/shftex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/shindx.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sionav2segy.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sioseis_version.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/slave.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/smuted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/smutex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sorted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sortex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sort.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/spltfk.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/spp2.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/spp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sseis.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ssmied.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ssmiex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sspost2.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stacked.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stackex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stked.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stkex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stkse.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/stksem.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swap16.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swap32.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swap64.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swelled.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swellex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/swp_trhdr.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/syned.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/synex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2ded.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2dex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2d.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2dint.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2fed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/t2fex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tohex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/touted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tp2ted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tp2tex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tpchng.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/transp.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tredited.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/treditex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/trplot.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tvfilt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tvfvfc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tx2fed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tx2fex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tx2ted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tx2tex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/uadded.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/uaddex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/udeced.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/udecex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ufiled.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ufilex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/ukooain.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/umulted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/umultex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/upcase.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/vapsim.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/velaed.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/velaex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/velplt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/vsfdmc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/wbted.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/wbtex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/weiged.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/weigex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/wiener.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/window.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/woodfilt.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/wrttrc.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/xcored.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/xcorex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/xslice.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/xstared.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/xstarex.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/zcmute.f")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/zero.f")
# Define C sources for sioseis executable
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/bldgname.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/diskio.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/getdate.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/getdep.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/is_big_endian.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/magmacosx.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/shifts.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/sideann.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/tlann.c")
  list(APPEND SIOSEIS_SOURCES "${SIOSEIS_SOURCE_DIR}/unsigned.c")

  # # Define modules (built) for library
  # list(APPEND SIOSEIS_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/SIOSEIS.mod")

  add_executable(sioseis ${SIOSEIS_SOURCES})
  # add_dependencies(sioseis zzz)

  # Set compilation options for library target
#  target_compile_options(sioseis PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS sioseis)
endif(BUILD_SIOSEIS)

##### SIO2SUN ####

option(BUILD_SIO2SUN "Compile sio2sun executable" TRUE)

if(BUILD_SIO2SUN)
  # Set source and include prefix if needed
  set(SIO2SUN_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(SIO2SUN_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${SIO2SUN_INCLUDE_DIR})

  # Define headers for sio2sun executable
#  list(APPEND SIO2SUN_INCLUDES "${SIO2SUN_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for sio2sun executable
# Define C sources for sio2sun executable
  list(APPEND SIO2SUN_SOURCES "${SIO2SUN_SOURCE_DIR}/get_nbytes_sio.c")
  list(APPEND SIO2SUN_SOURCES "${SIO2SUN_SOURCE_DIR}/planes_to_pixels.c")
  list(APPEND SIO2SUN_SOURCES "${SIO2SUN_SOURCE_DIR}/sio2sun.c")
  list(APPEND SIO2SUN_SOURCES "${SIO2SUN_SOURCE_DIR}/swap32c.c")

  # # Define modules (built) for library
  # list(APPEND SIO2SUN_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/SIO2SUN.mod")

  add_executable(sio2sun ${SIO2SUN_SOURCES})
  # add_dependencies(sio2sun zzz)

  # Set compilation options for library target
#  target_compile_options(sio2sun PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS sio2sun)
endif(BUILD_SIO2SUN)

##### SIO2HP ####

option(BUILD_SIO2HP "Compile sio2hp executable" TRUE)

if(BUILD_SIO2HP)
  # Set source and include prefix if needed
  set(SIO2HP_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(SIO2HP_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${SIO2HP_INCLUDE_DIR})

  # Define headers for sio2hp executable
#  list(APPEND SIO2HP_INCLUDES "${SIO2HP_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for sio2hp executable
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/dcode.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/getoke.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/getparams.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/rdline.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/rline.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/sio2hp.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/sio2hp_version.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/swap16.f")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/upcase.f")
# Define C sources for sio2hp executable
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/diskio.c")
  list(APPEND SIO2HP_SOURCES "${SIO2HP_SOURCE_DIR}/is_big_endian.c")

  # # Define modules (built) for library
  # list(APPEND SIO2HP_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/SIO2HP.mod")

  add_executable(sio2hp ${SIO2HP_SOURCES})
  # add_dependencies(sio2hp zzz)

  # Set compilation options for library target
#  target_compile_options(sio2hp PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS sio2hp)
endif(BUILD_SIO2HP)

##### LSD ####

option(BUILD_LSD "Compile lsd executable" TRUE)

if(BUILD_LSD)
  # Set source and include prefix if needed
  set(LSD_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(LSD_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${LSD_INCLUDE_DIR})

  # Define headers for lsd executable
#  list(APPEND LSD_INCLUDES "${LSD_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for lsd executable
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/lsd.f")
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/secsdms.f")
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/swap16.f")
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/swap32.f")
# Define C sources for lsd executable
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/diskio.c")
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/is_big_endian.c")
  list(APPEND LSD_SOURCES "${LSD_SOURCE_DIR}/unsigned.c")

  # # Define modules (built) for library
  # list(APPEND LSD_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/LSD.mod")

  add_executable(lsd ${LSD_SOURCES})
  # add_dependencies(lsd zzz)

  # Set compilation options for library target
#  target_compile_options(lsd PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS lsd)
endif(BUILD_LSD)

##### LSH ####

option(BUILD_LSH "Compile lsh executable" TRUE)

if(BUILD_LSH)
  # Set source and include prefix if needed
  set(LSH_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(LSH_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${LSH_INCLUDE_DIR})

  # Define headers for lsh executable
#  list(APPEND LSH_INCLUDES "${LSH_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for lsh executable
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/lsh.f")
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/secsdms.f")
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/swap16.f")
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/swap32.f")
# Define C sources for lsh executable
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/diskio.c")
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/is_big_endian.c")
  list(APPEND LSH_SOURCES "${LSH_SOURCE_DIR}/unsigned.c")

  # # Define modules (built) for library
  # list(APPEND LSH_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/LSH.mod")

  add_executable(lsh ${LSH_SOURCES})
  # add_dependencies(lsh zzz)

  # Set compilation options for library target
#  target_compile_options(lsh PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS lsh)
endif(BUILD_LSH)

##### DUTIL ####

option(BUILD_DUTIL "Compile dutil executable" TRUE)

if(BUILD_DUTIL)
  # Set source and include prefix if needed
  set(DUTIL_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(DUTIL_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${DUTIL_INCLUDE_DIR})

  # Define headers for dutil executable
#  list(APPEND DUTIL_INCLUDES "${DUTIL_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for dutil executable
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/dcode.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/dd2ieee.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/dr2iee.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/dutil.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/ebcasc.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/getoke.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/ibm2fp.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/mp32ieee.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/rdline.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/rline.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/swap16.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/swap32.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/swap64.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/tohex.f")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/upcase.f")
# Define C sources for dutil executable
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/complement.c")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/diskio.c")
  list(APPEND DUTIL_SOURCES "${DUTIL_SOURCE_DIR}/shifts.c")

  # # Define modules (built) for library
  # list(APPEND DUTIL_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/DUTIL.mod")

  add_executable(dutil ${DUTIL_SOURCES})
  # add_dependencies(dutil zzz)

  # Set compilation options for library target
#  target_compile_options(dutil PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS dutil)
endif(BUILD_DUTIL)

##### JUL2CAL ####

option(BUILD_JUL2CAL "Compile jul2cal executable" TRUE)

if(BUILD_JUL2CAL)
  # Set source and include prefix if needed
  set(JUL2CAL_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(JUL2CAL_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${JUL2CAL_INCLUDE_DIR})

  # Define headers for jul2cal executable
#  list(APPEND JUL2CAL_INCLUDES "${JUL2CAL_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for jul2cal executable
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/dcode.f")
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/getoke.f")
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/jul2cal.f")
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/julcal.f")
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/rdline.f")
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/rline.f")
# Define C sources for jul2cal executable
  list(APPEND JUL2CAL_SOURCES "${JUL2CAL_SOURCE_DIR}/diskio.c")

  # # Define modules (built) for library
  # list(APPEND JUL2CAL_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/JUL2CAL.mod")

  add_executable(jul2cal ${JUL2CAL_SOURCES})
  # add_dependencies(jul2cal zzz)

  # Set compilation options for library target
#  target_compile_options(jul2cal PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS jul2cal)
endif(BUILD_JUL2CAL)

##### CAL2JUL ####

option(BUILD_CAL2JUL "Compile cal2jul executable" TRUE)

if(BUILD_CAL2JUL)
  # Set source and include prefix if needed
  set(CAL2JUL_SOURCE_DIR  "${CMAKE_CURRENT_SOURCE_DIR}")
#  set(CAL2JUL_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

  # Bring the headers into the project
#  include_directories(${CAL2JUL_INCLUDE_DIR})

  # Define headers for cal2jul executable
#  list(APPEND CAL2JUL_INCLUDES "${CAL2JUL_INCLUDE_DIR}/DoesNotExist.hpp")

# Define Fortran sources for cal2jul executable
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/cal2jul.f")
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/caljul.f")
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/dcode.f")
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/getoke.f")
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/rdline.f")
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/rline.f")
# Define C sources for cal2jul executable
  list(APPEND CAL2JUL_SOURCES "${CAL2JUL_SOURCE_DIR}/diskio.c")

  # # Define modules (built) for library
  # list(APPEND CAL2JUL_MODULES "${CMAKE_Fortran_MODULE_DIRECTORY}/CAL2JUL.mod")

  add_executable(cal2jul ${CAL2JUL_SOURCES})
  # add_dependencies(cal2jul zzz)

  # Set compilation options for library target
#  target_compile_options(cal2jul PUBLIC)

  # Add to installation list
  list(APPEND ALL_APPLICATIONS cal2jul)
endif(BUILD_CAL2JUL)

###############################################################################
## Analysis ###################################################################
###############################################################################

# Diagram project library dependencies
add_custom_target(dependency_map ALL
  COMMENT "Create dependency map"
  COMMAND ${CMAKE_COMMAND} "--graphviz=project_dependencies.dot" .
  COMMAND dot -Tpng project_dependencies.dot -o project_dependencies.png
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  BYPRODUCTS
    project_dependencies.dot
    project_dependencies.dot.cal2jul
    project_dependencies.dot.cal2jul.dependers
    project_dependencies.dot.dutil
    project_dependencies.dot.dutil.dependers
    project_dependencies.dot.jul2cal
    project_dependencies.dot.jul2cal.dependers
    project_dependencies.dot.lsd
    project_dependencies.dot.lsd.dependers
    project_dependencies.dot.lsh
    project_dependencies.dot.lsh.dependers
    project_dependencies.dot.sio2hp
    project_dependencies.dot.sio2hp.dependers
    project_dependencies.dot.sio2sun
    project_dependencies.dot.sio2sun.dependers
    project_dependencies.dot.sioseis
    project_dependencies.dot.sioseis.dependers
    project_dependencies.png
)

# # Run profiling tests (-C Profile)
# add_custom_target(profile
#   COMMENT "Run profiling tests"
#   COMMAND ${CMAKE_CTEST_COMMAND} -C Profile --output-on-failures
#   DEPENDS binnuc sioseis
#   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
# )
# 
# # Run valgrind tools (-C Valgrind)
# add_custom_target(valgrind
#   COMMENT "Run memory and performance checks"
#   COMMAND ${CMAKE_CTEST_COMMAND} -C Valgrind --output-on-failures
#   DEPENDS binnuc sioseis
#   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
# )

# Add static analysis activities here

###############################################################################
## Testing ####################################################################
###############################################################################

### Custom tests

# Set up test directories

# add_custom_command(
#     TARGET sioseis
#     POST_BUILD
#     COMMAND ${CMAKE_COMMAND} -E echo "  Copying SIOSEIS acceptance test directories:"
#     COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/test/test_1 ${CMAKE_CURRENT_BINARY_DIR}/test_1
#     COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/test/test_2 ${CMAKE_CURRENT_BINARY_DIR}/test_2
# )

# Acceptance tests

# # SIOSEIS acceptance test case (test_1)
# add_test(NAME test_1
#          COMMAND ctest_wrapper.sh $<TARGET_FILE:sioseis>
#          DEPENDS sioseis
#          WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_1"
#          CONFIGURATIONS Debug Release ""
# )
# 
# add_test(NAME test_2
#          COMMAND ctest_wrapper.sh $<TARGET_FILE:sioseis>
#          DEPENDS sioseis
#          WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_2"
#          CONFIGURATIONS Debug Release ""
# )
# 
# # Profiling tests (run manually with `ctest -C Profile`; not run by default)
# 
# #TODO: Ensure operf, opreport, dot, and gprof2dot are available
# # SIOSEIS_1 profiling run
# add_test(NAME profile_sioseis_1
#          COMMAND ctest_profile.sh $<TARGET_FILE:sioseis>
#          DEPENDS sioseis
#          WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/SIOSEIS_1"
#          CONFIGURATIONS Profile
# )
# 
# # LOCA_1 profiling run
# add_test(NAME profile_loca_1
#          COMMAND ctest_profile.sh $<TARGET_FILE:sioseis> $<TARGET_FILE:sioseis>
#          DEPENDS sioseis sioseis
#          WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/LOCA_1"
#          CONFIGURATIONS Profile
# )
# 
# # LOCA_2 profiling run
# add_test(NAME profile_loca_2
#          COMMAND ctest_profile.sh $<TARGET_FILE:sioseis> $<TARGET_FILE:sioseis>
#          DEPENDS sioseis sioseis
#          WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/LOCA_2"
#          CONFIGURATIONS Profile
# )
# 
# # Memory and performance checks (run manually with `ctest -C Valgrind`; not run by default)
# 
# # TODO: Ensure valgrind with memcheck, cachegrind, callgrind, and massif tools is available
# 
# # SIOSEIS memory and performance check
# add_test(NAME valgrind_sioseis_1
#          COMMAND ctest_valgrind.sh $<TARGET_FILE:sioseis>
#          DEPENDS sioseis
#          WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/test_1"
#          CONFIGURATIONS Valgrind
# )

# Non-functional and regressions tests

# if(LCOV_PATH)
#   if(CMAKE_BUILD_TYPE MATCHES "Debug")
#     SETUP_TARGET_FOR_COVERAGE_LCOV(
#         NAME coverage
# #        EXECUTABLE ctest -C Release -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
#         EXECUTABLE ctest -C Debug -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
# ## General ctest use (no -C)
# #        EXECUTABLE ctest -j ${N_CORES} # Executable in PROJECT_BINARY_DIR
# #        LCOV_ARGS --no-external
#         DEPENDENCIES
#             binnuc
#             sioseis
#     )
#   endif()
# endif()

###############################################################################
## Packaging ##################################################################
###############################################################################

# TODO: Does this interfere with other platforms?
# Autogenerate Debian dependency information
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)

# TODO: Does this need to be adjusted by platform?
# NOTE: Just assume GNU install dirs work everywhere unless shown otherwise
include(GNUInstallDirs)

# Note: Append PROJECT_NAME to CMAKE_INSTALL_DATADIR to use
# `install([DIRECTORY|FILES] ... TYPE DATA)` instead of an explicit
# DESTINATION. Without this change, CPack will pollute <prefix>/share with data
# files instead of keeping them segregated by project in
# <prefix>/share/<project>
set(CMAKE_INSTALL_DATADIR "${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}")

# if(UNIX)
#   # Set GNU system install dirs
# elseif(WIN32)
#   # Set Windows install dirs
# elseif(APPLE)
#   # Set OSX install dirs
# endif()

# TODO: Check on Windows
# TODO: Check on OSX

# if(APPLE)
# message("OSX installation package not supported yet")
# else()
  # For SIOSEIS executable and any libraries
  install(TARGETS sioseis sio2sun sio2hp lsd lsh dutil jul2cal cal2jul
    RUNTIME
      DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY
      DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE
      DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )

  # License and README
  install(FILES "${CPACK_PACKAGE_DESCRIPTION_FILE}"
                "${CPACK_RESOURCE_FILE_LICENSE}"
    TYPE DOC
  )

#ok   # Databases
#ok #  install(DIRECTORY "${CMAKE_SOURCE_DIR}/data"
#ok   install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/data/"
#ok       TYPE DATA
#ok   )
# endif()

if (WIN32 AND NOT UNIX)
  # Windows systems
  set(CPACK_GENERATOR "ZIP" "NSIS")

  # NSIS-specific settings
  set(CPACK_NSIS_DISPLAY_NAME "Sioseis")
  set(CPACK_NSIS_PACKAGE_NAME "Sioseis")
  set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)

  set(CPACK_NSIS_MUI_ICON "${PROJECT_SOURCE_DIR}/img/sioseis_logo_32.ico")
  set(CPACK_NSIS_MUI_UNIICON "${PROJECT_SOURCE_DIR}/img/sioseis_logo_32.ico")

  # NuGet-specific settings -- TBD

  # Enable once NuGet and WIX are installed/available
#  set(CPACK_GENERATOR "ZIP" "NSIS" "NuGet" "WIX")
else ()
  # Debian-like systems
  set(CPACK_DEBIAN_PACKAGE_MAINTAINER "TBD")
  set(CPACK_GENERATOR "ZIP" "TGZ" "TBZ2" "DEB")

  # Red Hat-like systems
  # set(CPACK_GENERATOR "ZIP" "TGZ" "TBZ2" "RPM")
endif ()

# This must be last
include(CPack)
