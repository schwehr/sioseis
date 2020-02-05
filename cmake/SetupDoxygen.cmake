### Doxygen congfiguration
# Doxygen is recommended but not required
# Add a target to generate API documentation with Doxygen
find_package(Doxygen)
if(DOXYGEN_FOUND)
  message("-- Found Doxygen: setting up configuration files")
  # FYI: CMake variables to be substituted into Doxyfile.in
  # CPACK_PACKAGE_NAME
  # CPACK_PACKAGE_VERSION
  # CPACK_PACKAGE_DESCRIPTION_SUMMARY

  # Set input and output files
  set(DOXYFILE_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/Doxyfile.in)
  set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

  # Create Doxyfile by substituting CMake vars into template
  configure_file(${DOXYFILE_IN} ${DOXYFILE_OUT} @ONLY)

  set(DOXYLAYOUT_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/DoxygenLayout.xml.in)
  set(DOXYLAYOUT_OUT ${CMAKE_CURRENT_BINARY_DIR}/DoxygenLayout.xml)

  # Create DoxygenLayout.xml by substituting CMake vars into template
  configure_file(${DOXYLAYOUT_IN} ${DOXYLAYOUT_OUT} @ONLY)

  set(DOXYEXTENSIONS_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/Doxygen_Extensions.cfg.in)
  set(DOXYEXTENSIONS_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxygen_Extensions.cfg)

  # Create DoxygenLayout.xml by substituting CMake vars into template
  configure_file(${DOXYEXTENSIONS_IN} ${DOXYEXTENSIONS_OUT} @ONLY)

  set(DOXYLATEXHEADER_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/doxygen_header.tex.in)
  set(DOXYLATEXHEADER_OUT ${CMAKE_CURRENT_BINARY_DIR}/doxygen_header.tex)

  # Create doxygen_header.tex by substituting CMake vars into template
  configure_file(${DOXYLATEXHEADER_IN} ${DOXYLATEXHEADER_OUT} @ONLY)

  set(DOXYLATEXFOOTER_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/doxygen_footer.tex.in)
  set(DOXYLATEXFOOTER_OUT ${CMAKE_CURRENT_BINARY_DIR}/doxygen_footer.tex)

  # Create doxygen_footer.tex by substituting CMake vars into template
  configure_file(${DOXYLATEXFOOTER_IN} ${DOXYLATEXFOOTER_OUT} @ONLY)

  set(DOXYLATEXSTYLE_IN  ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/doxygen.sty.in)
  set(DOXYLATEXSTYLE_OUT ${CMAKE_CURRENT_BINARY_DIR}/doxygen.sty)

  # Create doxygen_header.tex by substituting CMake vars into template
  configure_file(${DOXYLATEXSTYLE_IN} ${DOXYLATEXSTYLE_OUT} @ONLY)

  #ok COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
  add_custom_target(
    docs # ALL
    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE_OUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating API documentation PDF with Doxygen" VERBATIM
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/doc
               ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
               ${CMAKE_CURRENT_BINARY_DIR}/DoxygenLayout.xml
               ${CMAKE_CURRENT_BINARY_DIR}/Doxygen_Extensions.cfg
               ${CMAKE_CURRENT_BINARY_DIR}/doxygen_header.tex
               ${CMAKE_CURRENT_BINARY_DIR}/doxygen_footer.tex
               ${CMAKE_CURRENT_BINARY_DIR}/doxygen.sty
               ${CMAKE_CURRENT_BINARY_DIR}/doxygen_warnings.txt
  )
else()
  message(STATUS "!! Cannot find doxygen - skipping documentation production")
endif()

