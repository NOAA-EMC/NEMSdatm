list(APPEND _tools_srcs
  GFS_diagnostics.F90
  cdf.F90
  charstrings.F90
  fieldmatch.F90
  find_t850.F90
  gfstonc_sfc.F90
  gfstonc_sig.F90
  kinds.F90
  param.F90
  rdnemsio.F90
  setup_outcdf.F90
  sfc2nc.F90
  sfcvars.F90
  sigma2nc.F90
  sigmavars.F90
  tm_secs_from_bc.F90
  write_sigmacdf.F90)

add_executable(nemsio2nc ${_tools_srcs})
set_target_properties(nemsio2nc PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(nemsio2nc PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_link_libraries(nemsio2nc PRIVATE nemsio::nemsio)
target_link_libraries(nemsio2nc PRIVATE w3nco::w3nco_d)
target_link_libraries(nemsio2nc PRIVATE NetCDF::NetCDF_Fortran)

install(TARGETS nemsio2nc
        RUNTIME DESTINATION bin
        COMPONENT Utility)
