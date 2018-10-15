subroutine sfc2nc(fname,cstring)
 
  use param
  use nemsio_module
  use sfcvars
  use gfstonc_sfc
  use charstrings
  use cdf
  use netcdf

  implicit none

  character(len=*), intent(in) :: fname,cstring

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: nr

  character(len= 32) :: vname, vlong, vunit
             logical :: vout
  character(len=200) :: cdffile

  !---------------------------------------------------------------------

  call read_nemsio_2dgriddata(trim(fname),im,jm,nrecs,inames,itypes,grd2d)

  end subroutine sfc2nc
