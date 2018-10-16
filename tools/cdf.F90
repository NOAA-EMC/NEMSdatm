module cdf

  use netcdf

  implicit none

  integer, parameter :: ndim1 = 1, ndim2 = 2, ndim3 = 3, ndim4 = 4

  integer, dimension(ndim4) :: dim4, corner4, edge4
  integer, dimension(ndim3) :: dim3, corner3, edge3
  integer, dimension(ndim2) :: dim2, corner2, edge2
  integer, dimension(ndim1) :: dim1, corner1, edge1

  integer :: xtdim, ytdim, zrdim, tdim, ztdim
  integer :: xtid, ytid, ztid
  integer :: zdim1, zdim2, zdim3

  integer :: rc, ncid, timid, datid
  real(kind=4) :: mval = -9999.

  real(kind=8)                  :: tstamp, tstamp0
  character(len=31)             :: torg = 'hours since 1971-01-01 00:00:00'
  character(len=31)             :: tcal = 'gregorian'

  character(len=300) :: cwd
  character(len=500) :: history

  contains
!---------------------------------------------------------------------

  subroutine set_taxis(ytmp,mtmp,dtmp,htmp,mint,stmp)

  implicit none
  real*8 tm_secs_from_bc

  integer, intent(in) :: ytmp, mtmp, dtmp, htmp, mint, stmp

  real(kind = 8) :: tday

  tday = tm_secs_from_bc(ytmp, mtmp, dtmp,  &
                         htmp, mint, stmp) - &
         tm_secs_from_bc(1971,   1,    1,   &
                            0,   0,    0)
    tday = tday/(60.d0*60.d0)
  tstamp = tday

  end subroutine set_taxis
end module cdf
