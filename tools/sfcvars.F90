module sfcvars

  use nemsio_module
  use kinds

  use GFS_diagnostics, only : GFS_externaldiag_type

  implicit none

  real(kind=nemsio_realkind), allocatable, dimension(:)      :: lats,lons
                     integer, allocatable, dimension(:,:)    :: inames,itypes
           real(kind=r_kind), allocatable, dimension(:,:,:)  :: grd2d

           character(len=32), allocatable, dimension(:)      :: varnames, varlong
                real(kind=4), allocatable, dimension(:,:,:)  :: a2d

  integer, parameter :: maxsfcfields = 200

  type(GFS_externaldiag_type) :: GFSsfc(maxsfcfields)

end module sfcvars
