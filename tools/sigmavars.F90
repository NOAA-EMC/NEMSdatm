module sigmavars

  use nemsio_module

  implicit none

  integer, parameter :: nsigvars =  8 & !3-d
                                 +  2   !2-d

  real(kind=nemsio_realkind), allocatable, dimension(:,:,:) :: vcoord

  real(kind=nemsio_realkind), allocatable, dimension(:,:,:) :: ug,vg,tempg,qg,ozg,cwmrg,dpresg,presg
  real(kind=nemsio_realkind), allocatable, dimension(:,:)   :: psg,zsg

  real(kind=4), allocatable, dimension(:,:)     :: sig2d
  real(kind=4), allocatable, dimension(:,:,:)   :: sig3d

  type SigmaFieldsDefs
    character(len=12)                           :: varname
    character(len=12)                           :: varunit
    character(len=60)                           :: varlong
    logical                                     :: output
    logical                                     :: var3d
  end type SigmaFieldsDefs

  type(SigmaFieldsDefs) :: sgfields(nsigvars)

  ! number of vertical levels to output
  integer, parameter            :: kout = 10
  real(kind=4), dimension(kout) :: zout

  contains 

  subroutine sigmafield_setup

  ! see gfs_nemsiotonc_3d in j.whitaker's python version
  ! checked with Jun Wang and confirmed that FV3 is actual temperature, not 
  ! virtual temperature as listed in j.whitaker's python version
  integer :: ii = 0

  !default is to not output the variable
  sgfields(:)%output       = .false.
  !default is 3d-variable
  sgfields(:)%var3d        = .true.

  ii = ii + 1
  sgfields(ii)%varname     = 'ps'
  sgfields(ii)%varunit     = 'Pa'
  sgfields(ii)%varlong     = 'surface pressure'
  sgfields(ii)%output      = .true.
  sgfields(ii)%var3d       = .false.

  ii = ii + 1
  sgfields(ii)%varname     = 'zs'
  sgfields(ii)%varunit     = 'm'
  sgfields(ii)%varlong     = 'surface orography'
  sgfields(ii)%var3d       = .false.

  ii = ii + 1
  sgfields(ii)%varname     = 'dpres'
  sgfields(ii)%varunit     = 'Pa'
  sgfields(ii)%varlong     = 'pressure thickness of layer'

  ii = ii + 1
  sgfields(ii)%varname     = 'pres'
  sgfields(ii)%varunit     = 'Pa'
  sgfields(ii)%varlong     = 'layer pressure'

  ii = ii + 1
  sgfields(ii)%varname     = 'ugrd'
  sgfields(ii)%varunit     = 'm/s'
  sgfields(ii)%varlong     = 'zonal wind'

  ii = ii + 1
  sgfields(ii)%varname     = 'vgrd'
  sgfields(ii)%varunit     = 'm/s'
  sgfields(ii)%varlong     = 'meridional wind'

  ii = ii + 1
  sgfields(ii)%varname     = 'temp'
  sgfields(ii)%varunit     = 'deg K'
  sgfields(ii)%varlong     = 'temperature' 

  ii = ii + 1
  sgfields(ii)%varname     = 'spfh'
  sgfields(ii)%varunit     = 'kg/kg'
  sgfields(ii)%varlong     = 'specific humidity' 

  ii = ii + 1
  sgfields(ii)%varname     = 'o3mr'
  sgfields(ii)%varunit     = 'kg/kg'
  sgfields(ii)%varlong     = 'ozone mass mixing ratio' 

  ii = ii + 1
  sgfields(ii)%varname     = 'cwmr'
  sgfields(ii)%varunit     = 'kg/kg'
  sgfields(ii)%varlong     = 'total cloud condensate mixing ratio' 

  if(ii .ne. nsigvars)stop

  end subroutine sigmafield_setup

  subroutine alloc_sigma(im,jm,km)

  integer, intent(in) :: im,jm,km

    allocate(    ug(1:im,1:jm,1:km))
    allocate(    vg(1:im,1:jm,1:km))
    allocate( tempg(1:im,1:jm,1:km))
    allocate(    qg(1:im,1:jm,1:km))
    allocate(   ozg(1:im,1:jm,1:km))
    allocate( cwmrg(1:im,1:jm,1:km))
    allocate(dpresg(1:im,1:jm,1:km))
    allocate( presg(1:im,1:jm,1:km))
    allocate(   psg(1:im,1:jm))
    allocate(   zsg(1:im,1:jm))

    allocate(  sig2d(1:im,1:jm))
    allocate(  sig3d(1:im,1:jm,1:kout))

  end subroutine alloc_sigma

  subroutine dealloc_sigma

    deallocate(    ug)
    deallocate(    vg)
    deallocate( tempg)
    deallocate(    qg)
    deallocate(   ozg)
    deallocate( cwmrg)
    deallocate(dpresg)
    deallocate( presg)
    deallocate(   psg)
    deallocate(   zsg)
    deallocate( sig3d)
    deallocate( sig2d)

  end subroutine dealloc_sigma
end module sigmavars
