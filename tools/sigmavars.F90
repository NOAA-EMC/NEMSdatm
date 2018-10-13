module sigmavars

  use nemsio_module

  implicit none

  integer, parameter :: maxsigvars = 20

  real(kind=nemsio_realkind), allocatable, dimension(:,:,:) :: vcoord

  real(kind=nemsio_realkind), allocatable, dimension(:,:,:) :: ug,vg,tempg,qg,ozg,cwmrg,dpresg,presg
  real(kind=nemsio_realkind), allocatable, dimension(:,:)   :: psg,zsg

  real(kind=4), allocatable, dimension(:,:,:)   :: sig3d

  type SigmaFieldsDefs
    character(len=12)                           :: varname
    character(len=12)                           :: varunit
    character(len=60)                           :: varlong
  end type SigmaFieldsDefs

  type(SigmaFieldsDefs) :: sgfields(maxsigvars)
#ifdef debug
  ! number of vertical levels to output for debugging
  integer, parameter            :: kout = 10
  real(kind=4), dimension(kout) :: zout
#endif
  contains 

  subroutine sigmafield_setup

  use param

  ! see gfs_nemsiotonc_3d in j.whitaker's python version
  ! checked with Jun Wang and confirmed that FV3 is actual temperature, not 
  ! virtual temperature as listed in j.whitaker's python version
  integer :: idx = 0

  !default
  sgfields(:)%varname = " "
  sgfields(:)%varunit = " "
  sgfields(:)%varlong = " "

  idx = idx + 1
  sgfields(idx)%varname     = 'ps'
  sgfields(idx)%varunit     = 'Pa'
  sgfields(idx)%varlong     = 'surface pressure'

  idx = idx + 1
  sgfields(idx)%varname     = 'zs'
  sgfields(idx)%varunit     = 'm'
  sgfields(idx)%varlong     = 'surface orography'

  idx = idx + 1
  sgfields(idx)%varname     = 'dpres'
  sgfields(idx)%varunit     = 'Pa'
  sgfields(idx)%varlong     = 'pressure thickness of layer'

  idx = idx + 1
  sgfields(idx)%varname     = 'pres'
  sgfields(idx)%varunit     = 'Pa'
  sgfields(idx)%varlong     = 'layer pressure'

  idx = idx + 1
  sgfields(idx)%varname     = 'ugrd'
  sgfields(idx)%varunit     = 'm/s'
  sgfields(idx)%varlong     = 'zonal wind'

  idx = idx + 1
  sgfields(idx)%varname     = 'vgrd'
  sgfields(idx)%varunit     = 'm/s'
  sgfields(idx)%varlong     = 'meridional wind'

  idx = idx + 1
  sgfields(idx)%varname     = 'temp'
  sgfields(idx)%varunit     = 'deg K'
  sgfields(idx)%varlong     = 'temperature' 

  idx = idx + 1
  sgfields(idx)%varname     = 'spfh'
  sgfields(idx)%varunit     = 'kg/kg'
  sgfields(idx)%varlong     = 'specific humidity' 

  idx = idx + 1
  sgfields(idx)%varname     = 'o3mr'
  sgfields(idx)%varunit     = 'kg/kg'
  sgfields(idx)%varlong     = 'ozone mass mixing ratio' 

  idx = idx + 1
  sgfields(idx)%varname     = 'cwmr'
  sgfields(idx)%varunit     = 'kg/kg'
  sgfields(idx)%varlong     = 'total cloud condensate mixing ratio' 

  nsigvars = idx
  if(idx .gt. maxsigvars)stop

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

#ifdef debug
    allocate(  sig3d(1:im,1:jm,1:kout))
#endif

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

#ifdef debug
    deallocate( sig3d)
#endif

  end subroutine dealloc_sigma
end module sigmavars
