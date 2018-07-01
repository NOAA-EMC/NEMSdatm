module AtmFields
 
#include "LocalDefs.F90"
 
  use ESMF

  implicit none

  private

  ! Set the index type once and for all
  ! Setting the coords and mask values from file assume that the indexing
  ! is global
  type(ESMF_Index_Flag), public :: AtmIndexType = ESMF_INDEX_GLOBAL
  !type(ESMF_Index_Flag), public :: AtmIndexType = ESMF_INDEX_DELOCAL

  integer, public            :: imin_e, imax_e, jmin_e, jmax_e
  integer, public            :: imin_t, imax_t, jmin_t, jmax_t
  integer, public            :: imin_c, imax_c, jmin_c, jmax_c
#ifdef lowres
  integer, parameter, public :: iatm =  192, jatm =   96
  ! for now, hardwired
  character(len=ESMF_MAXSTR), public ::  dirpath = &
       '/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/DATM-MOM6-CICE5/master-20180627/ATM/'
  character(len=ESMF_MAXSTR), public :: gridfile = &
       'gdas.t18z.sfcf.2018041400.nc'
#endif
#ifdef hires
  integer, parameter, public :: iatm = 3072, jatm = 1536
  ! for now, hardwired
  character(len=ESMF_MAXSTR), public ::  dirpath = &
       '/scratch4/NCEPDEV/ocean/save/Denise.Worthen/NEMS_INPUT0.1/DAtm/C768/'
  character(len=ESMF_MAXSTR), public :: gridfile = &
       'gdas.t18z.sfcf.2018041400.nc'
#endif
  integer, public :: lPet

  ! grid associated stagger_center lats,lons,mask
  ! coords are defined 2dim here, which makes writing with ESMF_ArrayWrite easy
  real(kind=ESMF_KIND_R8), pointer, public, save :: atmlonc(:,:)
  real(kind=ESMF_KIND_R8), pointer, public, save :: atmlatc(:,:)
  ! a land mask field that the med seems to require
  real(kind=ESMF_KIND_R8), pointer, public, save :: land_mask(:,:)

  ! stagger_corner lats,lons
  real(kind=ESMF_KIND_R8), pointer, public, save :: atmlonq(:,:)
  real(kind=ESMF_KIND_R8), pointer, public, save :: atmlatq(:,:)
  !-----------------------------------------------------------------------------
  ! Fortran array pointers
  ! Fields for DAtm model 
  ! in Atm exportState
  !-----------------------------------------------------------------------------
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: zlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: tlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: qlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: ulowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: plowest

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dlwrf

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dusfc
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dvsfc
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: ulwrf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: shtfl
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: psurf

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vbdsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vddsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: nbdsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: nddsf

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: prate
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: snwrate

end module AtmFields
