module AtmExportFields
 
#include "LocalDefs.F90"
 
  use ESMF
  use AtmInternalFields, only : AtmFieldCount
  use AtmInternalFields, only : AtmField_Definition

  implicit none

  private

  type(AtmField_Definition),   public :: AtmFieldsToExport(AtmFieldCount)
  !-----------------------------------------------------------------------------
  ! Fortran array pointers
  ! Fields for DAtm model 
  ! in Atm exportState
  !-----------------------------------------------------------------------------

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dusfc
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dvsfc

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: zlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: tlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: qlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: ulowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vlowest
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: plowest

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dswrf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: dlwrf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: ulwrf

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: lhtfl
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: shtfl

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vbdsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: vddsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: nbdsf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: nddsf

  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: psurf
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: prate
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: snwrate

  ! called by Cap
  public :: AtmExportFieldsSetUp

  contains

  !-----------------------------------------------------------------------------

  subroutine AtmExportFieldsSetUp

  integer :: ii
  character(len=ESMF_MAXSTR) :: msgString

  ! default values
  AtmFieldsToExport(:)%staggertype = 'center'

    ii = 0
  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_zonal_moment_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dusfc'
    AtmFieldsToExport(ii)%file_varname  = 'dusfc'
    AtmFieldsToExport(ii)%unit_name     = 'N/m2'
    AtmFieldsToExport(ii)%farrayPtr     => dusfc

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_merid_moment_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dvsfc'
    AtmFieldsToExport(ii)%file_varname  = 'dvsfc'
    AtmFieldsToExport(ii)%unit_name     = 'N/m2'
    AtmFieldsToExport(ii)%farrayPtr     => dvsfc

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Zlowest'
    AtmFieldsToExport(ii)%file_varname  = 'hgt_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'K'
    AtmFieldsToExport(ii)%farrayPtr     => zlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_temp_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Tlowest'
    AtmFieldsToExport(ii)%file_varname  = 'tmp_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'K'
    AtmFieldsToExport(ii)%farrayPtr     => tlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_spec_humid_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Qlowest'
    AtmFieldsToExport(ii)%file_varname  = 'spfh_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'kg/kg'
    AtmFieldsToExport(ii)%farrayPtr     => qlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_zonal_wind_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Ulowest'
    AtmFieldsToExport(ii)%file_varname  = 'ugrd_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'm/s'
    AtmFieldsToExport(ii)%farrayPtr     => ulowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_merid_wind_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Vlowest'
    AtmFieldsToExport(ii)%file_varname  = 'vgrd_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'm/s'
    AtmFieldsToExport(ii)%farrayPtr     => vlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_pres_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Plowest'
    AtmFieldsToExport(ii)%file_varname  = 'pres_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'Pa'
    AtmFieldsToExport(ii)%farrayPtr     => plowest

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dswrf'
    AtmFieldsToExport(ii)%file_varname  = 'DSWRF'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => dswrf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_lw_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dlwrf'
    AtmFieldsToExport(ii)%file_varname  = 'DLWRF'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => dlwrf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_up_lw_flx'
    AtmFieldsToExport(ii)%field_name    = 'Ulwrf'
    AtmFieldsToExport(ii)%file_varname  = 'ULWRF'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => ulwrf

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_sensi_heat_flx'
    AtmFieldsToExport(ii)%field_name    = 'Shtfl'
    AtmFieldsToExport(ii)%file_varname  = 'shtfl_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => shtfl

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_laten_heat_flx'
    AtmFieldsToExport(ii)%field_name    = 'Lhtfl'
    AtmFieldsToExport(ii)%file_varname  = 'lhtfl_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => lhtfl

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_vis_dir_flx'
    AtmFieldsToExport(ii)%field_name    = 'Vbdsf'
    AtmFieldsToExport(ii)%file_varname  = 'vbdsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => vbdsf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_vis_dif_flx'
    AtmFieldsToExport(ii)%field_name    = 'Vddsf'
    AtmFieldsToExport(ii)%file_varname  = 'vddsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => vddsf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_ir_dir_flx'
    AtmFieldsToExport(ii)%field_name    = 'Nbdsf'
    AtmFieldsToExport(ii)%file_varname  = 'nbdsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => nbdsf
    
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_ir_dif_flx'
    AtmFieldsToExport(ii)%field_name    = 'Nddsf'
    AtmFieldsToExport(ii)%file_varname  = 'nddsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr     => nddsf

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_pres_height_surface'
    AtmFieldsToExport(ii)%field_name    = 'Psurf'
    AtmFieldsToExport(ii)%file_varname  = 'psurf'
    AtmFieldsToExport(ii)%unit_name     = 'Pa'
    AtmFieldsToExport(ii)%farrayPtr     => psurf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_prec_rate'
    AtmFieldsToExport(ii)%field_name    = 'Prate'
    AtmFieldsToExport(ii)%file_varname  = 'precp'
    AtmFieldsToExport(ii)%unit_name     = 'kg/m2/s'
    AtmFieldsToExport(ii)%farrayPtr     => prate

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_fprec_rate'
    AtmFieldsToExport(ii)%field_name    = 'Snwrate'
    AtmFieldsToExport(ii)%file_varname  = 'fprecp'
    AtmFieldsToExport(ii)%unit_name     = 'kg/m2/s'
    AtmFieldsToExport(ii)%farrayPtr     => snwrate
  
  !-----------------------------------------------------------------------------
  ! check
  !-----------------------------------------------------------------------------
    if(ii .ne. size(AtmFieldsToExport)) &
    call ESMF_LogWrite("ERROR: check # AtmFieldsToExport", ESMF_LOGMSG_INFO)

    call ESMF_LogWrite('AtmFieldsToExport : ', ESMF_LOGMSG_INFO)
    do ii = 1,size(AtmFieldsToExport)
     write(msgString,'(i6,2(a2,a14),a2,a)')ii,'  ',trim(AtmFieldsToExport(ii)%file_varname), &
                                              '  ',trim(AtmFieldsToExport(ii)%field_name), &
                                              '  ',trim(AtmFieldsToExport(ii)%standard_name)
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

  end subroutine AtmExportFieldsSetUp
end module AtmExportFields
