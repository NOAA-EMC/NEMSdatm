module AtmInternalFields

#include "LocalDefs.F90"

  use ESMF

  implicit none

  public :: ChkErr

  character(len=*),parameter :: u_FILE_u = &
     __FILE__

  private

  !from model_configure
                       integer, public :: iatm,jatm,nfhout
                      integer, public  :: scalar_field_count = 0
                      integer, public  :: scalar_field_idx_grid_nx = 0
                      integer, public  :: scalar_field_idx_grid_ny = 0
       real(kind=ESMF_KIND_R8), public :: dt_atmos 
  character(len=ESMF_MAXSTR),   public :: filename_base 
  character(len=ESMF_MAXSTR),   public :: cdate0 
  character(len=ESMF_MAXSTR),   public :: dirpath = 'DATM_INPUT/'
  character(len=ESMF_MAXSTR),   public :: scalar_field_name = ''

  ! the forward and backward timestamps
     real(kind=ESMF_KIND_R8), public :: hfwd, hbak

  ! Set the index type once and for all
  ! Setting the coords and mask values from file assume that the indexing
  ! is global
  type(ESMF_Index_Flag), public :: AtmIndexType = ESMF_INDEX_GLOBAL

  ! Here, the standard_name is used for field connections w/in NUOPC
  ! the field_name is the name of the field internal to the Atm Model
  ! and the file_varname is the name of the variable in the source file
  type, public :: AtmField_Definition
    character(len=64)                                :: standard_name
    character(len=64)                                :: shortname
    character(len=12)                                :: field_name
    character(len=12)                                :: file_varname
    character(len=12)                                :: unit_name
    character(len=10)                                :: staggertype
              logical                                :: isPresent
    ! for export
    real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr
    ! for forcing data, two time levels
    real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: farrayPtr_bak
    real(kind=ESMF_KIND_R4), dimension(:,:), pointer :: farrayPtr_fwd
  end type AtmField_Definition

  ! Field Bundles for Atm model used for time-interpolation of forcing
  type(ESMF_FieldBundle), public :: AtmBundleFwd
  type(ESMF_FieldBundle), public :: AtmBundleBak

  integer, parameter, public :: AtmFieldCount =  6  & !height lowest
                                              +  3  & !swd,lwd,lwup
                                              +  1  & !net lw
                                              +  4  & !momentum,sens,lat
                                              +  4  & !vis,ir,dir,dif
                                              +  3  & !ps,prec
                                              +  2  & !u10m,v10m
                                              +  2    !t2m,q2m

  type(AtmField_Definition), public :: AtmBundleFields(AtmFieldCount)

  integer, public   :: lPet, petCnt 
  integer, public   :: dbug_flag = 0
  ! a diagnostic point to print at
  integer, public   :: iprnt, jprnt
  integer :: icnt

  ! called by AtmInit
  public :: AtmBundleSetUp

  !-----------------------------------------------------------------------------
  ! grid associated stagger_center lats,lons,mask
  ! coords are defined 2dim here, which makes writing with ESMF_ArrayWrite easy
  !-----------------------------------------------------------------------------

  real(kind=ESMF_KIND_R8), public, pointer :: atmlonc(:,:)
  real(kind=ESMF_KIND_R8), public, pointer :: atmlatc(:,:)

  ! stagger_corner lats,lons
  real(kind=ESMF_KIND_R8), public, pointer :: atmlonq(:,:)
  real(kind=ESMF_KIND_R8), public, pointer :: atmlatq(:,:)

  contains

  subroutine AtmBundleSetUp

  type(ESMF_Config)       ::  cfdata

  integer :: ii,nfields,rc
  logical :: lvalue

  character(len=ESMF_MAXSTR) :: msgString
  
  ! default values
  AtmBundleFields(:)%staggertype = 'center'
  ! field availability will be set using data_table.IN
  !AtmBundleFields(:)%isPresent   = .true.
  ! used to set the field name in the exportState = standard_name
  AtmBundleFields(:)%shortname = ' '

    ii = 0
  !-----------------------------------------------------------------------------
  ! the same list of standard_name fields as in ExportState
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_zonal_moment_flx_atm'
    AtmBundleFields(ii)%field_name    = 'Dusfc'
    AtmBundleFields(ii)%file_varname  = 'dusfc'
    AtmBundleFields(ii)%unit_name     = 'N/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_merid_moment_flx_atm'
    AtmBundleFields(ii)%field_name    = 'Dvsfc'
    AtmBundleFields(ii)%file_varname  = 'dvsfc'
    AtmBundleFields(ii)%unit_name     = 'N/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Zlowest'
    AtmBundleFields(ii)%file_varname  = 'hgt_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'K'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_temp_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Tlowest'
    AtmBundleFields(ii)%file_varname  = 'tmp_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'K'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_spec_humid_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Qlowest'
    AtmBundleFields(ii)%file_varname  = 'spfh_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'kg/kg'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_zonal_wind_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Ulowest'
    AtmBundleFields(ii)%file_varname  = 'ugrd_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'm/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_merid_wind_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Vlowest'
    AtmBundleFields(ii)%file_varname  = 'vgrd_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'm/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_pres_height_lowest'
    AtmBundleFields(ii)%field_name    = 'Plowest'
    AtmBundleFields(ii)%file_varname  = 'pres_hyblev1'
    AtmBundleFields(ii)%unit_name     = 'Pa'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_u_wind_height10m'
    AtmBundleFields(ii)%field_name    = 'U10m'
    AtmBundleFields(ii)%file_varname  = 'u10m'
    AtmBundleFields(ii)%unit_name     = 'm/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_v_wind_height10m'
    AtmBundleFields(ii)%field_name    = 'V10m'
    AtmBundleFields(ii)%file_varname  = 'v10m'
    AtmBundleFields(ii)%unit_name     = 'm/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_temp_height2m'
    AtmBundleFields(ii)%field_name    = 'T2m'
    AtmBundleFields(ii)%file_varname  = 't2m'
    AtmBundleFields(ii)%unit_name     = 'K'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_spec_humid_height2m'
    AtmBundleFields(ii)%field_name    = 'Q2m'
    AtmBundleFields(ii)%file_varname  = 'q2m'
    AtmBundleFields(ii)%unit_name     = 'Kg/Kg'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_sw_flx'
    AtmBundleFields(ii)%field_name    = 'Dswrf'
    AtmBundleFields(ii)%file_varname  = 'DSWRF'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_lw_flx'
    AtmBundleFields(ii)%field_name    = 'Dlwrf'
    AtmBundleFields(ii)%file_varname  = 'DLWRF'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_up_lw_flx'
    AtmBundleFields(ii)%field_name    = 'Ulwrf'
    AtmBundleFields(ii)%file_varname  = 'ULWRF'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ! created from DLWRF-ULWRF in AtmForce
    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_net_lw_flx'
    AtmBundleFields(ii)%field_name    = 'Nlwrf'
    AtmBundleFields(ii)%file_varname  = ' '
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_sensi_heat_flx'
    AtmBundleFields(ii)%field_name    = 'Shtfl'
    AtmBundleFields(ii)%file_varname  = 'shtfl_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_laten_heat_flx'
    AtmBundleFields(ii)%field_name    = 'Lhtfl'
    AtmBundleFields(ii)%file_varname  = 'lhtfl_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_sw_vis_dir_flx'
    AtmBundleFields(ii)%field_name    = 'Vbdsf'
    AtmBundleFields(ii)%file_varname  = 'vbdsf_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_sw_vis_dif_flx'
    AtmBundleFields(ii)%field_name    = 'Vddsf'
    AtmBundleFields(ii)%file_varname  = 'vddsf_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_sw_ir_dir_flx'
    AtmBundleFields(ii)%field_name    = 'Nbdsf'
    AtmBundleFields(ii)%file_varname  = 'nbdsf_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()
    
    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_down_sw_ir_dif_flx'
    AtmBundleFields(ii)%field_name    = 'Nddsf'
    AtmBundleFields(ii)%file_varname  = 'nddsf_ave'
    AtmBundleFields(ii)%unit_name     = 'W/m2'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

  !-----------------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'inst_pres_height_surface'
    AtmBundleFields(ii)%field_name    = 'Psurf'
    AtmBundleFields(ii)%file_varname  = 'psurf'
    AtmBundleFields(ii)%unit_name     = 'Pa'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_prec_rate'
    AtmBundleFields(ii)%field_name    = 'Prate'
    AtmBundleFields(ii)%file_varname  = 'precp'
    AtmBundleFields(ii)%unit_name     = 'kg/m2/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    ii = ii + 1
    AtmBundleFields(ii)%standard_name = 'mean_fprec_rate'
    AtmBundleFields(ii)%field_name    = 'Snwrate'
    AtmBundleFields(ii)%file_varname  = 'fprecp'
    AtmBundleFields(ii)%unit_name     = 'kg/m2/s'
    AtmBundleFields(ii)%farrayPtr_bak => null()
    AtmBundleFields(ii)%farrayPtr_fwd => null()
    AtmBundleFields(ii)%farrayPtr     => null()

    if(ii .ne. size(AtmBundleFields)) &
    call ESMF_LogWrite("ERROR: check # AtmBundleFields", ESMF_LOGMSG_INFO)

  !-----------------------------------------------------------------------------
  ! get the input availability from the datm_data_table 
  !-----------------------------------------------------------------------------

    cfdata=ESMF_ConfigCreate(rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ConfigLoadFile(config=cfdata ,filename='datm_data_table' ,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    nfields = size(AtmBundleFields)
     icnt = 0
    do ii = 1,nfields
     call ESMF_ConfigGetAttribute(config=cfdata, &
                                  value=lvalue, &
                                  label=trim(AtmBundleFields(ii)%standard_name),rc=rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
     AtmBundleFields(ii)%isPresent=lvalue
     icnt = icnt + 1
    enddo
    if(icnt .ne. nfields)then
      call ESMF_LogWrite('Missing input fields in datm_data_table', ESMF_LOGMSG_INFO)
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

  !-----------------------------------------------------------------------------
  ! check
  !-----------------------------------------------------------------------------
  
    call ESMF_LogWrite('AtmBundleFields : ', ESMF_LOGMSG_INFO)
    do ii = 1,size(AtmBundleFields)
     write(msgString,'(i6,2(a2,a14),a2,a30,a2,l6)')ii, &
                                              '  ',trim(AtmBundleFields(ii)%file_varname), &
                                              '  ',trim(AtmBundleFields(ii)%field_name), &
                                              '  ',trim(AtmBundleFields(ii)%standard_name), &
                                              '  ',AtmBundleFields(ii)%isPresent
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

  end subroutine AtmBundleSetUp

  logical function ChkErr(rc, line, file)
    integer, intent(in) :: rc
    integer, intent(in) :: line
    character(len=*), intent(in) :: file
    integer :: lrc
    chkerr = .false.
    lrc = rc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)) then
     chkerr = .true.
    endif
  end function ChkErr

end module AtmInternalFields
