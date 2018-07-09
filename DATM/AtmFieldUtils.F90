module AtmFieldUtils
  !-----------------------------------------------------------------------------
  ! Field utility module
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  use AtmFields

  implicit none

  private

! Here, the standard_name is used for field connections w/in NUOPC
! the field_name is the name of the field internal to the Atm Model
! and the file_varname is the name of the variable in the source file

  type, public :: Atm_Field_Definition
    character(len=64)                                :: standard_name
    character(len=12)                                :: field_name
    character(len=12)                                :: file_varname
    character(len=12)                                :: unit_name
    character(len=10)                                :: staggertype
    logical                                          :: from3d
    real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr => null()
  end type Atm_Field_Definition
   
  integer, parameter :: AtmFieldCount =  7  & !CICE5 only
                                      +  5  & !MOM6 only 
                                      +  6    !CICE5+MOM6  

  type(Atm_Field_Definition),   public :: AtmFieldsToExport(AtmFieldCount)
  ! Mediator requires land_mask to be imported?
  type(Atm_Field_Definition),   public :: AtmFieldsToImport(1)

  ! called by Cap
  public :: AtmFieldsSetUp
  public :: AtmFieldsAdvertise, AtmFieldsRealize
  public :: AtmFieldCheck
  public :: AtmFieldDump

  contains

  !-----------------------------------------------------------------------------

  subroutine AtmFieldsSetUp

  integer :: ii
  character(len=ESMF_MAXSTR) :: msgString

  ! default values
  AtmFieldsToExport(:)%from3d      = .false.
  AtmFieldsToExport(:)%staggertype = 'center'

  AtmFieldsToImport(:)%staggertype = 'center'

    ii = 0
  !-----------------------------------------------------------------------------
  !Atm Import Fields (req by Mediator ?)
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToImport(ii)%standard_name = 'land_mask'
    AtmFieldsToImport(ii)%field_name    = 'LandMask'
    AtmFieldsToImport(ii)%file_varname  = ' '
    AtmFieldsToImport(ii)%unit_name     = '1'
    AtmFieldsToImport(ii)%farrayPtr => land_mask

    ii = 0
  !-----------------------------------------------------------------------------
  !Atm Export Fields used by CICE only
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Zlowest'
    AtmFieldsToExport(ii)%file_varname  = 'hgt_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'K'
    AtmFieldsToExport(ii)%farrayPtr => zlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_temp_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Tlowest'
    AtmFieldsToExport(ii)%file_varname  = 'tmp_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'K'
    AtmFieldsToExport(ii)%farrayPtr => tlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_spec_humid_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Qlowest'
    AtmFieldsToExport(ii)%file_varname  = 'spfh_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'kg/kg'
    AtmFieldsToExport(ii)%farrayPtr => qlowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_zonal_wind_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Ulowest'
    AtmFieldsToExport(ii)%file_varname  = 'ugrd_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'm/s'
    AtmFieldsToExport(ii)%farrayPtr => ulowest

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_merid_wind_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Vlowest'
    AtmFieldsToExport(ii)%file_varname  = 'vgrd_hyblev1'
    AtmFieldsToExport(ii)%unit_name     = 'm/s'
    AtmFieldsToExport(ii)%farrayPtr => vlowest

  ! ?? does this exist in output
  ! ?? CICE needs it to calc air density lowest
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_pres_height_lowest'
    AtmFieldsToExport(ii)%field_name    = 'Plowest'
  ! temporary work around
    AtmFieldsToExport(ii)%file_varname  = 'pres'
  !  AtmFieldsToExport(ii)%file_varname  = 'preshy'
  !  AtmFieldsToExport(ii)%from3d        = .true.
    AtmFieldsToExport(ii)%unit_name     = 'Pa'
    AtmFieldsToExport(ii)%farrayPtr => plowest
  ! ??

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_lw_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dlwrf'
    AtmFieldsToExport(ii)%file_varname  = 'dlwrf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => dlwrf

  !-----------------------------------------------------------------------------
  !Atm Export Fields used by MOM6 only
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_zonal_moment_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dusfc'
    AtmFieldsToExport(ii)%file_varname  = 'uflx_ave'
    AtmFieldsToExport(ii)%unit_name     = 'N/m2'
    AtmFieldsToExport(ii)%farrayPtr => dusfc

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_merid_moment_flx'
    AtmFieldsToExport(ii)%field_name    = 'Dvsfc'
    AtmFieldsToExport(ii)%file_varname  = 'vflx_ave'
    AtmFieldsToExport(ii)%unit_name     = 'N/m2'
    AtmFieldsToExport(ii)%farrayPtr => dvsfc

    !?? need mean up lw to provide mean net lw
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_up_lw_flx'
    AtmFieldsToExport(ii)%field_name    = 'Ulwrf'
    AtmFieldsToExport(ii)%file_varname  = 'ulwrf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => ulwrf
    !??

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_sensi_heat_flx'
    AtmFieldsToExport(ii)%field_name    = 'Shtfl'
    AtmFieldsToExport(ii)%file_varname  = 'shtfl_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => shtfl

    !?? mean evap rate
    !ii = ii + 1
    !AtmFieldsToExport(ii)%standard_name = 'mean_'
    !AtmFieldsToExport(ii)%field_name    = ''
    !AtmFieldsToExport(ii)%file_varname  = XXX
    !AtmFieldsToExport(ii)%unit_name     = ''
    !AtmFieldsToExport(ii)%farrayPtr => XXX
    !??

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'inst_pres_height_surface'
    AtmFieldsToExport(ii)%field_name    = 'Psurf'
    AtmFieldsToExport(ii)%file_varname  = 'pres'
    AtmFieldsToExport(ii)%unit_name     = 'Pa'
    AtmFieldsToExport(ii)%farrayPtr => psurf

  !-----------------------------------------------------------------------------
  !Atm Export Fields used by CICE and MOM6
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_vis_dir_flx'
    AtmFieldsToExport(ii)%field_name    = 'Vbdsf'
    AtmFieldsToExport(ii)%file_varname  = 'vbdsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => vbdsf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_vis_dif_flx'
    AtmFieldsToExport(ii)%field_name    = 'Vddsf'
    AtmFieldsToExport(ii)%file_varname  = 'vddsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => vddsf

    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_ir_dir_flx'
    AtmFieldsToExport(ii)%field_name    = 'Nbdsf'
    AtmFieldsToExport(ii)%file_varname  = 'nbdsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => nbdsf
    
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_down_sw_ir_dif_flx'
    AtmFieldsToExport(ii)%field_name    = 'Nddsf'
    AtmFieldsToExport(ii)%file_varname  = 'nddsf_ave'
    AtmFieldsToExport(ii)%unit_name     = 'W/m2'
    AtmFieldsToExport(ii)%farrayPtr => nddsf

  ! ?? liquid only
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_prec_rate'
    AtmFieldsToExport(ii)%field_name    = 'Prate'
    AtmFieldsToExport(ii)%file_varname  = 'prate_ave'
    AtmFieldsToExport(ii)%unit_name     = 'kg/m2/s'
    AtmFieldsToExport(ii)%farrayPtr => prate
  ! ??

  ! ?? snow 
  ! missing in file, use liquid as workaround
    ii = ii + 1
    AtmFieldsToExport(ii)%standard_name = 'mean_fprec_rate'
    AtmFieldsToExport(ii)%field_name    = 'Snwrate'
    AtmFieldsToExport(ii)%file_varname  = 'prate_ave'
    AtmFieldsToExport(ii)%unit_name     = 'kg/m2/s'
    AtmFieldsToExport(ii)%farrayPtr => snwrate
  ! ??

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

  end subroutine AtmFieldsSetUp

  !-----------------------------------------------------------------------------

  subroutine AtmFieldsAdvertise(state, field_defs, rc)

    type(ESMF_State),           intent(inout)  :: state
    type(Atm_Field_Definition), intent(inout)  :: field_defs(:)
    integer,                    intent(  out)  :: rc

  ! Local items
    integer :: ii, nfields

    rc = ESMF_SUCCESS

  ! number of items
    nfields = size(field_defs)
  !print *,'found nfields = ',nfields,' to advertise ',field_defs%field_name

    do ii = 1,nfields
      call NUOPC_Advertise(state, &
        StandardName=trim(field_defs(ii)%standard_name), &
                name=trim(field_defs(ii)%field_name), &
                  rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine AtmFieldsAdvertise

  !-----------------------------------------------------------------------------

  subroutine AtmFieldsRealize(state, grid, field_defs, tag, rc)

    type(ESMF_State),           intent(inout)  :: state
    type(ESMF_Grid),            intent(in   )  :: grid
    type(Atm_Field_Definition), intent(inout)  :: field_defs(:)
    character(len=*),           intent(in   )  :: tag
    integer,                    intent(out  )  :: rc

    type(ESMF_ArraySpec)       :: arrayspecR8
    type(ESMF_Field)           :: field
    type(ESMF_StaggerLoc)      :: staggerloc
    character(len=ESMF_MAXSTR) :: msgString

    integer              :: ii,nfields
    logical              :: connected

    rc = ESMF_SUCCESS
    call ESMF_LogWrite("User routine AtmFieldsRealize "//trim(tag)//" started", ESMF_LOGMSG_INFO)
                  
    nfields=size(field_defs)
    !print *,'found nfields = ',nfields,' to realize ',field_defs%field_name

    call ESMF_ArraySpecSet(arrayspecR8, rank=2, typekind=ESMF_TYPEKIND_R8, rc=rc)

    do  ii = 1,nfields
     ! they should all be center ;-)
     if(field_defs(ii)%staggertype == 'center')staggerloc = ESMF_STAGGERLOC_CENTER

      field = ESMF_FieldCreate(grid=grid, &
                               arrayspec=arrayspecR8, &
                               indexflag=AtmIndexType, &
                               staggerloc=staggerloc, &
                               name=trim(field_defs(ii)%field_name), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      connected = NUOPC_IsConnected(state, fieldName=trim(field_defs(ii)%field_name), rc=rc)
      if(     connected)write(msgString,*)'Field '//trim(field_defs(ii)%field_name)//' is connected '
      if(.not.connected)write(msgString,*)'Field '//trim(field_defs(ii)%field_name)//' is NOT connected '
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

      call NUOPC_Realize(state, field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_FieldGet(field, farrayPtr=field_defs(ii)%farrayPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      field_defs(ii)%farrayPtr = 0.0
      !do j = lbound(field_defs(ii)%farrayPtr,2),ubound(field_defs(ii)%farrayPtr,2)
      ! do i = lbound(field_defs(ii)%farrayPtr,1),ubound(field_defs(ii)%farrayPtr,1)
      !  field_defs(ii)%farrayPtr(i,j) = 0.0
      ! enddo
      !enddo
    enddo

    call ESMF_LogWrite("User routine AtmFieldsRealize "//trim(tag)//" finished", ESMF_LOGMSG_INFO)
  end subroutine AtmFieldsRealize

  !-------------------------------------------------------------------------------------

  subroutine AtmFieldCheck(importState, exportState, tag, rc)

  type(ESMF_State)              :: importState, exportState
  character(len=*), intent( in) :: tag
           integer, intent(out) :: rc

  type(ESMF_Field) :: field

  integer :: ii, nfields
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  ! Check Fields
  !-----------------------------------------------------------------------------

  nfields = size(AtmFieldsToExport)
  do ii = 1,nfields
    call ESMF_StateGet(exportState, &
                       itemName=trim(AtmFieldsToExport(ii)%field_name), &
                       field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field, farrayPtr=AtmFieldsToExport(ii)%farrayPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write (msgString,*)trim(tag), ' AtmFieldsToExport ',&
                       trim(AtmFieldsToExport(ii)%field_name),' min,max,sum ',&
                       minval(real(AtmFieldsToExport(ii)%farrayPtr,4)),&
                       maxval(real(AtmFieldsToExport(ii)%farrayPtr,4)),&
                          sum(real(AtmFieldsToExport(ii)%farrayPtr,4))
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
   enddo

  end subroutine AtmFieldCheck

  !-----------------------------------------------------------------------------

  subroutine AtmFieldDump(importState, exportState, tag, iicnt, rc)

  type(ESMF_State)              :: importState
  type(ESMF_State)              :: exportState
  character(len=*), intent( in) :: tag
           integer, intent( in) :: iicnt
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Field)                 :: field

  integer :: ii,nfields
  character(len=ESMF_MAXSTR) :: varname
  character(len=ESMF_MAXSTR) :: fname
  character(len=ESMF_MAXSTR) :: filename
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmFieldDump started", ESMF_LOGMSG_INFO)

  ! Atm variables in exportState
  nfields = size(AtmFieldsToExport)
  do ii = 1,nfields
   call ESMF_StateGet(exportState, &
                      itemName = trim(AtmFieldsToExport(ii)%field_name), &
                      field=field,rc=rc)
    varname = trim(AtmFieldsToExport(ii)%field_name)
    if(trim(tag) .eq. 'before AtmRun')filename = 'field_atm_exportb_'//trim(varname)//'.nc'
    if(trim(tag) .eq.  'after AtmRun')filename = 'field_atm_exporta_'//trim(varname)//'.nc'

    write(msgString, *)'Writing exportState field ',trim(varname),' to ',trim(filename),' iicnt = ',iicnt
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_FieldWrite(field, &
                         fileName=filename, &
                         variableName=varname, &
                         overwrite=.true., &
                         timeslice=iicnt,rc=rc)
     if(iicnt .eq. 1)then
      write(msgString, *)'Writing exportState field ',trim(varname),' to ',trim(filename)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
     endif
  enddo
  call ESMF_LogWrite("User routine AtmFieldDump finished", ESMF_LOGMSG_INFO)

  end subroutine AtmFieldDump

end module AtmFieldUtils
