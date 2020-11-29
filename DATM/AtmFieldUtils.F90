module AtmFieldUtils
  !-----------------------------------------------------------------------------
  ! Field utility module
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  use AtmInternalFields

  implicit none

  private

  ! called by Cap
  public :: AtmFieldsAdvertise, AtmFieldsRealize
  public :: AtmFieldCheck
  public :: AtmFieldDump
  public :: State_SetScalar

  ! called by AtmInit, AtmRun
  public :: AtmForceFwd2Bak, AtmBundleCheck
  public :: AtmBundleIntp

  character(len=*),parameter :: u_FILE_u = &
     __FILE__

  contains

  !-----------------------------------------------------------------------------

  subroutine AtmFieldsAdvertise(state, field_defs, rc)

    type(ESMF_State),          intent(inout)  :: state
    type(AtmField_Definition), intent(inout)  :: field_defs(:)
    integer,                   intent(  out)  :: rc

  ! Local items
    integer :: ii, nfields

    rc = ESMF_SUCCESS

  ! number of items
   nfields = size(field_defs)
    
    ! create a shortname == standard_name for the fields in the state
    do ii = 1,nfields
       field_defs(ii)%shortname = trim(trim(field_defs(ii)%standard_name))

      call ESMF_LogWrite("Advertise Field "// &
       trim(field_defs(ii)%standard_name)//" : "// &
       trim(field_defs(ii)%shortname), ESMF_LOGMSG_INFO)

      call NUOPC_Advertise(state, &
        StandardName=trim(field_defs(ii)%standard_name), &
                name=trim(field_defs(ii)%shortname), &
                  rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
    enddo

    ! advertise scalars for cmeps
    if (len_trim(scalar_field_name) > 0) then
     call NUOPC_Advertise(state, standardName=trim(scalar_field_name), name=trim(scalar_field_name), rc=rc)
     call ESMF_LogWrite("Advertise Field "// trim(scalar_field_name), ESMF_LOGMSG_INFO)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

  end subroutine AtmFieldsAdvertise

  !-----------------------------------------------------------------------------

  subroutine AtmFieldsRealize(state, grid, field_defs, tag, rc)

    type(ESMF_State),          intent(inout)  :: state
    type(ESMF_Grid),           intent(in   )  :: grid
    type(AtmField_Definition), intent(inout)  :: field_defs(:)
    character(len=*),          intent(in   )  :: tag
    integer,                   intent(out  )  :: rc

    type(ESMF_ArraySpec)       :: arrayspecR8
    type(ESMF_Field)           :: field
    type(ESMF_StaggerLoc)      :: staggerloc
    character(len=ESMF_MAXSTR) :: msgString
    integer                    :: ii,nfields

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
                               name=trim(field_defs(ii)%shortname), rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_Realize(state, field=field, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_FieldGet(field, farrayPtr=field_defs(ii)%farrayPtr, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      ! initialize
      field_defs(ii)%farrayPtr = 0.0_ESMF_KIND_R8

      msgString = trim(tag)//" Field "// trim(field_defs(ii)%shortname) // " is connected on root pe"
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

    ! realize scalars for cmeps
    if (len_trim(scalar_field_name) > 0) then
      call SetScalarField(field, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_Realize(state, field=field, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      msgString = trim(tag)//" Field "// trim(scalar_field_name) // " is connected on root pe"
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endif

    call ESMF_LogWrite("User routine AtmFieldsRealize "//trim(tag)//" finished", ESMF_LOGMSG_INFO)
  end subroutine AtmFieldsRealize

  !-------------------------------------------------------------------------------------

  subroutine AtmFieldCheck(exportState, tag, rc)

  use AtmInternalFields, only : iprnt, jprnt

  type(ESMF_State)              :: exportState
  character(len=*), intent( in) :: tag
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Field)           :: field
  character(len=ESMF_MAXSTR) :: msgString

  integer :: ii, nfields, ijloc(2)

  ! Initialize return code
  rc = ESMF_SUCCESS

  !-----------------------------------------------------------------------------
  ! Check Fields
  !-----------------------------------------------------------------------------

  nfields = size(AtmBundleFields)
  do ii = 1,nfields
    call ESMF_StateGet(exportState, &
                       itemName=trim(AtmBundleFields(ii)%shortname), &
                       field=field, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(field, farrayPtr=AtmBundleFields(ii)%farrayPtr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !write (msgString,*)trim(tag), ' AtmBundleFields ',&
    !                   trim(AtmBundleFields(ii)%shortname),'  ',&
    !                   real(AtmBundleFields(ii)%farrayPtr(iprnt,jprnt),4)
    !call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    !ijloc = maxloc(abs(AtmBundleFields(ii)%farrayPtr))
    !write (msgString,*)trim(tag), ' AtmBundleFields ',&
    !                   trim(AtmBundleFields(ii)%shortname),' maxloc ',&
    !                   ijloc(1),ijloc(2),&
    !                   real(AtmBundleFields(ii)%farrayPtr(ijloc(1),ijloc(2)),4)
    !call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    write (msgString,*)trim(tag), ' AtmBundleFields ',&
                       trim(AtmBundleFields(ii)%shortname),' min,max,sum ',&
                       minval(real(AtmBundleFields(ii)%farrayPtr,4)),&
                       maxval(real(AtmBundleFields(ii)%farrayPtr,4)),&
                          sum(real(AtmBundleFields(ii)%farrayPtr,4))
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
   enddo

  end subroutine AtmFieldCheck

  !-----------------------------------------------------------------------------

  subroutine AtmFieldDump(exportState, filename, rc)

  type(ESMF_State)              :: exportState
  character(len=*), intent( in) :: filename
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Field)                 :: field

  integer :: ii,nfields
  character(len=ESMF_MAXSTR) :: varname
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmFieldDump started", ESMF_LOGMSG_INFO)

  ! Atm variables in exportState
  nfields = size(AtmBundleFields)
  do ii = 1,nfields
   call ESMF_StateGet(exportState, &
                      itemName = trim(AtmBundleFields(ii)%shortname), &
                      field=field,rc=rc)
    varname = trim(AtmBundleFields(ii)%shortname)

    write(msgString, '(a,i6)')'Writing exportState field '//trim(varname)//' to ' &
                            //trim(filename)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    call ESMF_FieldWrite(field, &
                         fileName =trim(filename), &
                         timeslice=1, &
                         overwrite=.true., rc=rc)
  enddo
  call ESMF_LogWrite("User routine AtmFieldDump finished", ESMF_LOGMSG_INFO)

  end subroutine AtmFieldDump

  !-----------------------------------------------------------------------------

  subroutine AtmForceFwd2Bak(rc)

  use AtmInternalFields, only : hfwd, hbak
  use AtmInternalFields, only : AtmBundleFields
  use AtmInternalFields, only : AtmBundleFwd, AtmBundleBak
  
  integer, intent(out) :: rc

  ! Local variables
  integer :: ii,nfields

  type(ESMF_Field)           :: fieldfwd, fieldbak
  character(len=ESMF_MAXSTR) :: fnamefwd, fnamebak
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmForceFwd2Bak started", ESMF_LOGMSG_INFO)

  call ESMF_FieldBundleGet(AtmBundleFwd,fieldCount=nfields,rc=rc)
  do ii = 1,nfields
    fnamefwd = trim(AtmBundleFields(ii)%field_name)//'_fwd'
    fnamebak = trim(AtmBundleFields(ii)%field_name)//'_bak'

    call ESMF_FieldBundleGet(AtmBundleFwd, &
                             fieldName=trim(fnamefwd), &
                             field=fieldfwd,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldBundleGet(AtmBundleBak, &
                             fieldName=trim(fnamebak), &
                             field=fieldbak,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! copy the fwd fields to the bak fields
    ! this function is defined (fieldout,fieldin)
    call ESMF_FieldCopy(fieldbak, fieldfwd, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
  enddo

  call ESMF_LogWrite("User routine AtmForceFwd2Bak finished", ESMF_LOGMSG_INFO)

  end subroutine AtmForceFwd2Bak

  !-----------------------------------------------------------------------------

  subroutine AtmBundleCheck(tag,rc)

  use AtmInternalFields, only : iprnt, jprnt
  use AtmInternalFields, only : AtmBundleFields
  use AtmInternalFields, only : AtmBundleFwd, AtmBundleBak
  
    character(len=*), intent(in ) :: tag
             integer, intent(out) :: rc

  ! Local variables
  integer :: ii,nfields

  type(ESMF_Field)           :: fieldfwd, fieldbak
  character(len=ESMF_MAXSTR) :: fnamefwd, fnamebak
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmBundleCheck "//trim(tag)//" started", ESMF_LOGMSG_INFO)

  call ESMF_FieldBundleGet(AtmBundleFwd,fieldCount=nfields,rc=rc)
  do ii = 1,nfields
    fnamefwd = trim(AtmBundleFields(ii)%field_name)//'_fwd'
    fnamebak = trim(AtmBundleFields(ii)%field_name)//'_bak'

    call ESMF_FieldBundleGet(AtmBundleBak, &
                             fieldName=trim(fnamebak), &
                             field=fieldbak,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
  
    call ESMF_FieldGet(fieldbak,farrayPtr=AtmBundleFields(ii)%farrayPtr_bak,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldBundleGet(AtmBundleFwd, &
                             fieldName=trim(fnamefwd), &
                             field=fieldfwd,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(fieldfwd,farrayPtr=AtmBundleFields(ii)%farrayPtr_fwd,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
 
     write(msgString,'(i4,2(a2,a12),2f14.5)')ii,'  ',trim(fnamebak), &
                                                '  ',trim(fnamefwd), &
                                AtmBundleFields(ii)%farrayPtr_bak(iprnt,jprnt), &
                                AtmBundleFields(ii)%farrayPtr_fwd(iprnt,jprnt)
    !                            AtmBundleFields(ii)%farrayPtr_bak(152,60), &
    !                            AtmBundleFields(ii)%farrayPtr_fwd(152,60)
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
  enddo

  call ESMF_LogWrite("User routine AtmBundleCheck finished", ESMF_LOGMSG_INFO)

  end subroutine AtmBundleCheck

  !-----------------------------------------------------------------------------

  subroutine AtmBundleIntp(gcomp, exportState, externalClock, hour, rc)

  use AtmInternalFields, only : hfwd,hbak
  use AtmInternalFields, only : iprnt,jprnt
  use AtmInternalFields, only : AtmBundleFields
  use AtmInternalFields, only : AtmBundleFwd, AtmBundleBak

  type(ESMF_GridComp)        :: gcomp
  type(ESMF_State)           :: exportState
  type(ESMF_Clock)           :: externalClock

  real(ESMF_KIND_R8), intent(in) :: hour
            integer, intent(out) :: rc

  ! Local variables
  integer :: ii,nfields,ijloc(2)

  type(ESMF_Field)            :: fieldbak, field, fieldfwd
  character(len=ESMF_MAXSTR)  :: fnamefwd, fnamebak
  character(len=ESMF_MAXSTR)  :: msgString
  real(kind=ESMF_KIND_R8)     :: wf, wb, wtot

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmBundleIntp  started", ESMF_LOGMSG_INFO)

  nfields = size(AtmBundleFields)
  do ii = 1,nfields
    call ESMF_StateGet(exportState, &
                       itemName=trim(AtmBundleFields(ii)%shortname), &
                       field=field, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(field, farrayPtr=AtmBundleFields(ii)%farrayPtr, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    AtmBundleFields(ii)%farrayPtr = 0.0_ESMF_KIND_R8

    ! get the corresponding _fwd and _bak fields
    fnamefwd = trim(AtmBundleFields(ii)%field_name)//'_fwd'
    fnamebak = trim(AtmBundleFields(ii)%field_name)//'_bak'

    call ESMF_FieldBundleGet(AtmBundleFwd, &
                             fieldName=trim(fnamefwd), &
                             field=fieldfwd,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(fieldfwd,farrayPtr=AtmBundleFields(ii)%farrayPtr_fwd,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldBundleGet(AtmBundleBak, &
                             fieldName=trim(fnamebak), &
                             field=fieldbak,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_FieldGet(fieldbak,farrayPtr=AtmBundleFields(ii)%farrayPtr_bak,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

   !special case at initialization
   if(int(hour,4) .eq. 0)then
    AtmBundleFields(ii)%farrayPtr = real(AtmBundleFields(ii)%farrayPtr_bak,8)
   else
    ! interpolate in time
      wf = hfwd - hour
      wb = hour - hbak
    wtot = wf+wb
    AtmBundleFields(ii)%farrayPtr = (wf*real(AtmBundleFields(ii)%farrayPtr_bak,8) &
                                  +  wb*real(AtmBundleFields(ii)%farrayPtr_fwd,8))/wtot
   endif !hour=0

    !ijloc = maxloc(abs(AtmBundleFields(ii)%farrayPtr))
    !write (msgString,*)' AtmIntp ',&
    !                   trim(AtmBundleFields(ii)%field_name),' maxloc ',&
    !                   ijloc(1),ijloc(2),&
    !                   real(AtmBundleFields(ii)%farrayPtr(ijloc(1),ijloc(2)),4),&
    !                   real(AtmBundleFields(ii)%farrayPtr_bak(ijloc(1),ijloc(2)),4),&
    !                   real(AtmBundleFields(ii)%farrayPtr_fwd(ijloc(1),ijloc(2)),4)
    !call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    if(dbug > 0)then
     write (msgString,*)' AtmIntp ',&
                        trim(AtmBundleFields(ii)%shortname),&
                        real(AtmBundleFields(ii)%farrayPtr(iprnt,jprnt),4),&
                        real(AtmBundleFields(ii)%farrayPtr_bak(iprnt,jprnt),4),&
                        real(AtmBundleFields(ii)%farrayPtr_fwd(iprnt,jprnt),4)
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    end if

    !write (msgString,*)' AtmBundleFields ',&
    !                   trim(AtmBundleFields(ii)%field_name),' min,max,sum ',&
    !                   minval(real(AtmBundleFields(ii)%farrayPtr,4)),&
    !                   maxval(real(AtmBundleFields(ii)%farrayPtr,4)),&
    !                      sum(real(AtmBundleFields(ii)%farrayPtr,4))
    ! call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
   enddo

  call ESMF_LogWrite("User routine AtmBundleIntp finished", ESMF_LOGMSG_INFO)
  end subroutine AtmBundleIntp

  !-----------------------------------------------------------------------------

  !> Set scalar data from state for a particular name
  subroutine State_SetScalar(value, scalar_id, State, mytask, scalar_name, scalar_count,  rc)
    real(ESMF_KIND_R8),intent(in)     :: value
    integer,           intent(in)     :: scalar_id
    type(ESMF_State),  intent(inout)  :: State
    integer,           intent(in)     :: mytask
    character(len=*),  intent(in)     :: scalar_name
    integer,           intent(in)     :: scalar_count
    integer,           intent(inout)  :: rc           !< return code
  
    ! local variables
    type(ESMF_Field)                :: field
    real(ESMF_KIND_R8), pointer     :: farrayptr(:,:)
    character(len=*), parameter     :: subname='(DATM: State_SetScalar)'
    !--------------------------------------------------------
  
    rc = ESMF_SUCCESS
  
    call ESMF_StateGet(State, itemName=trim(scalar_name), field=field, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
  
    if (mytask == 0) then
      call ESMF_FieldGet(field, farrayPtr=farrayptr, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return
  
      if (scalar_id < 0 .or. scalar_id > scalar_count) then
         call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg=subname//": ERROR in scalar_id", line=__LINE__, file=__FILE__, rcToReturn=rc)
         return
      endif
  
      farrayptr(scalar_id,1) = value
    endif
  
  end subroutine State_SetScalar

  !-----------------------------------------------------------------------------

  subroutine SetScalarField(field, rc)

    ! create a field with scalar data on the root pe
    type(ESMF_Field), intent(inout)  :: field
    integer,          intent(inout)  :: rc

    ! local variables
    type(ESMF_Distgrid) :: distgrid
    type(ESMF_Grid)     :: grid
    character(len=*), parameter :: subname='(DATM: SetScalarField)'

    rc = ESMF_SUCCESS

    ! create a DistGrid with a single index space element, which gets mapped onto DE 0.
    distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    grid = ESMF_GridCreate(distgrid, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! num of scalar values
    field = ESMF_FieldCreate(name=trim(scalar_field_name), grid=grid, typekind=ESMF_TYPEKIND_R8, &
         ungriddedLBound=(/1/), ungriddedUBound=(/scalar_field_count/), gridToFieldMap=(/2/), rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine SetScalarField
end module AtmFieldUtils
