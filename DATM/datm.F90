module DAtm

#include "LocalDefs.F90"

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS            => SetServices, &
    model_label_SetRunClock     => label_SetRunClock, &
    model_label_Advance         => label_Advance

  ! Fields exported by Atm
  use AtmFieldUtils,     only : AtmFieldsAdvertise, AtmFieldsRealize
  use AtmFieldUtils,     only : AtmFieldDump
  use AtmFieldUtils,     only : AtmFieldCheck
  use AtmFieldUtils,     only : State_SetScalar
  use AtmGridUtils,      only : WriteCoord, WriteMask

  ! AtmInit called by InitializeP2, AtmRun called by ModelAdvance
  use AtmModel,          only : AtmInit, AtmRun, AtmFinal
  use AtmInternalFields

  implicit none

  private

  public SetServices

  type(ESMF_VM)   :: vm

  ! from attributes in coupled system
  logical, public ::  coldstart = .true.
  logical, public :: dumpfields = .true.
  logical, public :: profile_memory = .false.

  character(len=*),parameter :: u_FILE_u = &
     __FILE__

  contains

  subroutine SetServices(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! set entry point for methods that require specific implementation

    ! overwrite the default IPDv00 with IPDv02
    call ESMF_GridCompSetEntryPoint(model, &
                                    ESMF_METHOD_INITIALIZE, &
                                    userRoutine=InitializeP0, &
                                    phase=0, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Advertise Fields
    call NUOPC_CompSetEntryPoint(model, &
                                 ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv02p1"/), &
                                 userRoutine=InitializeP1, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Realize Fields
    call NUOPC_CompSetEntryPoint(model, &
                                 ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv02p2"/), &
                                 userRoutine=InitializeP2, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! specialize label_SetRunClock which ensures the correct timeStep
    ! is set during the run cycle
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(model, model_label_SetRunClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(model, &
                              specLabel=model_label_SetRunClock, &
                              specRoutine=SetRunClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call NUOPC_CompSpecialize(model, &
                              specLabel=model_label_Advance, &
                              specRoutine=ModelAdvance, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_FINALIZE, &
                                    userRoutine=AtmFinal, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    call ESMF_VMGet(vm,petCount=petCnt,localPet=lPet,rc=rc)

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(model, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)   :: model
    type(ESMF_State)      :: importState
    type(ESMF_State)      :: exportState
    type(ESMF_Clock)      :: externalClock
    integer, intent(out)  :: rc
    logical               :: isPresent, isSet

    integer                    :: iostat
    character(len=64)          :: value
    character(len=ESMF_MAXSTR) :: msgString

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP0 Atm started", ESMF_LOGMSG_INFO)

    ! Switch to IPDv02 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, &
                                  ESMF_METHOD_INITIALIZE, &
                                  acceptStringList=(/"IPDv02"/), &
                                  !acceptStringList=(/"IPDv04"/), &
                                  rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
#ifdef coupled
    call ESMF_AttributeGet(model, &
                           name="DumpFields", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    dumpfields=(trim(value)=="true")

    write(msgString,'(A,l6)')'DATM Dumpfields = ',dumpfields
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    !like module_MEDIATOR
    call ESMF_AttributeGet(model, &
                           name="ProfileMemory", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    profile_memory=(trim(value)/="false")

    write(msgString,'(A,l6)')'DATM Profile_memory = ',profile_memory
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

    scalar_field_name = ""
    call NUOPC_CompAttributeGet(model, name="ScalarFieldName", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       scalar_field_name = trim(value)
       call ESMF_LogWrite('DATM: ScalarFieldName = '//trim(scalar_field_name), ESMF_LOGMSG_INFO)
    else
       rc = ESMF_FAILURE
       call ESMF_LogWrite('ScalarFieldName must be set', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    scalar_field_count = 0
    call NUOPC_CompAttributeGet(model, name="ScalarFieldCount", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(value, *, iostat=iostat) scalar_field_count
       if (iostat /= 0) then
         call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="DATM : ScalarFieldCount not an integer: "//trim(value), &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
         return
       endif
       write(msgString,*) scalar_field_count
       call ESMF_LogWrite('DATM: ScalarFieldCount = '//trim(msgString), ESMF_LOGMSG_INFO)
    else
       rc = ESMF_FAILURE
       call ESMF_LogWrite('ScalarFieldCount must be set', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    scalar_field_idx_grid_nx = 0
    call NUOPC_CompAttributeGet(model, name="ScalarFieldIdxGridNX", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(value, *, iostat=iostat) scalar_field_idx_grid_nx
       if (iostat /= 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
               msg="DATM : ScalarFieldIdxGridNX not an integer: "//trim(value), &
               line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
       endif
       write(msgString,*) scalar_field_idx_grid_nx
       call ESMF_LogWrite('DATM: ScalarFieldIdxGridNX = '//trim(msgString), ESMF_LOGMSG_INFO)
    else
       rc = ESMF_FAILURE
       call ESMF_LogWrite('ScalarFieldIdxGridNX must be set', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    scalar_field_idx_grid_ny = 0
    call NUOPC_CompAttributeGet(model, name="ScalarFieldIdxGridNY", value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
       read(value, *, iostat=iostat) scalar_field_idx_grid_ny
       if (iostat /= 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
               msg="DATM : ScalarFieldIdxGridNY not an integer: "//trim(value), &
               line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
       endif
       write(msgString,*) scalar_field_idx_grid_ny
       call ESMF_LogWrite('DATM: ScalarFieldIdxGridNY = '//trim(msgString), ESMF_LOGMSG_INFO)
    else
       rc = ESMF_FAILURE
       call ESMF_LogWrite('ScalarFieldIdxGridNY must be set', ESMF_LOGMSG_ERROR)
       call ESMF_Finalize(endflag=ESMF_END_ABORT)
    endif

    dbug = 0
    call NUOPC_CompAttributeGet(model, name='dbug_flag', value=value, &
         isPresent=isPresent, isSet=isSet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    if (isPresent .and. isSet) then
     read(value,*) dbug
    end if
    write(msgString,*) dbug
    call ESMF_LogWrite('DATM: Debug flag = '//trim(msgString), ESMF_LOGMSG_INFO)
#endif

    call ESMF_LogWrite("User initialize routine InitP0 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP0


  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Config)       :: cf
    real(ESMF_KIND_R8)      :: medAtmCouplingIntervalSec
    character(ESMF_MAXSTR)  :: msgString
    character(20)           :: cvalue

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP1 Atm started", ESMF_LOGMSG_INFO)

    ! Set up the fields in the AtmBundle
    call AtmBundleSetUp

    call AtmFieldsAdvertise(exportState, AtmBundleFields, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

  !-----------------------------------------------------------------------------
  ! get config variables, like fv3_cap
  ! ? could also get npx,npy (npx*npy=nprocs) to set decomposition
  !-----------------------------------------------------------------------------

    cf=ESMF_ConfigCreate(rc=rc)
    call ESMF_ConfigLoadFile(config=cf ,filename='model_configure' ,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=iatm, &
                                 label='iatm:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,i6)')'Model configure found with iatm = ',iatm
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=jatm, &
                                 label='jatm:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,i6)')'Model configure found with jatm = ',jatm
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=cdate0, &
                                 label='cdate0:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,a)')'Model configure found with cdate0 = ',trim(cdate0)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=nfhout, &
                                 label='nfhout:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,i6)')'Model configure found with nfhout = ',nfhout
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=filename_base, &
                                 label='filename_base:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,a)')'Model configure found with filename_base = ', &
                            trim(filename_base)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=dt_atmos, &
                                 label='dt_atmos:',rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,f8.1)')'Model configure found with dt_atmos = ',dt_atmos
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ConfigGetAttribute(config=cf, &
                                 value=medAtmCouplingIntervalSec, &
                                 label="atm_coupling_interval_sec:", &
                                 default=-1.0_ESMF_KIND_R8, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    write(msgString,'(a,f8.1)')'Model configure found with  atm_coupling_interval_sec = ', &
                            medAtmCouplingIntervalSec
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)


    call ESMF_LogWrite("User initialize routine InitP1 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP1

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(model, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalClock
    type(ESMF_Time)      :: currTime
    type(ESMF_VM)        :: vm
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(ESMF_Field)        :: field
    character(ESMF_MAXSTR)  :: msgString
    character(ESMF_MAXSTR)  :: timestr
    character(ESMF_MAXSTR)  :: fname

    integer :: localPet, npet, mpicom
    integer :: ii, nfields

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP2 Atm started", ESMF_LOGMSG_INFO)

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_VMGet(vm, petCount=npet, mpiCommunicator=mpicom, localPet=localPet, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockPrint(externalClock, options="currTime", &
         preString="InitP2 Atm CLOCK_EARTH current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(externalClock, options="startTime", &
         preString="InitP2 Atm CLOCK_EARTH start:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(externalClock, options="stopTime", &
         preString="InitP2 Atm CLOCK_EARTH stop:    ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    fname = trim(dirpath)//trim(filename_base)//'SCRIP.nc'
    call ESMF_LogWrite('reading grid file '//trim(fname), ESMF_LOGMSG_INFO)

    gridIn = ESMF_GridCreate(filename=trim(fname),&
                             fileformat = ESMF_FILEFORMAT_SCRIP, &
                             addCornerStagger=.true., &
                             indexflag=AtmIndexType, &
                             addMask=.true.,rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    gridOut = gridIn ! for now out same as in

    ! Write coords and mask to file
    call WriteCoord(gridIn, ESMF_STAGGERLOC_CENTER, 1, 'atmlonc', lPet, rc)
    call WriteCoord(gridIn, ESMF_STAGGERLOC_CENTER, 2, 'atmlatc', lPet, rc)
    call WriteCoord(gridIn, ESMF_STAGGERLOC_CORNER, 1, 'atmlonq', lPet, rc)
    call WriteCoord(gridIn, ESMF_STAGGERLOC_CORNER, 2, 'atmlatq', lPet, rc)

    call WriteMask(gridIn, ESMF_STAGGERLOC_CENTER, 'atmmask', lPet, rc)

    ! Attach the grid to the Component
    call ESMF_GridCompSet(model, grid=gridOut, rc=rc)
    !call ESMF_GridCompPrint(model, rc=rc)

    ! Create and fill the AtmBundle
    call    AtmBundleCreate(model, exportState, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call AtmFieldsRealize(exportState, gridOut, AtmBundleFields, 'Atm Export', rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call AtmInit(model, exportState, externalClock, rc)

    ! AtmInit calls AtmForce and loads the values for the first integration
    ! timestep, so.....
    ! -> set Updated Field Attribute to "true", indicating to the IPDv02p5
    ! generic code to set the timestamp for this Field

    nfields = size(AtmBundleFields)
    do ii = 1,nfields
      call ESMF_StateGet(exportState, &
                         field=field, &
                         itemName=trim(AtmBundleFields(ii)%shortname), rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_LogWrite(trim(AtmBundleFields(ii)%shortname)//' set to Updated', ESMF_LOGMSG_INFO)
    enddo !ii

    ! set scalars for cmeps
    if(len_trim(scalar_field_name) > 0) then
      call State_SetScalar(dble(iatm),scalar_field_idx_grid_nx, exportState, localPet, &
          scalar_field_name, scalar_field_count, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call State_SetScalar(dble(jatm),scalar_field_idx_grid_ny, exportState, localPet, &
           scalar_field_name, scalar_field_count, rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_StateGet(exportState, itemName=trim(scalar_field_name), field=field, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_LogWrite(trim(scalar_field_name)//' set to Updated', ESMF_LOGMSG_INFO)
    end if

    ! the component needs to indicate that it is fully done with
    ! initializing its data:
    ! -> set InitializeDataComplete Component Attribute to "true", indicating
    ! to the driver that this Component has fully initialized its data
    call NUOPC_CompAttributeSet(model, &
                                name="InitializeDataComplete", &
                                value="true", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_LogWrite('Atm InitializeDataComplete', ESMF_LOGMSG_INFO)

    if(dbug > 5)call AtmFieldCheck(exportState, 'InitP2 Atm', rc)

    ! the initial fields at model startup
    if(dumpfields)then
     call ESMF_ClockGet(externalClock, currTime=currTime, rc = rc)
     call ESMF_TimeGet(currTime, timestring=timestr, rc=rc)

     fname = 'field_atm_exporti_'//trim(timestr)//'.nc'
     call AtmFieldDump(exportState, trim(fname), rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call ESMF_LogWrite("User initialize routine InitP2 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP2

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)           :: exportState
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            ::  stopTime
    type(ESMF_Time)            :: startTime
    type(ESMF_Time)            ::  currTime
    type(ESMF_TimeInterval)    :: timeStep

    character(len=ESMF_MAXSTR) :: fname
    character(len=ESMF_MAXSTR) :: msgString
    character(len=ESMF_MAXSTR) :: export_timestr

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User routine ModelAdvance Atm started", ESMF_LOGMSG_INFO)

    ! query the Component for its clock and exportState
    call NUOPC_ModelGet(model, &
                        modelClock=modelClock, &
                        exportState=exportState, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call ESMF_ClockGet(modelClock, &
                       currTime=currTime, &
                       startTime=startTime, &
                       stopTime=stopTime, &
                       timeStep=timeStep, &
                       rc=rc)

    call ESMF_TimePrint(currTime, &
         preString="ModelAdvance DATM with CLOCK current:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_TimePrint(currTime + timeStep, &
         preString="ModelAdvance DATM with CLOCK advance to: ", &
         unit=msgString, rc=rc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    call ESMF_TimePrint(stopTime, &
         preString="ModelAdvance DATM with CLOCK stop:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_TimeGet(currTime+timestep, timestring=export_timestr, rc=rc)

    ! Run the component
    call AtmRun(model, exportState, modelClock, rc)

    ! Check Values
    if(dbug > 5)call AtmFieldCheck(exportState, 'after AtmRun', rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if(dumpfields)then
     fname = 'field_atm_exporta_'//trim(export_timestr)//'.nc'
     call AtmFieldDump(exportState, trim(fname), rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call ESMF_LogWrite("User routine ModelAdvance Atm finished", ESMF_LOGMSG_INFO)

  end subroutine ModelAdvance

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: modelClock, driverClock
    type(ESMF_Time)            :: mcurrtime, dcurrtime
    type(ESMF_Time)            :: mstoptime, dstoptime
    type(ESMF_TimeInterval)    :: mtimestep, dtimestep

    character(len=ESMF_MAXSTR) :: mtimestring, dtimestring
    character(len=ESMF_MAXSTR) :: msgString
    character(len=ESMF_MAXSTR) :: subname = "SetRunClock"

    rc = ESMF_SUCCESS
#ifndef coupled
    return
#endif
    call ESMF_LogWrite("User routine SetRunClock Atm started", ESMF_LOGMSG_INFO)

    ! query the model for clocks
    call NUOPC_ModelGet(model, &
                        modelClock=modelClock, &
                        driverClock=driverClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(driverClock, currTime=dcurrtime, timeStep=dtimestep, &
                       stopTime=dstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockGet(modelClock, currTime=mcurrtime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalSet(mtimestep, s_r8=dt_atmos, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    !--------------------------------
    ! check that the current time in the model and driver are the same
    !--------------------------------

    if (mcurrtime /= dcurrtime) then
      call ESMF_TimeGet(dcurrtime, timeString=dtimestring, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_TimeGet(mcurrtime, timeString=mtimestring, rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_LogSetError(ESMF_RC_VAL_WRONG, &
           msg=trim(subname)//": ERROR in time consistency: "//trim(dtimestring)//" != "//trim(mtimestring),  &
           line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    !--------------------------------
    ! force model clock currtime and timestep to match driver and set stoptime
    !--------------------------------

    mstoptime = mcurrtime + dtimestep

    call ESMF_ClockSet(modelClock, currTime=dcurrtime, timeStep=dtimestep, stopTime=mstoptime, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite("User routine SetRunClock Atm finished", ESMF_LOGMSG_INFO)
  end subroutine SetRunClock

end module DAtm
