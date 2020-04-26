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
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: externalClock
    integer, intent(out)  :: rc

    character(len=10)          :: value
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
   ! Use attributes
    call ESMF_AttributeGet(model, &
                           name="Coldstart", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    coldstart=(trim(value)=="true")
    write(msgString,'(A,l6)')'DATM ColdStart = ',coldstart
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

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
#endif

    call ESMF_LogWrite("User initialize routine InitP0 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP0

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, externalClock, rc)
   
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc
   
    ! local variables
    type(ESMF_Config)       :: cf
    real(ESMF_KIND_R8)      :: medAtmCouplingIntervalSec
    character(ESMF_MAXSTR)  :: msgString

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
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(ESMF_Field)        :: field
    character(ESMF_MAXSTR)  :: msgString
    character(ESMF_MAXSTR)  :: fname

    integer :: ii, nfields

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP2 Atm started", ESMF_LOGMSG_INFO)

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
    call    AtmBundleCreate(model, importState, exportState, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call AtmFieldsRealize(exportState, gridOut, AtmBundleFields, 'Atm Export', rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call AtmInit(model, importState, exportState, externalClock, rc)

    ! AtmInit calls AtmForce and loads the values for the first integration 
    ! timestep, so.....
    ! -> set Updated Field Attribute to "true", indicating to the IPDv02p5
    ! generic code to set the timestamp for this Field

    nfields = size(AtmBundleFields)
    do ii = 1,nfields
      call ESMF_StateGet(exportState, &
                         field=field, &
                         itemName=trim(AtmBundleFields(ii)%field_name), rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ChkErr(rc,__LINE__,u_FILE_u)) return

      call ESMF_LogWrite(trim(AtmBundleFields(ii)%field_name)//' set to Updated', ESMF_LOGMSG_INFO)
    enddo !ii

    ! the component needs to indicate that it is fully done with
    ! initializing its data:
    ! -> set InitializeDataComplete Component Attribute to "true", indicating
    ! to the driver that this Component has fully initialized its data
    call NUOPC_CompAttributeSet(model, &
                                name="InitializeDataComplete", &
                                value="true", rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_LogWrite('Atm InitializeDataComplete', ESMF_LOGMSG_INFO)

    call AtmFieldCheck(importState, exportState, 'InitP2 Atm', rc)

    call ESMF_LogWrite("User initialize routine InitP2 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP2
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_State)           :: importState, exportState
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            ::  stopTime
    type(ESMF_Time)            :: startTime
    type(ESMF_Time)            ::  currTime
    type(ESMF_TimeInterval)    :: timeStep

    character(len=ESMF_MAXSTR) :: msgString
    character(len=ESMF_MAXSTR) :: export_timestr

    rc = ESMF_SUCCESS
  
    call ESMF_LogWrite("User routine ModelAdvance Atm started", ESMF_LOGMSG_INFO)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, &
                        modelClock=modelClock, &
                        importState=importState, &
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
                       rc=rc)

    call ESMF_ClockPrint(modelClock, options="currTime", &
         preString="ModelAdvance DATM with CLOCK current:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(modelClock, options="stopTime", &
         preString="ModelAdvance DATM with CLOCK stop:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockGet(modelClock, currTime=currTime, timeStep=timeStep, rc=rc)

    call ESMF_TimeGet(currTime+timestep, timestring=export_timestr, rc=rc)

    ! Run the component
    call AtmRun(model, importState, exportState, modelClock, rc)

    ! Check Values
    call AtmFieldCheck(importState, exportState, 'after AtmRun', rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    if(dumpfields)then
     call AtmFieldDump(importstate, exportstate, 'after AtmRun', trim(export_timestr), rc)
     if (ChkErr(rc,__LINE__,u_FILE_u)) return
    endif

    call ESMF_LogWrite("User routine ModelAdvance Atm finished", ESMF_LOGMSG_INFO)

  end subroutine ModelAdvance

  ! like cice_cap, which had the simplest clock settings I could find
  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep, timeStep

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock
    call ESMF_GridCompGet(model, clock=clock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalSet(timestep, s_r8=dt_atmos, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, s_r8=dt_atmos, rc=rc) 
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    
  end subroutine SetClock

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: modelClock, driverClock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep

    character(len=ESMF_MAXSTR) :: msgString

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

    ! set the modelClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_TimeIntervalSet(timestep, s_r8=dt_atmos, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockSet(modelClock, currTime=currTime, timeStep=timestep, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_ClockPrint(modelClock, options="currTime", &
         preString="entering SetRunClock DATM with modelClock current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(driverClock, options="currTime", &
         preString="entering SetRunClock DATM with driverClock current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    call ESMF_ClockPrint(modelClock, options="stopTime", &
         preString="entering SetRunClock DATM with modelClock stop: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)
    call ESMF_ClockPrint(driverClock, options="stopTime", &
         preString="entering SetRunClock DATM with driverClock stop: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO)

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(model, driverClock, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite("User routine SetRunClock Atm finished", ESMF_LOGMSG_INFO)
  end subroutine SetRunClock

end module DAtm
