module DAtm

#include "LocalDefs.F90"

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS            => SetServices, &
    !model_label_SetRunClock     => label_SetRunClock, &
    model_label_SetClock        => label_SetClock, &
    model_label_CheckImport     => label_CheckImport, &
    model_label_Advance         => label_Advance
 
  use AtmFieldUtils, only : AtmFieldsToExport
#ifndef toydatm
  use AtmFieldUtils, only : AtmFieldsToImport
#endif

  use AtmFieldUtils, only : AtmFieldsSetUp
  use AtmFieldUtils, only : AtmFieldsAdvertise, AtmFieldsRealize
  use AtmFieldUtils, only : AtmFieldDump
  use AtmFieldUtils, only : AtmFieldCheck

  ! AtmInit called by InitializeP2, AtmRun called by ModelAdvance
  use AtmModel,  only : AtmInit, AtmRun, AtmFinal
  
  use AtmFields, only : iatm,jatm,lPet

  implicit none
  
  private
  
  public SetServices

  type(ESMF_VM)   :: vm
  integer, public :: petCnt

  ! from attributes in coupled system
  logical, public ::  coldstart = .true.
  logical, public :: dumpfields = .false.
  logical, public :: profile_memory = .true.

  contains
  
  subroutine SetServices(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    
    ! overwrite the default IPDv00 with IPDv02
    call ESMF_GridCompSetEntryPoint(model, &
                                    ESMF_METHOD_INITIALIZE, &
                                    userRoutine=InitializeP0, &
                                    phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Advertise Fields
    call NUOPC_CompSetEntryPoint(model, &
                                 ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv02p1"/), &
                                 userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Realize Fields
    call NUOPC_CompSetEntryPoint(model, &
                                 ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv02p2"/), &
                                 userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
#ifdef test
    ! specialize label_SetRunClock which ensures the correct timeStep
    ! is set during the run cycle
    ! -> NUOPC specializes by default --->>> first need to remove the default
    call ESMF_MethodRemove(model, model_label_SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(model, &
                              specLabel=model_label_SetRunClock, &
                              specRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    ! attach specializing method(s)
    ! No need to change clock settings
    call ESMF_MethodAdd(model, label=model_label_SetClock, &
      userRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! overwrite default CheckImport method 
    ! if not overwritten, checkimport verifies that all import fields are at the current
    ! time of internal clock 
    call ESMF_MethodRemove(model, label=model_label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=model_label_CheckImport, &
      specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(model, &
                              specLabel=model_label_Advance, &
                              specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! overwrite Finalize
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_FINALIZE, &
                                    userRoutine=AtmFinal, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    call ESMF_VMGet(vm,petCount=petCnt,localPet=lPet,rc=rc)

   ! set up the field atts for the Atm component 
    call AtmFieldsSetUp

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

    ! like P0 in module_Mediator
    call ESMF_AttributeGet(model, &
                           name="Verbosity", &
                           value=value, &
                           defaultValue="max", &
                           convention="NUOPC", purpose="Instance", rc=rc)

    write(msgString,'(A,l6)')'DATM Verbosity = '//trim(value)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    ! Switch to IPDv02 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, &
                                  ESMF_METHOD_INITIALIZE, &
                                  acceptStringList=(/"IPDv02"/), &
                                  !acceptStringList=(/"IPDv04"/), &
                                  rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

   ! Use attributes
    call ESMF_AttributeGet(model, &
                           name="Coldstart", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    coldstart=(trim(value)=="true")
    write(msgString,'(A,l6)')'DATM ColdStart = ',coldstart
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

   ! Use attributes
    call ESMF_AttributeGet(model, &
                           name="DumpFields", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    dumpfields=(trim(value)=="true")

    write(msgString,'(A,l6)')'DATM Dumpfields = ',dumpfields
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    !like module_MEDIATOR
    call ESMF_AttributeGet(model, &
                           name="ProfileMemory", &
                           value=value, &
                           defaultValue="true", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    profile_memory=(trim(value)/="false")

    write(msgString,'(A,l6)')'DATM Profile_memory = ',profile_memory
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_LogWrite("User initialize routine InitP0 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP0

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(model, importState, exportState, externalClock, rc)
   
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc
   
    rc = ESMF_SUCCESS
   
    call ESMF_LogWrite("User initialize routine InitP1 Atm started", ESMF_LOGMSG_INFO)

    call AtmFieldsAdvertise(exportState, AtmFieldsToExport, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#ifndef toydatm
    call AtmFieldsAdvertise(importState, AtmFieldsToImport, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif

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

    integer :: ii, nfields

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP2 Atm started", ESMF_LOGMSG_INFO)

    call ESMF_ClockPrint(externalClock, options="currTime", &
         preString="entering DATM_INITIALIZE with CLOCK_EARTH current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_ClockPrint(externalClock, options="startTime", &
         preString="entering DATM_INITIALIZE with CLOCK_EARTH start:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_ClockPrint(externalClock, options="stopTime", &
         preString="entering DATM_INITIALIZE with CLOCK_EARTH stop:    ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call AtmGridSetUp(gridIn,petCnt,'Atm grid','InitP2 Atm',rc)
    gridOut = gridIn ! for now out same as in

    call AtmFieldsRealize(exportState, gridOut, AtmFieldsToExport, 'Atm Export', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#ifndef toydatm
    call AtmFieldsRealize(importState, gridOut, AtmFieldsToImport, 'Atm Import', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    ! Attach the grid to the Component
    call ESMF_GridCompSet(model, grid=gridOut, rc=rc)

    !call ESMF_GridCompPrint(model, rc=rc)

    call AtmInit(model, importState, exportState, externalClock, rc)
    ! AtmInit calls AtmForce and loads the values for the first integration 
    ! timestep, so.....
    ! -> set Updated Field Attribute to "true", indicating to the IPDv02p5
    ! generic code to set the timestamp for this Field

    nfields = size(AtmFieldsToExport)
    do ii = 1,nfields
      call ESMF_StateGet(exportState, &
                         field=field, &
                         itemName=trim(AtmFieldsToExport(ii)%field_name), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call ESMF_LogWrite(trim(AtmFieldsToExport(ii)%field_name)//' set to Updated', ESMF_LOGMSG_INFO)
    enddo !ii

    ! the component needs to indicate that it is fully done with
    ! initializing its data:
    ! -> set InitializeDataComplete Component Attribute to "true", indicating
    ! to the driver that this Component has fully initialized its data
    call NUOPC_CompAttributeSet(model, &
                                name="InitializeDataComplete", &
                                value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite('Atm InitializeDataComplete', ESMF_LOGMSG_INFO)

    call ESMF_LogWrite("User initialize routine InitP2 Atm finished", ESMF_LOGMSG_INFO)

  end subroutine InitializeP2
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               ::  currTime
    type(ESMF_Time)               ::  prevTime
    type(ESMF_Time)               :: startTime

    integer(kind=ESMF_KIND_I8)    :: stepcount
    ! starting jday, current jday, previous jday
    integer(kind=ESMF_KIND_I4) :: jday0, jday, jdayp
    ! true on first timestep of new day
    logical :: newday

                       integer :: i,j
                       integer :: idumpcnt = 0
    character(len=ESMF_MAXSTR) :: msgString

    rc = ESMF_SUCCESS
  
    call ESMF_LogWrite("User routine ModelAdvance Atm started", ESMF_LOGMSG_INFO)

    idumpcnt = idumpcnt + 1
    ! query the Component for its clock, importState and exportState

    call NUOPC_ModelGet(model, &
                        modelClock=clock, &
                        importState=importState, &
                        exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call ESMF_ClockGet(clock, &
                       currTime=currTime, &
                       prevTime=prevTime, &
                       startTime=startTime, &
                       advanceCount=stepcount, &
                       rc=rc)
    call ESMF_TimeGet(startTime,dayOfYear=jday0 ,rc=rc)
    call ESMF_TimeGet( currTime,dayOfYear=jday  ,rc=rc)
    call ESMF_TimeGet( prevTime,dayOfYear=jdayp,rc=rc)
                        newday = .false.
    if(jdayp .ne.  jday)newday = .true.
    if(jdayp .eq. jday0)newday = .true.
    !print *,newday,jday0,jday,jdayp,stepcount

    ! get forcing on each newday
    !if(newday)call AtmRun(model, importState, exportState, clock, rc)
    !call AtmRun(model, importState, exportState, clock, rc)

    ! Check Values
    call AtmFieldCheck(importState, exportState, 'after AtmRun', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if(dumpfields)then
     call AtmFieldDump(importstate, exportstate, 'after AtmRun', idumpcnt, rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
    endif
!#ifdef test
    call ESMF_ClockPrint(clock, options="startTime", &
         preString="ModelAdvance DATM with CLOCK start:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_ClockPrint(clock, options="stopTime", &
         preString="ModelAdvance DATM with CLOCK stop:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
!#endif
#ifdef test
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
#endif
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

    ! TODO: Get from config?
    ! temp hardwire here
    real(ESMF_KIND_R4) :: dt = 1800.

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock
    call ESMF_GridCompGet(model, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! tcraig: dt is the cice thermodynamic timestep in seconds
    call ESMF_TimeIntervalSet(timestep, s=nint(dt), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, s=nint(dt), rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
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

    call ESMF_LogWrite("User routine SetRunClock Atm started", ESMF_LOGMSG_INFO)

    ! query the model for clocks
    call NUOPC_ModelGet(model, &
                        modelClock=modelClock, &
                        driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(modelClock, options="currTime", &
         preString="entering SetRunClock DATM with modelClock current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_ClockPrint(driverClock, options="currTime", &
         preString="entering SetRunClock DATM with driverClock current: ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! set the modelClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockSet(modelClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(modelClock, options="currTime", &
         preString="after reset SetRunClock modelClock to driverClock : ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(model, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("User routine SetRunClock Atm finished", ESMF_LOGMSG_INFO)
  end subroutine SetRunClock

  !-----------------------------------------------------------------------------

  subroutine CheckImport(model, rc)

    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc

    ! This is the routine that enforces the complex time dependence on the
    ! import fields. 
    !
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currtime, starttime, stoptime
    logical                       :: neededCurrent
    logical                       :: atStopTime
    logical                       :: atCurrTime

    integer :: ii,nfields
    character(len=ESMF_MAXSTR) :: msgString

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User routine CheckImport Atm started", ESMF_LOGMSG_INFO)

    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the current time and stop time out of the clock
    call ESMF_ClockGet(clock, &
                       currTime=currtime, &
                       startTime=starttime, &
                       stopTime=stoptime,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="currTime", &
         preString="CheckImport with CLOCK current:   ", &
         unit=msgString)
    call ESMF_ClockPrint(clock, options="startTime", &
         preString="CheckImport with CLOCK start:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    call ESMF_ClockPrint(clock, options="stopTime", &
         preString="CheckImport with CLOCK stop:   ", &
         unit=msgString)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_StateGet(importState, itemCount=nfields,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !ensure fields from ATM are at stopTime 
    nfields = size(AtmFieldsToImport)
    do ii=1,nfields
    ! atStopTime = NUOPC_IsAtTime(importState, stopTime, &
    !                             fieldName=AtmFieldsToImport(ii)%field_name, rc=rc)
    enddo

    call ESMF_LogWrite("User routine CheckImport Atm finished", ESMF_LOGMSG_INFO)

  end subroutine CheckImport
end module DAtm
