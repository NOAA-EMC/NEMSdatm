module AtmCap

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
 
  use AtmFieldUtils, only : AtmFieldsToExport

  use AtmFieldUtils, only : AtmFieldsSetUp
  use AtmFieldUtils, only : AtmFieldsAdvertise, AtmFieldsRealize

  ! AtmInit called by InitializeP2, AtmRun called by ModelAdvance
  use AtmModel,  only : AtmInit, AtmRun, AtmFinal
  
  use AtmFields, only : iatm,jatm

  implicit none
  
  private
  
  public SetServices

  integer       :: petCnt

  contains
  
  subroutine SetServices(model, rc)

    type(ESMF_VM)        :: vm
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

    ! attach specializing method(s)
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
    call ESMF_VMGet(vm,petCount=petCnt, rc=rc)

   ! set up the field atts for the Atm component 
    call AtmFieldsSetUp

  end subroutine SetServices
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(model, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)   :: model
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: externalClock
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP0 Atm started", ESMF_LOGMSG_INFO)

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

    integer :: ii, nfields

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User initialize routine InitP2 Atm started", ESMF_LOGMSG_INFO)
 
    call AtmGridSetUp(gridIn,petCnt,iatm,jatm,'Atm grid','InitP2 Atm',rc)
    gridOut = gridIn ! for now out same as in

    call AtmFieldsRealize(exportState, gridOut, AtmFieldsToExport, 'Atm Export', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

    rc = ESMF_SUCCESS
  
    call ESMF_LogWrite("User routine ModelAdvance Atm started", ESMF_LOGMSG_INFO)

    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
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
#ifdef test
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing ATM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
#endif
    call ESMF_LogWrite("User routine ModelAdvance Atm finished", ESMF_LOGMSG_INFO)

  end subroutine ModelAdvance

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(model, rc)

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: modelClock, driverClock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep

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

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(model, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("User routine SetRunClock Atm finished", ESMF_LOGMSG_INFO)
  end subroutine SetRunClock
end module AtmCap
