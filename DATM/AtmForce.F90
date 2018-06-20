subroutine AtmForce(gcomp,exportState,externalClock,rc)

#include "LocalDefs.F90"

  use ESMF

  use AtmFields
  use AtmFieldUtils, only : AtmFieldsToExport
  !use AtmFieldUtils, only : AtmForcingFiles

  implicit none

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: exportState
  type(ESMF_Clock)     :: externalClock
  integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Field)              :: field
  type(ESMF_Time)               :: currTime

  integer(kind=ESMF_KIND_I8)    :: stepcount
  integer(kind=ESMF_KIND_I4)    :: year, month, day, jday

  integer :: ii,nfields

  character(len=ESMF_MAXSTR) :: varname, cyear, tsrc
  character(len=ESMF_MAXSTR) :: forcefile
  character(len=ESMF_MAXSTR) :: dirpath = 'forcing/'
  character(len=ESMF_MAXSTR) :: msgString

  ! Set initial values

  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmForce started", ESMF_LOGMSG_INFO)

  call ESMF_ClockGet(externalClock, currTime=currTime, advanceCount=stepcount,rc=rc)
  call ESMF_TimeGet(currTime,yy=year,mm=month,dd=day,dayOfYear=jday,rc=rc)

  write(cyear,'(i4)')year
  ! have 2-d lat,lon arrays
  tsrc = trim(cyear)//'.nc'

  ! read the Atm field data
  nfields = size(AtmFieldsToExport)
  do ii = 1,nfields
    varname = trim(AtmFieldsToExport(ii)%file_varname)
  !forcefile = insert here

  ! insert code for 3d fields vs 2d fields
  !skip missing fields
    call ESMF_StateGet(exportState, &
                       itemName=trim(AtmFieldsToExport(ii)%field_name), &
                       field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    !call ESMF_FieldRead(field, &
    !                    fileName=trim(forcefile), &
    !                    variableName = trim(varname), &
    !                    timeslice = jday, &
    !                    rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field,farrayPtr=AtmFieldsToExport(ii)%farrayPtr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
  enddo

  call ESMF_LogWrite("User routine AtmForce finished", ESMF_LOGMSG_INFO)
end  subroutine AtmForce
