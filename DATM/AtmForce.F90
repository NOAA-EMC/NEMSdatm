subroutine AtmForce(gcomp,exportState,externalClock,rc)
#include "LocalDefs.F90"

  use ESMF

  use AtmFields
  use AtmFieldUtils, only : AtmFieldsToExport

  implicit none

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: exportState
  type(ESMF_Clock)     :: externalClock
  integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Field)              :: field
  type(ESMF_Time)               :: currTime

  integer(kind=ESMF_KIND_I8)    :: stepcount
  integer(kind=ESMF_KIND_I4)    :: year, month, day, hour, jday

  integer :: ii,nfields

  character(len=ESMF_MAXSTR) :: varname
  character(len=ESMF_MAXSTR) :: forcefile
  character(len=ESMF_MAXSTR) :: msgString

  character(len=4) :: cyear
  character(len=2) :: cmon, cday, chour

  character(len=8) :: i2fmt = '(i2.2)'
  character(len=8) :: i4fmt = '(i4.4)'

  ! Set initial values

  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User routine AtmForce started", ESMF_LOGMSG_INFO)

  call ESMF_ClockGet(externalClock, currTime=currTime, advanceCount=stepcount,rc=rc)
  call ESMF_TimeGet(currTime,yy=year,mm=month,dd=day,h=hour,dayOfYear=jday,rc=rc)
  
  write(cyear, i4fmt)year
  write( cmon, i2fmt)month
  write( cday, i2fmt)day
  write(chour, i2fmt)hour

  if(mod(hour,3) .ne. 0)return

  forcefile = trim(dirpath)//'gdas.'//trim(cyear)//trim(cmon)//trim(cday)//trim(chour)//'.nc'
  call ESMF_LogWrite(trim(forcefile), ESMF_LOGMSG_INFO, rc=rc)

  ! read the Atm field data
  nfields = size(AtmFieldsToExport)
  do ii = 1,nfields
    varname = trim(AtmFieldsToExport(ii)%file_varname)

    call ESMF_StateGet(exportState, &
                       itemName=trim(AtmFieldsToExport(ii)%field_name), &
                       field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldRead(field, &
                        fileName=trim(forcefile), &
                        variableName = trim(varname), &
                        rc=rc)
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
