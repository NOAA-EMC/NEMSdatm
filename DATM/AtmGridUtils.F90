module AtmGridUtils

  !-----------------------------------------------------------------------------
  ! Grid utility module
  !-----------------------------------------------------------------------------

  use ESMF
  use AtmInternalFields, only : ChkErr

  implicit none

  private

  ! called by AtmGridSetUp
  public :: WriteCoord, WriteMask
 
  character(len=*),parameter :: u_FILE_u = &
     __FILE__

  contains

  !-------------------------------------------------------------------------------------

  subroutine WriteCoord(grid,stagger,dimnum,fname,lPet,rc)

  type(ESMF_Grid)         :: grid
  type(ESMF_Array)        :: array2d
  type(ESMF_StaggerLoc)   :: stagger

           integer, intent(in) :: dimnum,lPet
  character(len=*), intent(in) :: fname
  
          integer, intent(out) :: rc

  !Local Variables
  real(kind=ESMF_KIND_R8), pointer  :: r8Ptr(:,:)

  character(len=ESMF_MAXSTR) :: varname, filename
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  ! retrieve the coord
   call ESMF_GridGetCoord(grid, &
                          coordDim=dimnum, &
                          staggerloc=stagger, &
                          array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! a pointer to the array
  !call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, localDe=0, rc = rc)
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, rc = rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! verify values
  write(msgString,*)'r8Ptr ',trim(fname),' lPet', &
                    lPet,lbound(r8Ptr,1),ubound(r8Ptr,1), &
                         lbound(r8Ptr,2),ubound(r8Ptr,2), &
                         minval(real(r8Ptr,4)), &
                         maxval(real(r8Ptr,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

  ! write to a file
  varname =  trim(fname); filename = trim(varname)//'.nc'
  call ESMF_ArrayWrite(array2d, &
                       fileName=filename, &
                       variableName=varname, &
                       overwrite=.true., rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine WriteCoord

  !-------------------------------------------------------------------------------------

  subroutine WriteMask(grid,stagger,fname,lPet,rc)

  use ESMF

  type(ESMF_Grid)         :: grid
  type(ESMF_Array)        :: array2d
  type(ESMF_StaggerLoc)   :: stagger

           integer, intent(in) :: lPet
  character(len=*), intent(in) :: fname
  
          integer, intent(out) :: rc

  ! Local variables
  integer(kind=ESMF_KIND_R4), pointer  :: i4Ptr(:,:)

  character(len=ESMF_MAXSTR) :: varname, filename
  character(len=ESMF_MAXSTR) :: msgString

  ! Initialize return code
  rc = ESMF_SUCCESS

  ! retrieve the mask
  call ESMF_GridGetItem(grid, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  !a pointer to the mask
  !call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, localDE=0, rc = rc)
  call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, rc = rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! verify values
  write(msgString,*)'i4Ptr ',trim(fname),' lPet', &
                     lPet,lbound(i4Ptr,1),ubound(i4Ptr,1), &
                          lbound(i4Ptr,2),ubound(i4Ptr,2), &
                          minval(i4Ptr), &
                          maxval(i4Ptr)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

  ! write to a file
   varname =  trim(fname); filename = trim(varname)//'.nc'
   call ESMF_ArrayWrite(array2d, &
                        fileName=filename, &
                        variableName=varname, &
                        overwrite=.true., rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  end subroutine WriteMask
end module AtmGridUtils
