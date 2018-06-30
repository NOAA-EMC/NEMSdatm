module AtmGridUtils

  !-----------------------------------------------------------------------------
  ! Grid utility module
  !-----------------------------------------------------------------------------

  use ESMF

  implicit none

  private

  ! called by AtmGridSetUp
  public :: ReadCoordFromFile
  public :: AddCoord2Grid, WriteCoord, WriteMask 
 
  contains

  !-------------------------------------------------------------------------------------

  subroutine ReadCoordFromFile(filename,coordname,coorddim,coordarray)

  use netcdf

          character(len=*), intent(in) :: filename, coordname
                   integer, intent(in) :: coorddim

  real(kind=ESMF_KIND_R8), intent(out) :: coordarray(coorddim)

  integer :: ncid, varid, rc

  rc = nf90_open(trim(filename), nf90_nowrite, ncid)
  rc = nf90_inq_varid(ncid, trim(coordname), varid)
  rc = nf90_get_var(ncid, varid, coordarray)
  rc = nf90_close(ncid)

  end subroutine ReadCoordFromFile

  !-------------------------------------------------------------------------------------

  subroutine AddCoord2Grid(grid,stagger,imax,jmax,coordX,coordY,rc)

  type(ESMF_Grid)         :: grid
  type(ESMF_Array)        :: array2d
  type(ESMF_StaggerLoc)   :: stagger

                  integer, intent(in) :: imax, jmax
  real(kind=ESMF_KIND_R8), intent(in) :: coordX(imax) 
  real(kind=ESMF_KIND_R8), intent(in) :: coordY(jmax) 

                 integer, intent(out) :: rc

    integer :: i,j
    real(kind=ESMF_KIND_R8), pointer  :: r8Ptr(:,:)

  ! Initialize return code
  rc = ESMF_SUCCESS

  ! retrieve an array for the first coord
  call ESMF_GridGetCoord(grid, &
                        coordDim=1, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  print *,lbound(r8Ptr,1),ubound(r8Ptr,1),lbound(coordX,1),ubound(coordX,1)

  ! fill the values from the coordXc farray
  do i = lbound(r8Ptr,1),ubound(r8Ptr,1)
   r8Ptr(i,:) = coordX(i)
  enddo

  ! Set the coord in the grid, not required?
  call ESMF_GridSetCoord(grid, &
                        coordDim=1, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! retrieve an array for the second coord
  call ESMF_GridGetCoord(grid, &
                        coordDim=2, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  print *,lbound(r8Ptr,2),ubound(r8Ptr,2),lbound(coordY,1),ubound(coordY,1)

  ! fill the values from the coordY farray
  do j = lbound(r8Ptr,2),ubound(r8Ptr,2)
   r8Ptr(:,j) = coordY(j)
  enddo

  ! Set the coord in the grid, not required?
  call ESMF_GridSetCoord(grid, &
                        coordDim=2, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  end subroutine AddCoord2Grid

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
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! verify values
  write(msgString,*)'r8Ptr ',trim(fname),' lPet', &
                    lPet,lbound(r8Ptr,1),ubound(r8Ptr,1), &
                         lbound(r8Ptr,2),ubound(r8Ptr,2), &
                         minval(real(r8Ptr,4)), &
                         maxval(real(r8Ptr,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! write to a file
  varname =  trim(fname); filename = trim(varname)//'.nc'
  call ESMF_ArrayWrite(array2d, &
                       fileName=filename, &
                       variableName=varname, &
                       overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

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
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !a pointer to the mask
  call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! verify values
  write(msgString,*)'i4Ptr ',trim(fname),' lPet', &
                     lPet,lbound(i4Ptr,1),ubound(i4Ptr,1), &
                          lbound(i4Ptr,2),ubound(i4Ptr,2), &
                          minval(i4Ptr), &
                          maxval(i4Ptr)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! write to a file
   varname =  trim(fname); filename = trim(varname)//'.nc'
   call ESMF_ArrayWrite(array2d, &
                        fileName=filename, &
                        variableName=varname, &
                        overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  end subroutine WriteMask
end module AtmGridUtils
