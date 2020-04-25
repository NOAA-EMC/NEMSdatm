module AtmGridUtils

  !-----------------------------------------------------------------------------
  ! Grid utility module
  !-----------------------------------------------------------------------------

  use ESMF
  use AtmInternalFields, only : ChkErr

  implicit none

  private

  ! called by AtmGridSetUp
  public :: ReadCoordFromFile, ReadMaskFromFile
  public :: AddCoord2Grid, WriteCoord, WriteMask 
 
  character(len=*),parameter :: u_FILE_u = &
     __FILE__

  contains

  !-------------------------------------------------------------------------------------

  subroutine ReadCoordFromFile(filename,coordname,coorddim,coordarray)

  use netcdf

          character(len=*), intent(in) :: filename, coordname
                   integer, intent(in) :: coorddim

  real(kind=ESMF_KIND_R8), intent(out) :: coordarray(coorddim)

  real(kind=ESMF_KIND_R4) :: coordR4(coorddim)

  integer :: ncid, varid, rc

  rc = nf90_open(trim(filename), nf90_nowrite, ncid)
  if(rc .ne. 0)then
   call ESMF_LogWrite(trim(filename)//' not found!', ESMF_LOGMSG_INFO)
   call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  rc = nf90_inq_varid(ncid, trim(coordname), varid)
  rc = nf90_get_var(ncid, varid, coordR4)
  rc = nf90_close(ncid)

  coordarray = real(coordR4,8)

  end subroutine ReadCoordFromFile

  !-------------------------------------------------------------------------------------

  subroutine ReadMaskFromFile(filename,maskname,maskarray)

  use netcdf
  use AtmInternalFields, only : iatm, jatm

          character(len=*), intent(in) :: filename, maskname

  real(kind=ESMF_KIND_R4), intent(out) :: maskarray(iatm,jatm)

  integer :: ncid, varid, rc

  rc = nf90_open(trim(filename), nf90_nowrite, ncid)
  if(rc .ne. 0)then
   call ESMF_LogWrite(trim(filename)//' not found!', ESMF_LOGMSG_INFO)
   call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end if

  rc = nf90_inq_varid(ncid, trim(maskname), varid)
  rc = nf90_get_var(ncid, varid, maskarray)
  rc = nf90_close(ncid)

  end subroutine ReadMaskFromFile

  !-------------------------------------------------------------------------------------

  subroutine AddCoord2Grid(grid,stagger,imax,jmax,coordX,coordY,rc)

  type(ESMF_Grid)         :: grid
  type(ESMF_Array)        :: array2d
  type(ESMF_StaggerLoc)   :: stagger

                  integer, intent(in) :: imax, jmax
  real(kind=ESMF_KIND_R8), intent(in) :: coordX(imax) 
  real(kind=ESMF_KIND_R8), intent(in) :: coordY(jmax) 

                 integer, intent(out) :: rc

    integer :: i,j,lde,localDECount
    real(kind=ESMF_KIND_R8), pointer  :: r8Ptr(:,:)
    character(len=ESMF_MAXSTR)        :: msgString


  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  do lde = 0,localDECount-1
 
  ! Retrieve a pointer to the first coord 
  call ESMF_GridGetCoord(grid, localDE=lde,&
                        coordDim=1, &
                        staggerloc=stagger, &
                        farrayPtr=r8Ptr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! fill the values from the coordX farray
  do j = lbound(r8Ptr,2),ubound(r8Ptr,2)
   do i = lbound(r8Ptr,1),ubound(r8Ptr,1)
    r8Ptr(i,j) = coordX(i)
   enddo
  enddo

  ! get an array from the grid to set the coord in the grid
  call ESMF_GridGetCoord(grid, &
                         coordDim=1, &
                         staggerloc=stagger, &
                         array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! a pointer to the array on this DE
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, localDE=lde, rc = rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! Set the first coord in the grid
  call ESMF_GridSetCoord(grid, &
                        coordDim=1, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! Retrieve a pointer to the second coord 
  call ESMF_GridGetCoord(grid, localDE=lde,&
                        coordDim=2, &
                        staggerloc=stagger, &
                        farrayPtr=r8Ptr, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! fill the values from the coordY farray
  do j = lbound(r8Ptr,2),ubound(r8Ptr,2)
   do i = lbound(r8Ptr,1),ubound(r8Ptr,1)
    r8Ptr(i,j) = coordY(j)
   enddo
  enddo

  ! get an array from the grid to set the coord in the grid
  call ESMF_GridGetCoord(grid, &
                         coordDim=2, &
                         staggerloc=stagger, &
                         array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! a pointer to the array on this DE
  call ESMF_ArrayGet(array2d, farrayPtr=r8Ptr, localDE=lde, rc = rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  ! Set the second coord in the grid
  call ESMF_GridSetCoord(grid, &
                        coordDim=2, &
                        staggerloc=stagger, &
                        array=array2d, rc=rc)
  if (ChkErr(rc,__LINE__,u_FILE_u)) return

  enddo !lde

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
