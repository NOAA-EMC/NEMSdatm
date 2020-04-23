subroutine AtmGridSetUp(grid,petCnt,gridname,tag,rc)

#include "LocalDefs.F90"

  use ESMF
  use AtmInternalFields, only : lPet, iatm, jatm, dirpath, cdate0, filename_base
  use AtmInternalFields, only : AtmIndexType

  use AtmGridUtils

  implicit none

  type(ESMF_Grid)         :: grid

           integer, intent( in) :: petCnt
  character(len=*), intent( in) :: gridname,tag
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Array)                 :: array2d

  character(len=ESMF_MAXSTR) :: filename
  character(len=ESMF_MAXSTR) :: msgString

  logical :: north_south
  integer :: i,j,lde,peX,peY,peList(2),localDECount
  integer(kind=ESMF_KIND_I4), pointer  :: i4Ptr(:,:)
  
  ! gaussian grid center coords
  real(kind=ESMF_KIND_R8), allocatable :: coordXc(:),coordYc(:)
  ! gaussian grid corner coords
  real(kind=ESMF_KIND_R8), allocatable :: coordXq(:),coordYq(:)
  ! gaussian grid landsfc mask
  real(kind=ESMF_KIND_R4), allocatable :: landsfc(:,:)

  filename = trim(dirpath)//trim(filename_base)//trim(cdate0)//'.nc'

  rc = ESMF_SUCCESS

  call ESMF_LogWrite(trim(tag)//" AtmGridSetUp routine started", ESMF_LOGMSG_INFO)
  write(msgString,*)'using grid file : ',trim(filename)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'petCnt = ',petCnt,' lPet =  ', lPet
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  !-------------------------------------------------------------------------------------
  ! read Gaussian coords from file. Native EMSF_ArrayRead does not read Y coord from
  ! file correctly
  !-------------------------------------------------------------------------------------

  allocate(coordXc(1:iatm)); allocate(coordXq(1:iatm  ))
  allocate(coordYc(1:jatm)); allocate(coordYq(1:jatm+1))

  call ReadCoordFromFile(trim(filename),trim('lon'),iatm,coordXc)
  call ReadCoordFromFile(trim(filename),trim('lat'),jatm,coordYc)

  write(msgString,*)'coordXc,   ',lPet,minval(real(coordXc,4)), &
                                       maxval(real(coordXc,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'coordYc,   ',lPet,minval(real(coordYc,4)), &
                                       maxval(real(coordYc,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! find the N-S ordering of the grid
  ! N(j=1):S(j=jatm) or S(j=1):N(j=jatm)
  north_south = .true.
  if(coordYc(2) .gt. coordYc(1))north_south = .false.

  if(north_south)then
   ! create corner points; npole at j=1, spole at jatm+1
            j = 1
   coordYq(j) =  90.0_ESMF_KIND_R8
            j = jatm+1
   coordYq(j) = -90.0_ESMF_KIND_R8
  else
   ! create corner points; spole at j=1, npole at jatm+1
            j = 1
   coordYq(j) = -90.0_ESMF_KIND_R8
            j = jatm+1
   coordYq(j) =  90.0_ESMF_KIND_R8
  endif
  do j = 2,jatm
   coordYq(j) = (coordYc(j-1) + coordYc(j))*0.5_ESMF_KIND_R8
  enddo
 
  ! like module_CPLFIELDS.F90 in GSM
  do i = 1,iatm
   coordXq(i) = 360.0_ESMF_KIND_R8/real(iatm) * (real(i) - 1.5_ESMF_KIND_R8)
  enddo
  
  write(msgString,*)'coordXq,   ',lPet,minval(real(coordXq,4)), &
                                       maxval(real(coordXq,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'coordYq,   ',lPet,minval(real(coordYq,4)), &
                                       maxval(real(coordYq,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  !-------------------------------------------------------------------------------------
  ! ESMF_ArrayRead can read the mask from the file, but it is the wrong type (r4) and
  ! so requires working around that. Easier to just get global mask array like for the
  ! coords
  !-------------------------------------------------------------------------------------

  allocate(landsfc(1:iatm,1:jatm))
  call ReadMaskFromFile(trim(filename), trim('slmsksfc'), landsfc)

  !-------------------------------------------------------------------------------------
  ! Create the gaussian grid and fill the coords and mask
  ! for now, this defaults to the default distribution where the first dimension is
  ! decomposed with all PEs
  !-------------------------------------------------------------------------------------
  grid = ESMF_GridCreate1PeriDim(maxIndex = (/iatm,jatm/), &
  !                               regDecomp = peList, &
                                 coordDep1=(/1,2/), &            ! grid is defined with 2d
                                 coordDep2=(/1,2/), &            ! lat,lon arrays
                                 periodicDim=1,&
                                 poleDim=2,&
                                 indexflag=AtmIndexType, &
                                 name=trim(gridname), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  call ESMF_GridGet(grid, localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  write(msgString,*)'localDECount ',lPet,localDECount
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  !-------------------------------------------------------------------------------------
  ! Allocate storage for coords and mask
  !-------------------------------------------------------------------------------------
  ! P (Center) stagger
  call ESMF_GridAddCoord(grid, &
                         staggerloc=ESMF_STAGGERLOC_CENTER, &
                         rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Q (Corner) stagger
  call ESMF_GridAddCoord(grid, &
                         staggerloc=ESMF_STAGGERLOC_CORNER, &
                         rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Mask
  call ESMF_GridAddItem(grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        itemTypeKind=ESMF_TYPEKIND_I4, &
                        rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Add coords
  !-------------------------------------------------------------------------------------

  call AddCoord2Grid(grid, ESMF_STAGGERLOC_CENTER, iatm, jatm  , coordXc, coordYc, rc)
  call AddCoord2Grid(grid, ESMF_STAGGERLOC_CORNER, iatm, jatm+1, coordXq, coordYq, rc)

  !-------------------------------------------------------------------------------------
  ! Add mask 
  !-------------------------------------------------------------------------------------

  do lde = 0,localDECount-1

  ! retrieve a pointer for the mask
  call ESMF_GridGetItem(grid, localDE=lde,&
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        farrayPtr=i4Ptr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !fill the value using the landsfc mask
  i4Ptr = 0
   do j = lbound(i4Ptr,2),ubound(i4Ptr,2)
    do i = lbound(i4Ptr,1),ubound(i4Ptr,1)
     if(landsfc(i,j) .eq. 1.0)i4Ptr(i,j) = int(landsfc(i,j))
    enddo
   enddo

  ! get an array from the grid to set the mask
  call ESMF_GridGetItem(grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array on this DE
  call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, localDE=lde, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Set the mask value in the grid
  call ESMF_GridSetItem(grid, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  enddo !lde
  !-------------------------------------------------------------------------------------
  ! Write coords and mask to file
  !-------------------------------------------------------------------------------------

  call WriteCoord(grid, ESMF_STAGGERLOC_CENTER, 1, 'atmlonc', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CENTER, 2, 'atmlatc', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CORNER, 1, 'atmlonq', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CORNER, 2, 'atmlatq', lPet, rc)

  call  WriteMask(grid, ESMF_STAGGERLOC_CENTER, 'atmmask', lPet, rc)

  deallocate(coordXc); deallocate(coordXq)
  deallocate(coordYc); deallocate(coordYq)
  deallocate(landsfc)

  call ESMF_LogWrite("User AtmGridSetUp routine ended", ESMF_LOGMSG_INFO)

end subroutine AtmGridSetUp
