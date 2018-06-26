subroutine AtmGridSetUp(grid,petCnt,idim,jdim,gridname,tag,rc)

#include "LocalDefs.F90"

  use ESMF
  use AtmFields, only : lPet, AtmIndexType
  use AtmFields, only : atmfsm, atmlonc, atmlatc

  implicit none

  type(ESMF_Grid)         :: grid

           integer, intent( in) :: petCnt,idim,jdim
  character(len=*), intent( in) :: gridname,tag
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Array)                 :: array2d
  type(ESMF_Distgrid)              :: distgrid
  type(ESMF_ArraySpec)             :: arrayspec

  character(len=ESMF_MAXSTR) :: gridfile
  character(len=ESMF_MAXSTR) :: dirpath = '/scratch4/NCEPDEV/ocean/save/Denise.Worthen/NEMS_INPUT0.1/DAtm/'
  character(len=ESMF_MAXSTR) :: filename = 'gdas.20170731.t18z.sfcf006.nc'
  character(len=ESMF_MAXSTR) ::  varname
  character(len=ESMF_MAXSTR) :: msgString

  integer :: i,j,blocklist(2)
  integer(kind=ESMF_KIND_I4), pointer  :: i4Ptr(:,:)
     real(kind=ESMF_KIND_R8), pointer  :: r8Ptr(:,:)

  real(kind=ESMF_KIND_R8), allocatable :: coordX(:),coordY(:)

  ! for now, hardwired file for retrieveing mask and coords
  gridfile = trim(dirpath)//trim(filename)

  rc = ESMF_SUCCESS

  call ESMF_LogWrite(trim(tag)//" AtmGridSetUp routine started", ESMF_LOGMSG_INFO)
  write(msgString,*)'using ',trim(gridfile)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'petCnt = ',petCnt,' lPet =  ', lPet
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  !Decomposed in x only
  !blocklist = (/petCnt,1/)
  !Decomposed in y only
  !blocklist = (/1,petCnt/)
  !-------------------------------------------------------------------------------------
  ! read Gaussian coords from file. Native EMSF_ArrayRead does not read Y coord from
  ! file correctly
  !-------------------------------------------------------------------------------------

  allocate(coordX(1:idim))
  allocate(coordY(1:jdim))

  call ReadCoordFromFile(trim(gridfile),trim('lon'),idim,coordX)
  call ReadCoordFromFile(trim(gridfile),trim('lat'),jdim,coordY)

  write(msgString,*)'coordX,   ',lPet,minval(real(coordX,4)), &
                                      maxval(real(coordX,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'coordY,   ',lPet,minval(real(coordY,4)), &
                                      maxval(real(coordY,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  !-------------------------------------------------------------------------------------
  ! Create the gaussian grid and fill the coords and mask
  !-------------------------------------------------------------------------------------
  grid = ESMF_GridCreate1PeriDim(maxIndex=(/idim,jdim/), &
                                 !regDecomp=blocklist, &
                                 coordDep1=(/1,2/), &            ! grid is defined with 2d
                                 coordDep2=(/1,2/), &            ! lat,lon arrays
                                 periodicDim=1,&
                                 poleDim=2,&
                                 polekindflag=(/ESMF_POLEKIND_MONOPOLE, &
                                                ESMF_POLEKIND_MONOPOLE/), &
                                 indexflag=AtmIndexType, &
                                 name=trim(gridname), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
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

  call ESMF_GridAddItem(grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        itemTypeKind=ESMF_TYPEKIND_I4, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Q (Corner) stagger
  !call ESMF_GridAddCoord(grid, &
  !                       staggerloc=ESMF_STAGGERLOC_CORNER, &
  !                       rc=rc)
  !if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !  line=__LINE__, &
  !  file=__FILE__)) &
  !  return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Read the mask from a file
  !-------------------------------------------------------------------------------------
  ! get the distgrid
  call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! A rank2, r4 arrayspec for the mask
  call ESMF_ArraySpecSet(arrayspec, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !Create an array
  array2d = ESMF_ArrayCreate(arrayspec=arrayspec, &
                             distgrid=distgrid, &
                             indexflag=AtmIndexType, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array2d, farrayPtr=atmfsm, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read the mask from a file
  call ESMF_ArrayRead(array2d,trim(gridfile), variableName='landsfc',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  write(msgString,*)'atmfsm,   ',lPet,lbound(atmfsm,1),ubound(atmfsm,1), &
                                      lbound(atmfsm,2),ubound(atmfsm,2), &
                                      minval(real(atmfsm,4)), &
                                      maxval(real(atmfsm,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  !-------------------------------------------------------------------------------------
  ! Add mask 
  !-------------------------------------------------------------------------------------
  ! retrieve an array for the mask
  call ESMF_GridGetItem(grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !a pointer to the mask array
  call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  write(msgString,*)'i4Ptr,   ',lPet,lbound(i4Ptr,1),ubound(i4Ptr,1), &
                                     lbound(i4Ptr,2),ubound(i4Ptr,2)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  !fill the value using the landsfc mask
                        i4Ptr = 0
  where(atmfsm .eq. 1.0)i4Ptr = int(atmfsm)

  ! Set the mask value in the grid
  call ESMF_GridSetItem(grid, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Add coords 
  !-------------------------------------------------------------------------------------
  ! retrieve an array for the first coord
  call ESMF_GridGetCoord(grid, &
                        coordDim=1, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
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

  ! fill the values from the coordX farray
  do i = lbound(r8Ptr,1),ubound(r8Ptr,1)
   r8Ptr(i,:) = coordX(i)
  enddo

  ! Set the coord in the grid, not required?
  !call ESMF_GridSetCoord(grid, &
  !                      coordDim=1, &
  !                      staggerloc=ESMF_STAGGERLOC_CENTER, &
  !                      array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! retrieve an array for the second coord
  call ESMF_GridGetCoord(grid, &
                        coordDim=2, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
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
  !call ESMF_GridSetCoord(grid, &
  !                      coordDim=2, &
  !                      staggerloc=ESMF_STAGGERLOC_CENTER, &
  !                      array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Check and write to file
  !-------------------------------------------------------------------------------------
  ! retrieve the dim1 coord
   call ESMF_GridGetCoord(grid, &
                          coordDim=1, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
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
  write(msgString,*)'r8Ptr, dim 1, lPet ',lPet,lbound(r8Ptr,1),ubound(r8Ptr,1), &
                                               lbound(r8Ptr,2),ubound(r8Ptr,2), &
                                          minval(real(r8Ptr,4)), &
                                          maxval(real(r8Ptr,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! write to a file
  varname =  'atmlonc'; filename = trim(varname)//'.nc'
  call ESMF_ArrayWrite(array2d, &
                       fileName=filename, &
                       variableName=varname, &
                       overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! retrieve the dim2 coord
   call ESMF_GridGetCoord(grid, &
                          coordDim=2, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
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
  write(msgString,*)'r8Ptr, dim 2, lPet ',lPet,lbound(r8Ptr,1),ubound(r8Ptr,1), &
                                               lbound(r8Ptr,2),ubound(r8Ptr,2), &
                                          minval(real(r8Ptr,4)), &
                                          maxval(real(r8Ptr,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! write to a file
  varname =  'atmlatc'; filename = trim(varname)//'.nc'
  call ESMF_ArrayWrite(array2d, &
                       fileName=filename, &
                       variableName=varname, &
                       overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

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
  write(msgString,*)'i4Ptr,   ',lPet,lbound(i4Ptr,1),ubound(i4Ptr,1), &
                                      lbound(i4Ptr,2),ubound(i4Ptr,2), &
                                      minval(i4Ptr), &
                                      maxval(i4Ptr)
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! write to a file
   varname =  'atmmask'; filename = trim(varname)//'.nc'
   call ESMF_ArrayWrite(array2d, &
                        fileName=filename, &
                        variableName=varname, &
                        overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  deallocate(coordX)
  deallocate(coordY)

  call ESMF_LogWrite("User AtmGridSetUp routine ended", ESMF_LOGMSG_INFO)

end subroutine AtmGridSetUp

subroutine ReadCoordFromFile(filename,coordname,coorddim,coordarray)

  use ESMF
  use netcdf

  implicit none

          character(len=*), intent(in) :: filename, coordname
                   integer, intent(in) :: coorddim

  real(kind=ESMF_KIND_R8), intent(out) :: coordarray(coorddim)

  integer :: ncid, varid, rc

  rc = nf90_open(trim(filename), nf90_nowrite, ncid)
  rc = nf90_inq_varid(ncid, trim(coordname), varid)
  rc = nf90_get_var(ncid, varid, coordarray)
  rc = nf90_close(ncid)

end subroutine ReadCoordFromFile
