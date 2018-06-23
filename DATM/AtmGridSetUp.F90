subroutine AtmGridSetUp(grid,petCnt,idim,jdim,gridname,tag,rc)

#include "LocalDefs.F90"

  use ESMF
  use AtmFields

  implicit none

  type(ESMF_Grid)         :: grid

           integer, intent( in) :: petCnt,idim,jdim
  character(len=*), intent( in) :: gridname,tag
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Array)                 :: array2d
  type(ESMF_Array)                 :: array1d
  type(ESMF_Distgrid)              :: distgrid

  character(len=ESMF_MAXSTR) :: gridfile
  character(len=ESMF_MAXSTR) :: dirpath = '/scratch4/NCEPDEV/ocean/save/Denise.Worthen/NEMS_INPUT0.1/DAtm/'
  character(len=ESMF_MAXSTR) :: filename = 'gdas.20170731.t18z.sfcf006.nc'
  character(len=ESMF_MAXSTR) ::  varname
  character(len=ESMF_MAXSTR) :: msgString

  integer :: i,j,blocklist(2)
  integer(kind=ESMF_KIND_I4), pointer :: i4Ptr(:,:)

  ! Explicit Fortran arrays
     real(kind=ESMF_KIND_R8), dimension(idim)      :: coordX
     real(kind=ESMF_KIND_R8), dimension(jdim)      :: coordY
     real(kind=ESMF_KIND_R4), dimension(idim,jdim) :: rland
  integer(kind=ESMF_KIND_I4), dimension(idim,jdim) :: iland

  ! for now, hardwired file for retrieveing mask and coords
  gridfile = trim(dirpath)//trim(filename)

  rc = ESMF_SUCCESS

  call ESMF_LogWrite(trim(tag)//" AtmGridSetUp routine started", ESMF_LOGMSG_INFO)
  write(msgString,*)'using ',trim(gridfile),' with petCnt ', petCnt
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  !-------------------------------------------------------------------------------------
  ! read Gaussian coords and land mask from file 
  !-------------------------------------------------------------------------------------
  !Create a distgrid for the lon array
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/idim/),rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !Create an array from the fortran array
  array1d = ESMF_ArrayCreate(farray=coordX, &
                             distgrid=distgrid, &
                             indexflag=AtmIndexType, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array1d, farrayPtr=atmlonc, localDe=0, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  call ESMF_ArrayRead(array1d,trim(gridfile), variableName='lon',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,coordX

  !Create a distgrid for the lat array
  distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/jdim/), rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !Create an array from the fortran array
  array1d = ESMF_ArrayCreate(farray=coordY, &
                             distgrid=distgrid, &
                             indexflag=AtmIndexType, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array1d, farrayPtr=atmlatc, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  call ESMF_ArrayRead(array1d,trim(gridfile), variableName='lat',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,coordY

  !Create a distgrid for the mask array
  distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/idim,jdim/),rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !Create an array from the fortran array
  array2d = ESMF_ArrayCreate(farray=rland, &
                             distgrid=distgrid, &
                             indexflag=AtmIndexType, rc=rc)
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

  ! a pointer to the array
  call ESMF_ArrayGet(array2d, farrayPtr=atmfsm, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! rland contains values (0,1,2) for ocean,land,ice
  ! create an integer mask containing only land values
                       iland = 0
  where(rland .eq. 1.0)iland = int(rland)
  !print *,minval(rland),maxval(rland)
  !print *,minval(iland),maxval(iland)
  !-------------------------------------------------------------------------------------
  ! Create the gaussian grid and fill the coords and mask
  !-------------------------------------------------------------------------------------
  !Decomposed in x only
  blocklist = (/petCnt,1/)

  grid = ESMF_GridCreate1PeriDim(maxIndex=(/idim,jdim/), &
                                 regDecomp=blocklist, &
                                 coordDep1=(/1/), &
                                 coordDep2=(/2/), &
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

  !a pointer to the land array
  call ESMF_ArrayGet(array2d, farrayPtr=i4Ptr, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  i4Ptr = iland

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
  call ESMF_GridGetCoord(grid, coordDim=1, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array1d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array1d, farrayPtr=atmlonc, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  atmlonc = coordX

  ! Set the coord in the grid
  call ESMF_GridSetCoord(grid, coordDim=1, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array1d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! retrieve an array for the second coord
  call ESMF_GridGetCoord(grid, coordDim=2, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array1d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! a pointer to the array
  call ESMF_ArrayGet(array1d, farrayPtr=atmlatc, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  atmlatc = coordY

  ! Set the coord in the grid
  call ESMF_GridSetCoord(grid, coordDim=2, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array1d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Check
  !-------------------------------------------------------------------------------------

   varname =  'atmmask'; filename = trim(varname)//'.nc'
  call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

   call ESMF_ArrayWrite(array2d, &
                        fileName=filename, &
                        variableName=varname, &
                        overwrite=.true., rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

    write(msgString, *)'atmlonc ',minval(atmlonc),maxval(atmlonc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    write(msgString, *)'atmlatc ',minval(atmlatc),maxval(atmlatc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  call ESMF_LogWrite("User AtmGridSetUp routine ended", ESMF_LOGMSG_INFO)

end subroutine AtmGridSetUp
