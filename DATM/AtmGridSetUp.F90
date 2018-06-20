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

  character(len=ESMF_MAXSTR) :: gridfile
  character(len=ESMF_MAXSTR) :: dirpath = 'grids/'
  !character(len=ESMF_MAXSTR) :: filename
  !character(len=ESMF_MAXSTR) ::  varname
  character(len=ESMF_MAXSTR) :: msgString

  integer :: blocklist(2)

  gridfile = trim(dirpath)//'lsmask.19294.esmf.nc'
  !gridfile = trim(dirpath)//'lsmask.19294.esmf.qc.nc'

  rc = ESMF_SUCCESS

  call ESMF_LogWrite(trim(tag)//" AtmGridSetUp routine started", ESMF_LOGMSG_INFO)
  write(msgString,*)'using ',trim(gridfile),' with petCnt ', petCnt
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  if(petCnt < 12)then
   blocklist = (/petCnt,1/)
  else
   !blocklist = (/petCnt/2,petCnt/2/)
   blocklist = (/petCnt,1/)
  endif

  grid = ESMF_GridCreate1PeriDim(maxIndex=(/idim,jdim/), &
                                 regDecomp=blocklist, &
                                 coordDep1=(/1,2/), &
                                 coordDep2=(/1,2/), &
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
  ! Allocate storage for coords on each stagger
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
  ! Q(1,1) coord is to the NE of P(1,1) 
                         staggerAlign=(/1,1/), &
                         rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !-------------------------------------------------------------------------------------
  ! Allocate storage for mask on center stagger
  !-------------------------------------------------------------------------------------
  ! P (Center) stagger
  call ESMF_GridAddItem(grid, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        itemFlag=ESMF_GRIDITEM_MASK, &
                        itemTypeKind=ESMF_TYPEKIND_I4, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Get an Array from the Grid to set the Mask from a file
  call ESMF_GridGetItem(grid, itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read the mask from a file
  !call ESMF_ArrayRead(array2d,trim(gridfile), variableName='land',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Set the mask value in the grid
  call ESMF_GridSetItem(grid, itemFlag=ESMF_GRIDITEM_MASK, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  !-------------------------------------------------------------------------------------
  ! Add Center stagger coords from file
  !-------------------------------------------------------------------------------------
  ! Get an array for the first coord, center stagger
  call ESMF_GridGetCoord(grid, coordDim=1, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  !call ESMF_ArrayRead(array2d,trim(gridfile), variableName='geolon_t',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! point to the coord 
  call ESMF_ArrayGet(array2d, farrayPtr=atmlonc, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,atmlonc

  ! Get an array for the second coord, center stagger
  call ESMF_GridGetCoord(grid, coordDim=2, &
                        staggerloc=ESMF_STAGGERLOC_CENTER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  !call ESMF_ArrayRead(array2d,trim(gridfile), variableName='geolat_t',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
 
  ! point to the coord 
  call ESMF_ArrayGet(array2d, farrayPtr=atmlatc, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,atmlatc
#ifdef test
  !-------------------------------------------------------------------------------------
  ! Add Corner stagger coords from file
  !-------------------------------------------------------------------------------------
  ! Get an array for the first coord, corner stagger
  call ESMF_GridGetCoord(grid, coordDim=1, &
                        staggerloc=ESMF_STAGGERLOC_CORNER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  !call ESMF_ArrayRead(array2d,trim(gridfile), variableName='geolon_q',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! point to the coord 
  call ESMF_ArrayGet(array2d, farrayPtr=atmlonq, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,atmlonq

  ! Get an array for the second coord, corner stagger
  call ESMF_GridGetCoord(grid, coordDim=2, &
                        staggerloc=ESMF_STAGGERLOC_CORNER, &
                        array=array2d, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out

  ! Read from a file
  !call ESMF_ArrayRead(array2d,trim(gridfile), variableName='geolat_q',rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
 
  ! point to the coord 
  call ESMF_ArrayGet(array2d, farrayPtr=atmlatq, rc = rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    return  ! bail out
  !print *,atmlatq
#endif
#ifdef test
  ! Check
   varname =  'atmlonc'; filename = trim(varname)//'.nc'
   call ESMF_GridGetCoord(grid, coordDim=1, &
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

   varname =  'atmlatc'; filename = trim(varname)//'.nc'
   call ESMF_GridGetCoord(grid, coordDim=2, &
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


   varname =  'atmlonq'; filename = trim(varname)//'.nc'
   call ESMF_GridGetCoord(grid, coordDim=1, &
                          staggerloc=ESMF_STAGGERLOC_CORNER, &
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

   varname =  'atmlatq'; filename = trim(varname)//'.nc'
   call ESMF_GridGetCoord(grid, coordDim=2, &
                          staggerloc=ESMF_STAGGERLOC_CORNER, &
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
#endif
    write(msgString, *)'atmlonc ',minval(atmlonc),maxval(atmlonc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    write(msgString, *)'atmlatc ',minval(atmlatc),maxval(atmlatc)
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    !write(msgString, *)'atmlonq ',minval(atmlonq),maxval(atmlonq)
    !call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    !write(msgString, *)'atmlatq ',minval(atmlatq),maxval(atmlatq)
    !call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  call ESMF_LogWrite("User AtmGridSetUp routine ended", ESMF_LOGMSG_INFO)

end subroutine AtmGridSetUp
