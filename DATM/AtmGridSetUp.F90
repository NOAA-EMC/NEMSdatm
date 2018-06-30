subroutine AtmGridSetUp(grid,petCnt,gridname,tag,rc)

#include "LocalDefs.F90"

  use ESMF
  use AtmFields, only : lPet, iatm, jatm
  use AtmFields, only : AtmIndexType
  use AtmFields, only : atmfsm, atmlonc, atmlatc

  use AtmGridUtils

  implicit none

  type(ESMF_Grid)         :: grid

           integer, intent( in) :: petCnt
  character(len=*), intent( in) :: gridname,tag
           integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Array)                 :: array2d
  type(ESMF_Distgrid)              :: distgrid
  type(ESMF_ArraySpec)             :: arrayspec

  character(len=ESMF_MAXSTR) :: gridfile
  character(len=ESMF_MAXSTR) :: dirpath = '/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/DATM-MOM6-CICE5/master-20180627/ATM/'
  character(len=ESMF_MAXSTR) :: filename = 'gdas.t18z.sfcf.2018041400.nc'
  character(len=ESMF_MAXSTR) ::  varname
  character(len=ESMF_MAXSTR) :: msgString

  integer :: i,j,blocklist(2)
  integer(kind=ESMF_KIND_I4), pointer  :: i4Ptr(:,:)
     real(kind=ESMF_KIND_R8), pointer  :: r8Ptr(:,:)

  ! gaussian grid center coords
  real(kind=ESMF_KIND_R8), allocatable :: coordXc(:),coordYc(:)
  ! gaussian grid corner coords
  real(kind=ESMF_KIND_R8), allocatable :: coordXq(:),coordYq(:)

  ! for now, hardwired file for retrieving mask and coords
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

  allocate(coordXc(1:iatm)); allocate(coordXq(1:iatm  ))
  allocate(coordYc(1:jatm)); allocate(coordYq(1:jatm+1))

  call ReadCoordFromFile(trim(gridfile),trim('lon'),iatm,coordXc)
  call ReadCoordFromFile(trim(gridfile),trim('lat'),jatm,coordYc)

  write(msgString,*)'coordXc,   ',lPet,minval(real(coordXc,4)), &
                                       maxval(real(coordXc,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
  write(msgString,*)'coordYc,   ',lPet,minval(real(coordYc,4)), &
                                       maxval(real(coordYc,4))
  call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

  ! create corner points; npole at j=1, spole at j=jatm+1
           j = 1
  coordYq(j) =  90.0_ESMF_KIND_R8
           j = jatm+1
  coordYq(j) = -90.0_ESMF_KIND_R8
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
  ! Create the gaussian grid and fill the coords and mask
  !-------------------------------------------------------------------------------------
  grid = ESMF_GridCreate1PeriDim(maxIndex=(/iatm,jatm/), &
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
  ! Add coords, then write them 
  !-------------------------------------------------------------------------------------

  call AddCoord2Grid(grid, ESMF_STAGGERLOC_CENTER, iatm, jatm  , coordXc, coordYc, rc)
  call AddCoord2Grid(grid, ESMF_STAGGERLOC_CORNER, iatm, jatm+1, coordXq, coordYq, rc)

  call WriteCoord(grid, ESMF_STAGGERLOC_CENTER, 1, 'atmlonc', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CENTER, 2, 'atmlatc', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CORNER, 1, 'atmlonq', lPet, rc)
  call WriteCoord(grid, ESMF_STAGGERLOC_CORNER, 2, 'atmlatq', lPet, rc)

  call  WriteMask(grid, ESMF_STAGGERLOC_CENTER, 'atmmask', lPet, rc)

  deallocate(coordXc); deallocate(coordXq)
  deallocate(coordYc); deallocate(coordYq)

  call ESMF_LogWrite("User AtmGridSetUp routine ended", ESMF_LOGMSG_INFO)

end subroutine AtmGridSetUp
