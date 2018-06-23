module AtmModel

#include "LocalDefs.F90"

  use ESMF
  use AtmFields

  implicit none

  private

  ! called by Cap
  public :: AtmInit, AtmRun, AtmFinal

  contains

  subroutine AtmInit(gcomp, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc

    ! Local variables
    type(ESMF_Grid)                :: grid
    type(ESMF_Field)               :: field

    integer(kind=ESMF_KIND_I4), pointer :: i4Ptr(:,:)

    integer :: i,j
    integer, dimension(2)         ::  lb,  ub
    integer, dimension(2)         :: tlb, tub
    integer, dimension(2)         :: clb, cub

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User run routine AtmInit started", ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

#ifdef test
    ! Get bounds information 
    call ESMF_StateGet(exportState, itemName='T2m',field=field,rc=rc)
    call ESMF_FieldGetBounds(field, localDE=0, &
                             totalLBound=tlb, totalUBound=tub, &
                             computationalLBound=clb, computationalUBound=cub, &
                             exclusiveLBound=lb, exclusiveUBound=ub, rc=rc)

    ! Exclusive region: data unique to this DE, can be used as source for halo operation
    imin_e =  lb(1); imax_e =  ub(1)
    jmin_e =  lb(2); jmax_e =  ub(2)
    ! Total region: data plus the halo widths
    imin_t = tlb(1); imax_t = tub(1)
    jmin_t = tlb(2); jmax_t = tub(2)
    ! Computational
    imin_c = clb(1); imax_c = cub(1)
    jmin_c = clb(2); jmax_c = cub(2)
    !print '(a5,i4,a41,4i6)','Pet#',lPet,' AtmInit imin,imax,jmin,jmax:', &
    !                                      imin_e,imax_e,jmin_e,jmax_e
    ! Get Coord information from Grid
    call ESMF_GridGetCoord(grid, coordDim=1, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=atmlonc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

     call ESMF_GridGetCoord(grid, coordDim=2, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=atmlatc, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
   
    ! Get the mask from the grid
    call ESMF_GridGetItem(grid, &
                          itemFlag=ESMF_GRIDITEM_MASK, &
                          staggerloc=ESMF_STAGGERLOC_CENTER, &
                          farrayPtr=i4Ptr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    allocate(atmfsm(lbound(i4Ptr,1):ubound(i4Ptr,1),lbound(i4Ptr,2):ubound(i4Ptr,2)))
    do j = jmin_e,jmax_e
     do i = imin_e,imax_e
      atmfsm(i,j) = real(i4Ptr(i,j),4)
     enddo
    enddo
    !print *,atmlonc
    !print *,
    !print *,atmlatc
#endif

    !call   AtmForce(gcomp,exportState,externalClock,rc)

    call ESMF_LogWrite("User run routine AtmInit finished", ESMF_LOGMSG_INFO)
   
  end subroutine AtmInit

  !-----------------------------------------------------------------------------

  subroutine AtmRun(gcomp, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc
   
    integer :: counter = 0

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User run routine AtmRun started", ESMF_LOGMSG_INFO)

    ! Increment counter
    counter = counter + 1
    !call AtmForce(gcomp,exportState,externalClock,rc)

    ! Prints Atm fields on Atm grid
    !call AtmPrint(gcomp, importState, exportState, externalClock, counter, rc)

    call ESMF_LogWrite("User run routine AtmRun finished", ESMF_LOGMSG_INFO)

  end subroutine AtmRun

  !-----------------------------------------------------------------------------

  subroutine AtmFinal(gcomp, importState, exportState, externalClock, rc)

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState
    type(ESMF_State)     :: exportState
    type(ESMF_Clock)     :: externalClock
    integer, intent(out) :: rc
 
    call ESMF_LogWrite("User finalize routine AtmFinal started", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User finalize routine AtmFinal finished", ESMF_LOGMSG_INFO)
  end subroutine AtmFinal
end module AtmModel
