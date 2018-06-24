module AtmModel

#include "LocalDefs.F90"

  use ESMF
  use AtmFields
  use AtmFieldUtils, only : AtmFieldsToExport

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

              integer :: i,j
    character(len=12) :: fname
    integer, dimension(2)         ::  lb,  ub
    integer, dimension(2)         :: tlb, tub
    integer, dimension(2)         :: clb, cub

    rc = ESMF_SUCCESS

    call ESMF_LogWrite("User run routine AtmInit started", ESMF_LOGMSG_INFO)

    call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)

    ! Get bounds information 
    fname = trim(AtmFieldsToExport(1)%field_name)
    call ESMF_StateGet(exportState, itemName=trim(fname),field=field,rc=rc)
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
    print *,' AtmInit imin,imax,jmin,jmax:', &
             imin_e,imax_e,jmin_e,jmax_e

    call   AtmForce(gcomp,exportState,externalClock,rc)

    call ESMF_StateWrite(exportState, "test.nc", rc = rc)

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
    call AtmForce(gcomp,exportState,externalClock,rc)

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
