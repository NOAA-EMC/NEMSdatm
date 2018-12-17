module AtmImportFields
 
#include "LocalDefs.F90"
 
#ifdef coupled
! when run w/ Mediator as component in coupled app, at least
! one import field seems to be required
  use ESMF
  use AtmInternalFields, only : AtmField_Definition

  implicit none

  private

  type(AtmField_Definition),   public :: AtmFieldsToImport(1)

  !-----------------------------------------------------------------------------
  ! Fortran array pointers
  ! Fields for DAtm model 
  ! in Atm importState
  !-----------------------------------------------------------------------------
  real(kind=ESMF_KIND_R8), dimension(:,:), public, pointer :: land_mask

  ! called by Cap
  public :: AtmImportFieldsSetUp

  contains

  !-----------------------------------------------------------------------------

  subroutine AtmImportFieldsSetUp

  integer :: ii
  character(len=ESMF_MAXSTR) :: msgString

  ! default values
  AtmFieldsToImport(:)%staggertype = 'center'

    ii = 0
  !-----------------------------------------------------------------------------
  !Atm Import Fields (req by Mediator ?)
  !-----------------------------------------------------------------------------

    ii = ii + 1
    AtmFieldsToImport(ii)%standard_name = 'land_mask'
    AtmFieldsToImport(ii)%field_name    = 'LandMask'
    AtmFieldsToImport(ii)%file_varname  = 'slmsksfc'
    AtmFieldsToImport(ii)%unit_name     = '1'
    AtmFieldsToImport(ii)%farrayPtr     => land_mask
  
  !-----------------------------------------------------------------------------
  ! check
  !-----------------------------------------------------------------------------
    if(ii .ne. size(AtmFieldsToImport)) &
    call ESMF_LogWrite("ERROR: check # AtmFieldsToImport", ESMF_LOGMSG_INFO)

    call ESMF_LogWrite('AtmFieldsToImport : ', ESMF_LOGMSG_INFO)
    do ii = 1,size(AtmFieldsToImport)
     write(msgString,'(i6,2(a2,a14),a2,a)')ii,'  ',trim(AtmFieldsToImport(ii)%file_varname), &
                                              '  ',trim(AtmFieldsToImport(ii)%field_name), &
                                              '  ',trim(AtmFieldsToImport(ii)%standard_name)
     call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    enddo

  end subroutine AtmImportFieldsSetUp
#endif
end module AtmImportFields
