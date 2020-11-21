subroutine AtmBundleCreate(gcomp, exportState, rc)

  use ESMF

  use AtmInternalFields

  implicit none

  type(ESMF_GridComp)  :: gcomp
  type(ESMF_State)     :: exportState
  integer, intent(out) :: rc

  ! Local variables
  type(ESMF_Grid)               :: grid
  type(ESMF_ArraySpec)          :: arrayspecR4
  type(ESMF_StaggerLoc)         :: staggerloc
  type(ESMF_Field)              :: field

  integer :: ii,nfields

  character(len=ESMF_MAXSTR)    :: aname, fnameb, fnamef
  character(len=ESMF_MAXSTR)    :: msgString
  character(len=*),parameter    :: u_FILE_u = &
     __FILE__

  ! Initialize return code
  rc = ESMF_SUCCESS

  call ESMF_LogWrite("User init routine AtmBundleCreate called", ESMF_LOGMSG_INFO)

  ! Get the component grid
  call ESMF_GridCompGet(gcomp, grid=grid, rc=rc)
  ! create the empty field bundle
  AtmBundleFwd=ESMF_FieldBundleCreate(name='Atm model field bundle Fwd',rc=rc)
  AtmBundleBak=ESMF_FieldBundleCreate(name='Atm model field bundle Bak',rc=rc)
  ! Create an ArraySpec, R4
  call ESMF_ArraySpecSet(arrayspecR4, rank=2, typekind=ESMF_TYPEKIND_R4, rc=rc)

  ! Define and Add Fields to the AtmBundle
  do ii = 1,size(AtmBundleFields)
    if(trim(AtmBundleFields(ii)%staggertype) .eq. 'center')staggerloc = ESMF_STAGGERLOC_CENTER

    ! a named field for the 'back' values
     aname = trim(AtmBundleFields(ii)%field_name)//'_bak'
    field  = ESMF_FieldCreate(grid=grid, &
                              arrayspec=arrayspecR4, &
                              indexflag=AtmIndexType, &
                              staggerloc=staggerloc, &
                              name=trim(aname), rc=rc)

    call ESMF_FieldBundleAdd(AtmBundleBak, (/field/), rc=rc)
    call ESMF_FieldGet(field, farrayPtr=AtmBundleFields(ii)%farrayPtr_bak, rc=rc)

    ! a named field for the 'foward' values
     aname = trim(AtmBundleFields(ii)%field_name)//'_fwd'
    field  = ESMF_FieldCreate(grid=grid, &
                              arrayspec=arrayspecR4, &
                              indexflag=AtmIndexType, &
                              staggerloc=staggerloc, &
                              name=trim(aname), rc=rc)

    call ESMF_FieldBundleAdd(AtmBundleFwd, (/field/), rc=rc)
    call ESMF_FieldGet(field, farrayPtr=AtmBundleFields(ii)%farrayPtr_fwd, rc=rc)

    AtmBundleFields(ii)%farrayPtr_bak = 0.0_ESMF_KIND_R8
    AtmBundleFields(ii)%farrayPtr_fwd = 0.0_ESMF_KIND_R8
  enddo !ii
 
  !-------------------------------------------------------------------
  ! Document what is in the Bundle
  !-------------------------------------------------------------------
 
  call ESMF_FieldBundleGet(AtmBundleBak,fieldCount=nfields,rc=rc)
  ii = nfields
  call ESMF_FieldBundleGet(AtmBundleFwd,fieldCount=nfields,rc=rc)

  if(ii .ne. nfields)then
   write(msgString,'(a)')'Number of fwd and bak fields do not match'
   call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
   call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif

  do ii = 1,nfields
   call ESMF_FieldBundleGet(AtmBundleBak,ii,field=field,rc=rc)
   call ESMF_FieldGet(field,name=fnameb,rc=rc)
   call ESMF_FieldBundleGet(AtmBundleFwd,ii,field=field,rc=rc)
   call ESMF_FieldGet(field,name=fnamef,rc=rc)

   write(msgString,'(i4,2a14,a16,2a14,a16)')ii,' added field ',trim(fnameb),' to AtmBundleBak', &
                                               '   and field ',trim(fnamef),' to AtmBundleFwd'
   call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
  enddo

  call ESMF_LogWrite("User init routine AtmBundleCreate finished", ESMF_LOGMSG_INFO)

end subroutine AtmBundleCreate
