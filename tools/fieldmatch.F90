subroutine fieldmatch

 use param
 use sfcvars
  
 implicit none

  integer :: ii, nr

  character(len= 12) :: varname1, varname2
  character(len= 40) :: varlong1, varlong2

  !---------------------------------------------------------------------

  index = -1
  do ii = 1,idxtotal
   if(GFSsfc(ii)%req4datm)then
    varname1 = trim(GFSsfc(ii)%name)
    varlong1 = trim(GFSsfc(ii)%desc)
   do nr = 1,nrecs
    varname2 = trim(varnames(nr))
    varlong2 = trim( varlong(nr))

    if(varname1 .eq. varname2)index(ii) = nr
    ! save the precip index to make snow and rain
    if(varname2 .eq. 'prate_ave')nr_prcp = nr

    if((varname1 .eq. 'totprcp_ave') .and. &
        (varname2  .eq.  'prate_ave') .and. &
         (varlong2   .eq.       'sfc'))index(ii) = nr
     
    if((varname1   .eq. 'DLWRF') .and. &
        (varname2 .eq. 'dlwrf_ave') .and. &
         (varlong2  .eq.      'sfc'))index(ii) = nr

    if((varname1   .eq. 'ULWRF') .and. &
        (varname2 .eq. 'ulwrf_ave') .and. &
         (varlong2  .eq.      'sfc'))index(ii) = nr

    if((varname1   .eq. 'DSWRF') .and. &
        (varname2 .eq. 'dswrf_ave') .and. &
         (varlong2  .eq.      'sfc'))index(ii) = nr

    if((varname1   .eq. 'u10m') .and. &
        (varname2 .eq. 'ugrd') .and. &
         (varlong2  .eq. '10 m above gnd'))index(ii) = nr
  
    if((varname1   .eq. 'v10m') .and. &
        (varname2 .eq. 'vgrd') .and. &
         (varlong2  .eq. '10 m above gnd'))index(ii) = nr
  
    if((varname1   .eq.  'q2m') .and. &
        (varname2 .eq. 'spfh') .and. &
         (varlong2  .eq. '2 m above gnd'))index(ii) = nr

    if((varname1   .eq.  't2m') .and. &
        (varname2 .eq.    'tmp') .and. &
         (varlong2  .eq. '2 m above gnd'))index(ii) = nr

    if((varname1   .eq.    'dusfc') .and. &
        (varname2 .eq. 'uflx_ave') .and. &
         (varlong2  .eq.     'sfc'))index(ii) = nr

    if((varname1   .eq.    'dvsfc') .and. &
        (varname2 .eq. 'vflx_ave') .and. &
         (varlong2  .eq.     'sfc'))index(ii) = nr

    if((varname1   .eq.    'psurf') .and. &
        (varname2 .eq.     'pres') .and. &
         (varlong2  .eq.     'sfc'))index(ii) = nr

    if((varname1   .eq.  'slmsksfc') .and. &
        (varname2 .eq.     'land') .and. &
         (varlong2  .eq.     'sfc'))index(ii) = nr
   enddo !nr
   endif !required
  enddo !ii

  do ii = 1,idxtotal
   varname1 = trim(GFSsfc(ii)%name)
   varlong1 = trim(GFSsfc(ii)%desc)
   if(index(ii) .ne. -1)then
          nr = index(ii)
    varname2 = trim(varnames(nr))
    varlong2 = trim( varlong(nr))
    print '(i6,a14,a2,a40,a2,a14,a2,a16)',ii,trim(varname1),'  ',trim(varlong1),'  ', &
                                             trim(varname2),'  ',trim(varlong2)
   endif
  enddo
end subroutine fieldmatch
