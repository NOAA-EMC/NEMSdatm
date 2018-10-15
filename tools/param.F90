module param

  implicit none

  ! retrieved from reading header of first sfcf and sigf file
  integer :: im,jm,nrecs,nlevs
  ! number of sigma variables defined
  integer :: nsigfields
  ! number of req fields at sfc
  integer :: idxtotal
  ! record index for tprcp in sfc field, total precipitation rate
  integer :: nr_tprcp
  ! index match between req sfc fields and record number in sfc file
  integer, allocatable :: index(:)

end module param
