module param

  implicit none

  ! "initialized at" hours
  integer, parameter ::  nhours = 4
  ! "forecast at" hours
  integer, parameter :: nfhours = 2

  ! retrieved from reading header of first sfcf and sigf file
  integer :: im,jm,nrecs,nlevs
  ! number of sigma variables defined
  integer :: nsigfields
  ! number of req fields at sfc
  integer :: idxtotal
  ! index match between req sfc fields and record number in sfc file
  integer, allocatable :: index(:)
  ! index of prate_ave in sfc file used to make snow and rain
  integer :: nr_prcp
end module param
