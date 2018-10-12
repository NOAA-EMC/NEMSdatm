module param

  !integer, parameter ::  yrbeg = 2015, yrend = 2016
  integer, parameter ::  yrbeg = 2015, yrend = 2015
  integer, parameter :: nyears = (yrend-yrbeg)+1
  integer, parameter ::   nmon = 12

  integer, parameter, dimension(nyears) ::  monbeg = (/ 4/)
  integer, parameter, dimension(nyears) ::  monend = (/ 9/)
  !integer, parameter, dimension(nyears) ::  monbeg = (/ 4, 1/)
  !integer, parameter, dimension(nyears) ::  monend = (/12, 3/)

  ! retrieved from reading header of first sfcf and sigf file
  integer :: im,jm,nrecs,nlevs

end module param
