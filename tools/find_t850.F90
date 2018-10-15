subroutine find_t850(t850)

  use param
  use sigmavars

  implicit none

  real, dimension(im,jm), intent(out) :: t850

  real(kind=4), parameter :: p850    = 85000.0

  integer :: i,j,k

  presg = 0.0
   t850 = tempg(:,:,1)
   do j = 1,jm
    do i = 1,im
! fv3gfs worflow sorc/gfs_bufr.fd/meteorg.f
       presg(i,j,1) = psg(i,j)
      do k = 2,nlevs
       presg(i,j,k) = presg(i,j,k-1) - dpresg(i,j,k-1)
      enddo

! gfsphysics/GFS_layer/GFS_physics_driver.F90
      do k = 1,nlevs-1
       if (presg(i,j,k) > p850 .and. presg(i,j,k+1) <= p850) then
         t850(i,j) = tempg(i,j,k) - (presg(i,j,k)-p850) / &
                    (presg(i,j,k)-presg(i,j,k+1)) *      &
                    (tempg(i,j,k)-tempg(i,j,k+1))
       endif
      enddo

    enddo
   enddo

   !i = im/2; j = jm/2
   !do k = 1,nlevs
   ! print *,k,presg(i,j,k),tempg(i,j,k),t850(i,j)
   !enddo  
end subroutine find_t850
