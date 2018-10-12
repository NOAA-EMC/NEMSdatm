subroutine sigma2nc(fname,cdate,lstep)
 
  use param
  use nemsio_module
  use sigmavars
  use gfstonc_sig
  use charstrings
  use cdf
  use netcdf

  implicit none

           integer, intent(in) :: lstep
  character(len=*), intent(in) :: fname
  character(len=6), intent(in) :: cdate

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  !integer :: i,k,im,jm,nlevs,nv
  integer :: i,k,nv

   character(len=12) :: vname
   character(len=12) :: vunit
   character(len=60) :: vlong
             logical :: vout
             logical :: v3d

  character(len=200) :: cdffile

  !---------------------------------------------------------------------

  call read_nemsio_griddata(trim(fname), im, jm, nlevs, ug, vg, &
                            tempg, zsg, psg, qg, ozg, cwmrg, dpresg, presg)

  !print *,lstep,cdate
  !---------------------------------------------------------------------

  do nv = 1,nsigvars
     vname = trim(sgfields(nv)%varname)
     vunit = trim(sgfields(nv)%varunit)
     vlong = trim(sgfields(nv)%varlong)
      vout = sgfields(nv)%output
       v3d = sgfields(nv)%var3d
   cdffile = trim(rtsrc)//trim(rtname)//'atmoutnc/'//trim(sgfields(nv)%varname)//'.nc'

    if(trim(vname) .eq. 'dpres')then
     sig3d(:,:,1:kout) = dpresg(:,:,1:kout)
     cdffile = trim(rtsrc)//trim(rtname)//trim(sgfields(nv)%varname)//'.'//trim(cdate)//'.nc'
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,lstep)
    endif

  enddo

  end subroutine sigma2nc
