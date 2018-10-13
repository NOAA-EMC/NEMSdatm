subroutine sigma2nc(fname,cstring)
 
  use param
  use nemsio_module
  use sigmavars
  use gfstonc_sig
  use charstrings
  use cdf
  use netcdf

  implicit none

  character(len=*), intent(in) :: fname,cstring

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: i,k,nv

   character(len=12) :: vname
   character(len=12) :: vunit
   character(len=60) :: vlong

  character(len=200) :: cdffile

  !---------------------------------------------------------------------

  call read_nemsio_griddata(trim(fname), im, jm, nlevs, ug, vg, &
                            tempg, zsg, psg, qg, ozg, cwmrg, dpresg, presg)

  do k = 1,nlevs
   print *,k,presg(im/2,jm/2,k)
  !print *,minval(presg(:,:,k)),maxval(presg(:,:,k))
  enddo

  !---------------------------------------------------------------------
#ifdef debug
  do nv = 1,nsigvars
     vname = trim(sgfields(nv)%varname)
     vunit = trim(sgfields(nv)%varunit)
     vlong = trim(sgfields(nv)%varlong)
   cdffile = trim(rtsrc)//trim(rtname)//trim(sgfields(nv)%varname)//'.nc'
   sig3d = 0.0

   if(trim(vname) .eq.  'temp')then
     sig3d(:,:,1:kout) =   tempg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'pres')then
     sig3d(:,:,1:kout) =   presg(:,:,1:kout)
     !call write_sigmacdf(trim(cdffile),vname,vunit,vlong,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'spfh')then
     sig3d(:,:,1:kout) =      qg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'dpres')then
     sig3d(:,:,1:kout) =  dpresg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,sig3d,im,jm,kout,1)
   endif
  enddo
#endif

  end subroutine sigma2nc
