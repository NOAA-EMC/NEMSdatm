subroutine sigma2nc(cstring)
#ifdef debug
 
  use param
  use sigmavars
  use cdf
  use netcdf
  use charstrings

  implicit none

  character(len=*), intent(in) :: cstring

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: i,k,nv

   character(len=12) :: vname
   character(len=12) :: vunit
   character(len=60) :: vlong
             logical :: v3d

  character(len=200) :: cdffile

  !---------------------------------------------------------------------

  do nv = 1,nsigfields
     vname = trim(sgfields(nv)%varname)
     vunit = trim(sgfields(nv)%varunit)
     vlong = trim(sgfields(nv)%varlong)
       v3d =      sgfields(nv)%var3d
   cdffile = trim(rtsrc)//trim(rtname)//trim(sgfields(nv)%varname)//'.'//trim(cstring)//'.nc'
   sig3d = 0.0; sig2d = 0.0

   if(trim(vname) .eq.    'ps')then
     sig2d(:,:) =   psg(:,:)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.    'zs')then
     sig2d(:,:) =   zsg(:,:)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'dpres')then
     sig3d(:,:,1:kout) =  dpresg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'pres')then
     sig3d(:,:,1:kout) =   presg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'temp')then
     sig3d(:,:,1:kout) =   tempg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.  'spfh')then
     sig3d(:,:,1:kout) =      qg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif
   if(trim(vname) .eq.   'delz')then
     sig3d(:,:,1:kout) =  delzg(:,:,1:kout)
     call write_sigmacdf(trim(cdffile),vname,vunit,vlong,v3d,sig2d,sig3d,im,jm,kout,1)
   endif

  enddo
#endif

  end subroutine sigma2nc
