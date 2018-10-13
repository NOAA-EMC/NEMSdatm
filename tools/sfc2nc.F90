subroutine sfc2nc(fname,cstring)
 
  use param
  use nemsio_module
  use sfcvars
  use gfstonc_sfc
  use charstrings
  use cdf
  use netcdf

  implicit none

  character(len=*), intent(in) :: fname,cstring

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: nr

  character(len= 32) :: vname, vlong, vunit
             logical :: vout
  character(len=200) :: cdffile

  !---------------------------------------------------------------------

  call read_nemsio_2dgriddata(trim(fname),im,jm,nrecs,inames,itypes,grd2d)

  do nr = 1,nrecs
     vname = trim(varnames(nr))
     vlong = trim( varlong(nr))
#ifdef test
   ! there are 2 fields with name 'tmp'
   if((trim(vname) .eq. 'tmp') .and. &
       (trim(vlong) .eq. '2 m above gnd'))then
      vname = 't2m'
  !    vunit = 'degC'
  ! a2d(:,:) = grd2d(:,:,nr) - 273.15
      vunit = 'K'
   a2d(:,:) = grd2d(:,:,nr) 
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if((trim(vname) .eq. 'spfh') .and. &
       (trim(vlong) .eq. '2 m above gnd'))then
      vname = 'q2m'
   !   vunit = 'g/kg'
   !a2d(:,:) = grd2d(:,:,nr)*1.0e3
      vunit = 'kg/kg'
   a2d(:,:) = grd2d(:,:,nr)
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if(trim(vname) .eq. 'tisfc')then 
      vlong = 'ice sfc temp?'
      !vunit = 'degC'
   !a2d(:,:) = grd2d(:,:,nr) - 273.15
      vunit = 'K'
   a2d(:,:) = grd2d(:,:,nr) 
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if(trim(vname) .eq. 'icec')then
      vunit = ' '
      vlong = 'ice concentration'
   a2d(:,:) = grd2d(:,:,nr) 
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if(trim(vname) .eq. 'land')then
      vunit = ' '
      vlong = '0 (ocean); 1 (land); 2 (ice)'
   a2d(:,:) = grd2d(:,:,nr)
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if(trim(vname) .eq. 'salbd')then
      vunit = ' '
      vlong = 'ratio net sw/down ?'
   a2d(:,:) = grd2d(:,:,nr)
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif

   if((trim(vname) .eq. 'tmp') .and. &
       (trim(vlong) .eq. 'sfc'))then
      vname = 'ts'
   !   vunit = 'degC'
   !a2d(:,:) = grd2d(:,:,nr) - 273.15
      vunit = 'K'
   a2d(:,:) = grd2d(:,:,nr)
    cdffile = trim(rtsrc)//trim(rtname)//trim(vname)//'.nc'
    call write_sfccdf(trim(cdffile),vname,vunit,vlong,a2d,im,jm,1)
   endif
#endif

  enddo

  end subroutine sfc2nc
