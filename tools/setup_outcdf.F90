subroutine setup_outcdf(cdffile)

  use param
  use cdf
  use sfcvars
  use netcdf

  implicit none

  character(len=*), intent(in) :: cdffile

  integer :: ii

  character(len=20) :: vname
  character(len=10) :: vunit
  character(len=40) :: vlong

  !-----------------------------------------------------------------------------

   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)
   !print *,trim(nf90_strerror(rc))

   rc = nf90_def_dim(ncid,   'lon',             im,     xtdim)
   rc = nf90_def_dim(ncid,   'lat',             jm,     ytdim)
   rc = nf90_def_dim(ncid,  'time', nf90_unlimited,     tdim)

   dim1(1) = xtdim
   rc = nf90_def_var(ncid,   'lon', nf90_float,    dim1, xtid)
   rc = nf90_put_att(ncid, xtid,       'units', 'degrees_east')
   rc = nf90_put_att(ncid, xtid,   'long_name',    'Longitude')
   rc = nf90_put_att(ncid, xtid,      'modulo',            ' ')

   dim1(1) = ytdim
   rc = nf90_def_var(ncid,   'lat', nf90_float,     dim1, ytid)
   rc = nf90_put_att(ncid, ytid,       'units', 'degrees_north')
   rc = nf90_put_att(ncid, ytid,   'long_name',      'Latitude')

   dim1(1) =  tdim
   rc = nf90_def_var(ncid, 'time', nf90_double,  dim1,      timid)
   rc = nf90_put_att( ncid, timid,            'units', trim(torg))
   rc = nf90_put_att( ncid, timid,         'calendar', trim(tcal))
   rc = nf90_put_att( ncid, timid,             'axis',        'T')

   do ii = 1,idxtotal
    vname = trim(GFSsfc(ii)%name)
    vlong = trim(GFSsfc(ii)%desc)
    vunit = trim(GFSsfc(ii)%unit)
    if(GFSsfc(ii)%req4datm)then
     dim3(3) =  tdim
     dim3(2) = ytdim
     dim3(1) = xtdim
     rc = nf90_def_var(ncid, vname, nf90_float,  dim3, datid)
     rc = nf90_put_att(ncid, datid,          'units',  vunit)
     rc = nf90_put_att(ncid, datid,      'long_name',  vlong)
    endif
   enddo
     rc = nf90_put_att(ncid, nf90_global, 'history', trim(history))
     rc = nf90_enddef(ncid)

     rc = nf90_put_var(ncid,  xtid,   lons)
     rc = nf90_put_var(ncid,  ytid,   lats)
     rc = nf90_put_var(ncid, timid, tstamp)
     rc = nf90_close(ncid)
end subroutine setup_outcdf
