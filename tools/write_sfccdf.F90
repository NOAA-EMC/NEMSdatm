subroutine write_sfccdf(cdffile, vname, vunit, vlong, sfc2d, idim, jdim, lstep)

  use charstrings
  use cdf
  use netcdf

  use sfcvars, only : lats, lons

  implicit none

           integer, intent(in) :: idim, jdim, lstep
  character(len=*), intent(in) :: cdffile
  character(len=*), intent(in) :: vname
  character(len=*), intent(in) :: vunit
  character(len=*), intent(in) :: vlong

  real(kind=4), dimension(idim,jdim), intent(in)   :: sfc2d

  !-----------------------------------------------------------------------------

  if(lstep .eq. 1)then
   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)
   print *,trim(nf90_strerror(rc))

   rc = nf90_def_dim(ncid,    'Xt',          idim,     xtdim)
   rc = nf90_def_dim(ncid,    'Yt',          jdim,     ytdim)
   rc = nf90_def_dim(ncid,  'time', nf90_unlimited,     tdim)

   dim1(1) = xtdim
   rc = nf90_def_var(ncid,     'Xt', nf90_float,    dim1, xtid)
   rc = nf90_put_att(ncid, xtid,       'units', 'degrees_east')
   rc = nf90_put_att(ncid, xtid,   'long_name',    'Longitude')
   rc = nf90_put_att(ncid, xtid,      'modulo',            ' ')

   dim1(1) = ytdim
   rc = nf90_def_var(ncid,     'Yt', nf90_float,     dim1, ytid)
   rc = nf90_put_att(ncid, ytid,       'units', 'degrees_north')
   rc = nf90_put_att(ncid, ytid,   'long_name',      'Latitude')

   dim1(1) =  tdim
   rc = nf90_def_var(ncid, 'time', nf90_double,  dim1,      timid)
   rc = nf90_put_att( ncid, timid,            'units', trim(torg))
   rc = nf90_put_att( ncid, timid,         'calendar', trim(tcal))
   rc = nf90_put_att( ncid, timid,             'axis',        'T')

  ! 2-d
   dim3(3) =  tdim
   dim3(2) = ytdim
   dim3(1) = xtdim
   rc = nf90_def_var(ncid, vname, nf90_float,  dim3, datid)
   rc = nf90_put_att(ncid, datid,          'units',  vunit)
   rc = nf90_put_att(ncid, datid,      'long_name',  vlong)

   rc = nf90_enddef(ncid)

   rc = nf90_put_var(ncid,  xtid,   lons)
   rc = nf90_put_var(ncid,  ytid,   lats)
   rc = nf90_close(ncid)
  endif 

  !-----------------------------------------------------------------------------

  if(lstep .ge. 1)then
   rc = nf90_open(trim(cdffile), nf90_write, ncid)

   corner1(1) = lstep
     edge1(1) = 1
   rc = nf90_inq_varid(ncid, 'time',  timid)
   rc = nf90_put_var(ncid, timid, tstamp, corner1)

    corner3(3) = lstep
    corner3(2) = 1
    corner3(1) = 1
      edge3(3) = 1
      edge3(2) = jdim
      edge3(1) = idim
    rc = nf90_inq_varid(ncid, trim(vname),  datid)
    rc = nf90_put_var(ncid, datid, sfc2d, corner3, edge3)
    rc = nf90_close(ncid)
  endif

end subroutine write_sfccdf
