subroutine write_sigmacdf(cdffile, vname, vunit, vlong, v3d, sig2d, sig3d, idim, jdim, kdim, lstep)
#ifdef debug
  use kinds
  use charstrings
  use cdf
  use netcdf

  use sfcvars,   only : lats, lons
  use sigmavars, only : zout

  implicit none

           integer, intent(in) :: idim, jdim, kdim, lstep
  character(len=*), intent(in) :: cdffile
  character(len=*), intent(in) :: vname
  character(len=*), intent(in) :: vunit
  character(len=*), intent(in) :: vlong
           logical, intent(in) :: v3d

  real(kind=4), dimension(idim,jdim),      intent(in)   :: sig2d
  real(kind=4), dimension(idim,jdim,kdim), intent(in)   :: sig3d

  !-----------------------------------------------------------------------------

  if(lstep .eq. 1)then
   rc = nf90_create(trim(cdffile), nf90_clobber, ncid)
   !print *,trim(nf90_strerror(rc)),trim(cdffile)

   rc = nf90_def_dim(ncid,   'lon',          idim,     xtdim)
   rc = nf90_def_dim(ncid,   'lat',          jdim,     ytdim)
   rc = nf90_def_dim(ncid,    'Zt',          kdim,     ztdim)
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

   dim1(1) = ztdim
   rc = nf90_def_var(ncid,     'Zt', nf90_float,     dim1, ztid)
   rc = nf90_put_att(ncid, ztid,   'long_name',         'sigma')
   !rc = nf90_put_att(ncid, ztid,    'positive',          'down')

   dim1(1) =  tdim
   rc = nf90_def_var(ncid, 'time', nf90_double,  dim1,      timid)
   rc = nf90_put_att( ncid, timid,            'units', trim(torg))
   rc = nf90_put_att( ncid, timid,         'calendar', trim(tcal))
   rc = nf90_put_att( ncid, timid,             'axis',        'T')

  if(v3d)then
  ! 3-d
    dim4(4) =  tdim
    dim4(3) = ztdim
    dim4(2) = ytdim
    dim4(1) = xtdim
    rc = nf90_def_var(ncid, vname, nf90_float,  dim4,  datid)
    rc = nf90_put_att(ncid, datid,           'units',  vunit)
    rc = nf90_put_att(ncid, datid,       'long_name',  vlong)
  else
  ! 2-d
    dim3(3) =  tdim
    dim3(2) = ytdim
    dim3(1) = xtdim
    rc = nf90_def_var(ncid, vname, nf90_float,  dim3,  datid)
    rc = nf90_put_att(ncid, datid,           'units',  vunit)
    rc = nf90_put_att(ncid, datid,       'long_name',  vlong)
  endif
    rc = nf90_enddef(ncid)
    if(rc .ne. 0)print *,trim(nf90_strerror(rc))

   rc = nf90_put_var(ncid,  xtid,   lons)
   rc = nf90_put_var(ncid,  ytid,   lats)
   rc = nf90_put_var(ncid,  ztid,   zout)
   rc = nf90_close(ncid)
  endif

  !-----------------------------------------------------------------------------

   rc = nf90_open(trim(cdffile), nf90_write, ncid)

   corner1(1) = lstep
     edge1(1) = 1
   rc = nf90_inq_varid(ncid, 'time',  timid)
   rc = nf90_put_var(ncid, timid, tstamp, corner1)

   if(v3d)then
    corner4(4) = lstep
    corner4(3) = 1
    corner4(2) = 1
    corner4(1) = 1
      edge4(4) = 1
      edge4(3) = kdim
      edge4(2) = jdim
      edge4(1) = idim
    rc = nf90_inq_varid(ncid, trim(vname),  datid)
    rc = nf90_put_var(ncid, datid, sig3d, corner4, edge4)

   else
    corner3(3) = lstep
    corner3(2) = 1
    corner3(1) = 1
      edge3(3) = 1
      edge3(2) = jdim
      edge3(1) = idim
    rc = nf90_inq_varid(ncid, trim(vname),  datid)
    rc = nf90_put_var(ncid, datid, sig2d, corner3, edge3)
   endif
    rc = nf90_close(ncid)
#endif
end subroutine write_sigmacdf
