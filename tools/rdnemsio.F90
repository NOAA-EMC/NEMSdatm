program rdnemsio

  use param
  use GFS_diagnostics
  use nemsio_module
  use gfstonc_sfc
  use gfstonc_sig
  use sfcvars
  use sigmavars
  use charstrings
  use cdf

  implicit none

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: nr
  integer :: ii,k,idx,idxtotal

  integer :: nfhour
  integer, dimension(7) :: idate

  character(len= 6) :: fmtc4
  character(len= 6) :: fmtc2
  character(len=10) :: cyear, cmon, cday, chour, cfhour
  character(len=40) :: cstring

  character(len=500) :: sfcfile, sigfile

  integer, parameter :: maxfields = 200
  type(GFS_externaldiag_type) :: GFSsfc(maxfields)

  fmtc4 = '(i4.4)'
  fmtc2 = '(i2.2)'

  !---------------------------------------------------------------------
  
   call GFS_externaldiag_populate(GFSsfc,idxtotal)
   
   ! extend the diag type by 2 for the 2 fields required from the 3-d fields
    idx = idxtotal
    idx = idx + 1
    GFSsfc(idx)%name = 'pres_hyblev1'
    GFSsfc(idx)%desc = 'layer 1 pressure'
    GFSsfc(idx)%unit = 'Pa'
    GFSsfc(idx)%req4datm = .true.

    idx = idx + 1
    GFSsfc(idx)%name = 't850'
    GFSsfc(idx)%desc = 'temperature at 850mb'
    GFSsfc(idx)%unit = 'K'
    GFSsfc(idx)%req4datm = .true.

    idxtotal = idx
 
   do ii = 1,idxtotal
     if(GFSsfc(ii)%req4datm)print '(i6,a14,a5,a60)',ii,&
     !                        print '(i6,a14,a5,a60)',ii,&
                   trim(GFSsfc(ii)%name),'     ',trim(GFSsfc(ii)%desc)
   enddo

  !---------------------------------------------------------------------

  call sigmafield_setup

  do ii = 1,nsigvars
   print *,ii,trim(sgfields(ii)%varname)
  enddo

  !---------------------------------------------------------------------
  ! find the grid and variables in the sfc file
  !---------------------------------------------------------------------

  sfcfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.sfcf000.nemsio'
  call read_nemsio_header_sfc(trim(sfcfile),im,jm,nrecs,idate,nfhour)

  print '(a,3i6)','im,jm,nrecs = ',im,jm,nrecs
  print '(a,a,7i6,a,i6)',trim(sfcfile),' idate = ',idate,' nfhour ',nfhour

  allocate(lons(1:im))
  allocate(lats(1:jm))
  call read_nemsio_latlons(trim(sfcfile),im,jm,lats,lons)

  allocate(inames(1:nrecs,1:32))
  allocate(itypes(1:nrecs,1:32))
  allocate(grd2d(1:im,1:jm,1:nrecs))
  allocate(  a2d(1:im,1:jm))

  call read_nemsio_varnames(trim(sfcfile),nrecs,inames,itypes)

  call read_nemsio_2dgriddata(trim(sfcfile),im,jm,nrecs,inames,itypes,grd2d)

  allocate(varnames(1:nrecs))
  allocate( varlong(1:nrecs))
  do nr = 1,nrecs
   call arrtostr(inames(nr,:),varnames(nr),32)
   call arrtostr(itypes(nr,:), varlong(nr),32)

  ! print '(i6,a12,a20,a12,a20)',nr,' varname = ',trim(varnames(nr)),' long_name ',trim(varlong(nr))
  enddo

  !---------------------------------------------------------------------
  ! find the grid and variables in the sigf file
  !---------------------------------------------------------------------

  sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
  call read_nemsio_header_sig(trim(sigfile),im,jm,nlevs,idate,nfhour)

  print *,trim(sigfile)
  print '(a,3i6)','im,jm,nlevs = ',im,jm,nlevs
  !print '(a,a,7i6,a,i6)',trim(fname),' idate = ',idate,' nfhour ',nfhour

  allocate(vcoord(nlevs+1,3,2))

  call read_nemsio_coords(trim(sigfile),im,jm,nlevs,vcoord,lats,lons)
  !do k = 1,nlevs+1
  ! print *,k,vcoord(k,1,1),vcoord(k,2,1),vcoord(k,3,1)
  !enddo
#ifdef debug
  do k = 1,kout
   zout(k) = vcoord(k,2,1)
  enddo
#endif

  call alloc_sigma(im,jm,nlevs)

  !---------------------------------------------------------------------
  ! create a timestamp
  ! set up the output netCDF file
  !---------------------------------------------------------------------

  call set_taxis(idate(1),idate(2),idate(3),idate(4),idate(5),idate(6))

  write( cyear,fmtc4)idate(1) 
  write(  cmon,fmtc2)idate(2) 
  write(  cday,fmtc2)idate(3) 
  write( chour,fmtc2)idate(4) 
  write(cfhour,fmtc2)nfhour 
  cstring = trim(cyear)//trim(cmon)//trim(cday)//trim(chour)//'.f'//trim(cfhour)

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------

  sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
  call sigma2nc(trim(sigfile),trim(cstring))

  !sfcfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.sfcf000.nemsio'
  !call sfc2nc(trim(sfcfile),trim(cstring))

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------

  call dealloc_sigma
  deallocate(    lons)
  deallocate(    lats)
  deallocate(  inames)
  deallocate(  itypes)
  deallocate(   grd2d)
  deallocate(     a2d)
  deallocate(varnames)
  deallocate( varlong)

end program rdnemsio
