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
  use kinds

  implicit none

!---------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

  integer :: i,j,k,nr
  integer :: ii,kk,idx

  integer :: nfhour
  integer, dimension(7) :: idate

  character(len=  6) :: fmtc4
  character(len=  6) :: fmtc2
  character(len= 10) :: cyear, cmon, cday, chour, cfhour
  character(len= 40) :: cstring
  character(len=500) :: sfcfile, sigfile, outcdf
!gfsphysics/physics/physcons.f90
!> gravity (\f$m/s^{2}\f$)
  real(kind=4),parameter:: con_g      =9.80665e+0
!> molar gas constant (\f$J/mol/K\f$)
  real(kind=4),parameter:: con_rgas   =8.314472
!> gas constant air (\f$J/kg/K\f$)
  real(kind=4),parameter:: con_rd     =2.8705e+2
!> gas constant H2O (\f$J/kg/K\f$)
  real(kind=4),parameter:: con_rv     =4.6150e+2
!> \name Secondary constants
  real(kind=4),parameter:: con_fvirt  =con_rv/con_rd-1.

  real(kind=r_kind), allocatable, dimension(:,:) :: t850, plowest, rain, snow

  fmtc4 = '(i4.4)'
  fmtc2 = '(i2.2)'

  !---------------------------------------------------------------------
  
   call GFS_externaldiag_populate(GFSsfc,idxtotal)
  
   ! extend the diag type for req fields created on fly 
    idx = idxtotal
    idx = idx + 1
    GFSsfc(idx)%name = 'pres_hyblev1'
    GFSsfc(idx)%desc = 'layer 1 pressure'
    GFSsfc(idx)%unit = 'Pa'
    GFSsfc(idx)%req4datm = .true.

    idx = idx + 1
    GFSsfc(idx)%name = 'precp'
    GFSsfc(idx)%desc = 'mean_prec_rate'
    GFSsfc(idx)%unit = 'kg s m-2'
    GFSsfc(idx)%req4datm = .true.

    idx = idx + 1
    GFSsfc(idx)%name = 'fprecp'
    GFSsfc(idx)%desc = 'mean_fprec_rate'
    GFSsfc(idx)%unit = 'kg s m-2'
    GFSsfc(idx)%req4datm = .true.

    idxtotal = idx

    allocate(index(1:idxtotal))

   do ii = 1,idxtotal
     if(GFSsfc(ii)%req4datm)print '(i6,a14,a5,a40)',ii,&
     !                        print '(i6,a14,a5,a40)',ii,&
                   trim(GFSsfc(ii)%name),'     ',trim(GFSsfc(ii)%desc)
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
  allocate(  a2d(1:im,1:jm,1:1))
  !
  allocate(    t850(1:im,1:jm))
  allocate( plowest(1:im,1:jm))
  allocate(    snow(1:im,1:jm))
  allocate(    rain(1:im,1:jm))

  call read_nemsio_varnames(trim(sfcfile),nrecs,inames,itypes)

  allocate(varnames(1:nrecs))
  allocate( varlong(1:nrecs))
  do nr = 1,nrecs
   call arrtostr(inames(nr,:),varnames(nr),32)
   call arrtostr(itypes(nr,:), varlong(nr),32)
   !print '(i6,a12,a20,a12,a20)',nr,' varname = ',trim(varnames(nr)),' long_name ',trim(varlong(nr))
  enddo
  !---------------------------------------------------------------------
  ! match the required fields with the fields from the sfc file
  !---------------------------------------------------------------------

  call fieldmatch
  
  !---------------------------------------------------------------------
  ! find the grid and variables in the sigf file
  !---------------------------------------------------------------------

  sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
  call read_nemsio_header_sig(trim(sigfile),im,jm,nlevs,idate,nfhour)

  print *,trim(sigfile)
  print '(a,3i6)','im,jm,nlevs = ',im,jm,nlevs
  print '(a,a,7i6,a,i6)',trim(sigfile),' idate = ',idate,' nfhour ',nfhour

  allocate(vcoord(nlevs+1,3,2))

  call read_nemsio_coords(trim(sigfile),im,jm,nlevs,vcoord,lats,lons)
  !do kk = 1,nlevs+1
  ! print *,kk,vcoord(kk,1,1),vcoord(kk,2,1),vcoord(kk,3,1)
  !enddo
#ifdef debug
  do kk = 1,kout
   zout(kk) = vcoord(kk,2,1)
  enddo

  call sigmafield_setup

  do ii = 1,nsigfields
  ! print *,ii,trim(sgfields(ii)%varname)
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

  outcdf = trim(rtsrc)//trim(rtname)//'gdas.'//trim(cstring)//'.nc'
  !print *,trim(outcdf)

  call setup_outcdf(trim(outcdf))

  !---------------------------------------------------------------------
  ! 
  !---------------------------------------------------------------------

  sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
  call read_nemsio_griddata(trim(sigfile), im, jm, nlevs, ug, vg, &
                            tempg, zsg, psg, qg, ozg, cwmrg, dpresg, presg, delzg)

  sfcfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.sfcf000.nemsio'
  call read_nemsio_2dgriddata(trim(sfcfile),im,jm,nrecs,inames,itypes,grd2d)

  !---------------------------------------------------------------------
  ! create additional fields on the fly
  !---------------------------------------------------------------------

  call find_t850(t850)
#ifdef debug
  call sigma2nc(trim(cstring))
#endif

  rain = 0.0; snow = 0.0
!gfsphysics/GFS_layer/GFS_physics_driver.F90
  do j = 1,jm
   do i = 1,im
    if (t850(i,j) > 273.16) then
      rain(i,j) = grd2d(i,j,nr_tprcp) 
    else
      snow(i,j) = grd2d(i,j,nr_tprcp)
    endif
   enddo
  enddo
!  from Jun Wang
!P(modellevel1)=delp(modellevel1) * R * T(modellevel1) *(1.+q(modellevel1)*fv)/g/delz(modellevel1)
!fv=con_rv/con_rd-1
  do j = 1,jm
   do i = 1,im
    plowest(i,j) = dpresg(i,j,1) * con_rgas * tempg(i,j,1) *(1.+qg(i,j,1)*con_fvirt)/con_g/delzg(i,j,1)
   enddo
  enddo

  !---------------------------------------------------------------------
  ! write to the output cdf
  !---------------------------------------------------------------------

     rc = nf90_open(trim(outcdf), nf90_write, ncid)
  do ii = 1,idxtotal
   if(GFSsfc(ii)%req4datm)then
             nr = index(ii)
    if(nr .eq. -1)then
     if(trim(GFSsfc(ii)%name) .eq. 'pres_hyblev1')a2d(:,:,1) = plowest(:,:)
     if(trim(GFSsfc(ii)%name) .eq.        'precp')a2d(:,:,1) =    rain(:,:)
     if(trim(GFSsfc(ii)%name) .eq.       'fprecp')a2d(:,:,1) =    snow(:,:)
    else
     a2d(:,:,1) = grd2d(:,:,nr)  
    endif
     print *,trim(GFSsfc(ii)%name),nr,a2d(im/2,jm/2,1)

     rc = nf90_inq_varid(ncid, trim(GFSsfc(ii)%name), datid)
     rc = nf90_put_var(ncid,                   datid,   a2d)
    endif
   enddo
    rc = nf90_close(ncid)

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

  deallocate(   index)
  deallocate(    t850)
  deallocate( plowest)
  deallocate(    rain)
  deallocate(    snow)
end program rdnemsio
