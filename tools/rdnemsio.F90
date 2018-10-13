program rdnemsio

  use param
  use caldata
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

  integer :: k,nr,ny, nm, nd, year, fhr, ll, lll
  integer :: mnend(nmon)
  integer :: nfhour
  integer :: ii,idxtotal

  integer, dimension(7) :: idate

  character(len= 6) :: cdate
  character(len= 4) :: cyear
  character(len= 2) :: cmon
  character(len= 4) :: cfhr

  character(len=500) :: sfcfile, sigfile

  !---------------------------------------------------------------------
  ! currently running everything from 2015-04-01 
  !year = 2015; nm = 3; nd = 31
  !year = 2015; nm = 4; nd = 1
  !call set_taxis(year,nm,nd,0,0,0)
  !tstamp0 = tstamp

  ! initial forecast in run
  !year = 2015; nm = monbeg(1); nd = 1
  !call set_taxis(year,nm,nd,0,0,0)
  ! fhr = int(tstamp - tstamp0) - 24
  ! fhr = int(tstamp - tstamp0) + 24
  ! fhr = int(tstamp - tstamp0) 
  !call i2c4(fhr,cfhr)

   !print *,'initial files to use sigf,sfcf ',trim(cfhr)
  !---------------------------------------------------------------------
  
   call GFS_externaldiag_populate(GFSsfc,idxtotal)

   print *,idxtotal
   do ii = 1,idxtotal
     print '(i6,a14,a5,a60)',ii,trim(GFSsfc(ii)%name),'     ',trim(GFSsfc(ii)%desc)
   enddo
  !---------------------------------------------------------------------

  call sigmafield_setup

  !---------------------------------------------------------------------
  ! find the grid and variables in the sfc file
  !---------------------------------------------------------------------

  !sfcfile = trim(rtsrc)//trim(rtname)//'atmout/sfcf'//trim(cfhr)
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

   print '(i6,a10,a20,a12,a20)',nr,'varname = ',trim(varnames(nr)),' long_name ',trim(varlong(nr))
   !print *,trim(varnames(nr)),' ',trim(varlong(nr)),minval(grd2d(:,:,nr)),maxval(grd2d(:,:,nr))
  enddo

  !---------------------------------------------------------------------
  ! find the grid and variables in the sigf file
  !---------------------------------------------------------------------

  !sigfile = trim(rtsrc)//trim(rtname)//'atmout/sigf'//trim(cfhr)
  sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
  call read_nemsio_header_sig(trim(sigfile),im,jm,nlevs,idate,nfhour)

  print '(a,3i6)','im,jm,nlevs = ',im,jm,nlevs
  !print '(a,a,7i6,a,i6)',trim(fname),' idate = ',idate,' nfhour ',nfhour

  allocate(vcoord(nlevs+1,3,2))

  call read_nemsio_coords(trim(sigfile),im,jm,nlevs,vcoord,lats,lons)
  do k = 1,nlevs+1
   print *,k,vcoord(k,1,1),vcoord(k,2,1),vcoord(k,3,1)
  enddo

  call alloc_sigma(im,jm,nlevs)
  ! save what looks to be first 22 sigma levels for output

  do k = 1,kout
   zout(k) = vcoord(k,2,1)
  enddo
#ifdef test
  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
#ifdef test
     ll = 0
    lll = 0
  do ny = 1,nyears
   year = yrbeg + (ny-1)
                        mnend = mnendn
  if(mod(year,4) .eq. 0)mnend = mnendl

   do nm = monbeg(ny),monend(ny)
    write(cyear,'(i4)')year
     if(nm .lt. 10)then
      write(cmon,'(a1,i1)')'0',nm
     else
      write(cmon,'(i2)')nm
     endif
     cdate = trim(cyear)//trim(cmon)
     print *,'working on ',trim(cdate)

        ll = 0
     do nd = 1, mnend(nm)
        ll =  ll + 1
       lll = lll + 1
       call set_taxis(year,nm,nd,0,0,0)
       fhr = int(tstamp - tstamp0)
       call i2c4(fhr,cfhr)
#endif
      lll = 1
      !sigfile = trim(rtsrc)//trim(rtname)//'atmout/sigf'//trim(cfhr)
      !print *,fhr,ll,year,nm,nd,'  sigf'//trim(cfhr),'  ',cdate
      sigfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.atmf000.nemsio'
      call sigma2nc(trim(sigfile),cdate,lll)

      !sfcfile = trim(rtsrc)//trim(rtname)//'atmout/sfcf'//trim(cfhr)
      sfcfile = trim(rtsrc)//trim(rtname)//'gdas.t18z.sfcf000.nemsio'
      call sfc2nc(trim(sfcfile),lll)
#ifdef test
     enddo !nd
    enddo !nm
   enddo !ny
#endif

#endif
  call dealloc_sigma
  deallocate(lons)
  deallocate(lats)
  deallocate(inames)
  deallocate(itypes)
  deallocate(grd2d)
  deallocate(a2d)
  deallocate(varnames)
  deallocate(varlong)

end program rdnemsio
