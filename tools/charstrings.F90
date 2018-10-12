module charstrings

  implicit none

   character(len= 80) :: rtsrc='/scratch3/NCEPDEV/stmp1/Denise.Worthen/'
   character(len=120) :: rtname='DATM/'

 contains
  subroutine i2c4(ihr,cfhr)

            integer, intent( in) :: ihr
   character(len=4), intent(out) :: cfhr

           if(ihr .eq. 0)then
             cfhr = '0000'
      elseif(ihr .lt.   10)then
       write(cfhr,'(a4,i1)')'000',ihr
      elseif(ihr .lt.  100)then
       write(cfhr,'(a2,i2)')'00',ihr
      elseif(ihr .lt. 1000)then
       write(cfhr,'(a1,i3)')'0',ihr
      else
       write(cfhr,'(i4)')ihr
      endif

  end subroutine i2c4
end module charstrings
