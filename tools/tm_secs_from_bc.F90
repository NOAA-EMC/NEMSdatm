      real(kind=8) function tm_secs_from_bc ( year, month, day, &
                                          hour, minute, second)
! will convert a date of yyyy-mm-dd hh:mm:ss to seconds since
! 0000-01-01 00:00:00
!
! written 6/16/87 by mark verschell for pmel/tmap
!
! revision 0.00 - 06/16/87 - initial incarnation
!
! argument definition
      integer year, month, day, hour, minute,second
!
! local definition
      integer days_in_month(12)
      real(kind=8) ::   secs_in_minute, secs_in_hour, secs_in_day, &
                        secs_in_year, secs_in_century, total_secs

      parameter(secs_in_minute  = 60., &
                secs_in_hour    = secs_in_minute*60., &
                secs_in_day     = secs_in_hour*24., &
                secs_in_year    = secs_in_day*365., &
                secs_in_century = secs_in_day*36524.)

      data days_in_month /   0,  31,  59,  90, 120, 151, &
                           181, 212, 243, 273, 304, 334/

! add lots of seconds for each century since 0 day
      total_secs = secs_in_century * int(year/100)
! add a day for every 400 years (leap year at centennial)
      total_secs = total_secs + secs_in_day*int(year/400.+0.9975)
! add a year for each year since turn of century
      total_secs = total_secs + secs_in_year*mod(year,100)
! add a day for each leap year since centennial (except for centennial)
      total_secs = total_secs + secs_in_day*int((mod(year,100)-1)/4)
! add seconds for number of months
      total_secs = total_secs + days_in_month(month)*secs_in_day
! add 1 day worth of seconds if this is leap year and past february
      if (month .gt. 2) then
       if (mod(year,400) .eq. 0) then
        total_secs = total_secs + secs_in_day
       else
        if (mod(year,4) .eq. 0 .and. mod(year,100) .ne. 0) &
                total_secs = total_secs + secs_in_day
       endif
      endif
! add seconds for number of days
      total_secs = total_secs + secs_in_day*(day-1)
! add seconds for number of hours
      total_secs = total_secs + secs_in_hour*(hour)
! add seconds for number of minutes
      total_secs = total_secs + secs_in_minute*(minute)
! add seconds for number of secs for total number of seconds

! finished
      tm_secs_from_bc = total_secs
      return
      end
