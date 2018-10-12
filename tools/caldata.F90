module caldata

  use param

  implicit none

!--------------------------------------------------------------------

  integer, parameter :: mnendn(nmon)=(/31, 28, 31, 30, 31, 30, &
                                       31, 31, 30, 31, 30, 31/)

  integer, parameter :: mnendl(nmon)=(/31, 29, 31, 30, 31, 30, &
                                       31, 31, 30, 31, 30, 31/)

  integer, parameter :: jbegn(nmon) = &
                                 (/   1,  32,  60,  91, 121, 152, &
                                    182, 213, 244, 274, 305, 335/)

  integer, parameter :: jendn(nmon) = &
                                (/   31,  59,  90, 120, 151, 181, &
                                    212, 243, 273, 304, 334, 365/)

  integer, parameter :: jbegl(nmon) = &
                                (/   1,  32,  61,  92, 122, 153, &
                                   183, 214, 245, 275, 306, 336/)

  integer, parameter :: jendl(nmon) = &
                                (/  31,  60,  91, 121, 152, 182, &
                                   213, 244, 274, 305, 335, 366/)

end module caldata
