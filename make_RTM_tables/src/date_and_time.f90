
    module date_and_time

        use NAN_support

        implicit none

        type time_str
            real(8)        :: m_secs        ! seconds since midnight
            integer(4)    :: hour            ! hour of day
            integer(4)    :: minute        ! minute of hour
            real(8)        :: sec            ! second of minute
            logical        :: updated        ! true if m_secs agree with hours,minute and second
        end type time_str

        type date_str
            real(8)            :: r_d        ! days since Jan 1, 1, gregorian calender
            real(8)            :: rd_2000  ! days since Jan 1, 2000
            integer(4)        :: year        ! year
            integer(4)        :: oday        ! ordinal day in the given year
            integer(4)        :: month    ! month in year
            integer(4)        :: day        ! day in month
            type(time_str)    :: time        ! time of day
            logical            :: updated  ! true if Jul_day agrees with other info
        end type date_str

        integer(4),parameter    :: GREG_EPOCH = 1

        integer(4),parameter    :: SEASON_UNKNOWN = 0
        integer(4),parameter    :: WINTER = 1
        integer(4),parameter    :: SPRING = 2
        integer(4),parameter    :: SUMMER = 3
        integer(4),parameter    :: FALL   = 4

        real(8),parameter        :: RD_1_1_2000 = 730120.0D0
        real(8),parameter        :: RD_1_1_1978 = 722085.0D0




        character(Len = 6),dimension(4),parameter    :: SEASON_NAMES = (/'winter','spring','summer','autumn'/)

        real(4),dimension(0:13,2),parameter :: month_centers = RESHAPE((/-15.5,15.5,45.0,74.5,105.0,135.5,166.0,196.5,227.5,258.0,288.5,319.0,349.5,380.5, &
                                                                         -15.5,15.5,45.5,75.5,106.0,136.5,167.0,197.5,228.5,259.0,289.5,320.0,350.5,381.5/),(/14,2/))
        real(4),dimension(0:13,2),parameter :: month_center_day = RESHAPE((/16.0,16.0,14.5,16.0,15.5,16.0,15.5,16.0,16.0,15.5,16.0,15.5,16.0,16.0, &
                                                                            16.0,16.0,15.0,16.0,15.5,16.0,15.5,16.0,16.0,15.5,16.0,15.5,16.0,16.0/),(/14,2/))
        interface set_date
            module procedure set_date_int_r_d        !(r_d,error)   2 integer(4)'s
            module procedure set_date_real_r_d        !(r_d,error)   1 real(8) and 1 integer(4)
            module procedure set_date_m_d_y            !(month,day,year,error) 4 integer(4)'s
            module procedure set_date_m_d_y_int_t    !(month,day,year,hour,minute,second,error) 7 integer(4)'s
            module procedure set_date_m_d_y_real_t    !(month,day,year,hour,minute,second,error) 5 integer(4)'s, 1 real(8), 1 integer(4)
            module procedure set_date_m_d_y_m_secs    !(month,day,year,secs_since_midnight,error) 3 integer(4)'s, 1 real(8), 1 integer(4)
            module procedure set_date_od_y_msecs    !(secs_since_midnight,ordinal_day#,year,error) 1 real(8), 3 integer(4)'s          !
        end interface

        interface set_date_2000
            module procedure set_date_2000_real4
            module procedure set_date_2000_real8
            module procedure set_date_2000_int4        
            module procedure set_date_2000_int2
        end interface

        interface set_time
            module procedure set_time_msecs            ! set time using msecs
            module procedure set_time_hms            ! set time using hours, minutes, real seconds
        end interface   

        type(time_str),parameter        :: MIDNIGHT = time_str(0.0d0,0,0,0.0D0,.true.)
        type(time_str),parameter        :: NOON     = time_str(43200.0D0,12,0,0.0D0,.true.)
        type(date_str),parameter        :: Jan_1_2000 = date_str(730120.0D0,0.0D0          ,2000,1,1,1,MIDNIGHT,.true.)
        type(date_str),parameter        :: Jan_1_0000 = date_str(0.0D0,    -730120.0D0     ,0000,1,1,1,MIDNIGHT,.true.)

    contains
        
        subroutine update_hms(time,error)

            ! this routine propogates a change in m_secs to the hour, min and second part
            ! of the time structure

            use NAN_support

            implicit none

            type(time_str),intent(INOUT)    :: time
            integer(4),intent(OUT)            :: error    ! = 0 if OK,-1 if error
                                                

            real(8)                            :: secs

            ! begin execution

            if ((time%m_secs >= 0.0) .and. (time%m_secs < 86400.0)) then   ! everything ok
                secs = time%m_secs
                time%hour = int(secs)/3600
                secs = secs - time%hour*3600
                time%minute = int(secs)/60
                time%sec = secs - time%minute*60
                time%updated = .TRUE.
                error = 0
            else
                time%hour = -1
                time%minute = -1
                time%sec  = NAN
                time%updated = .FALSE.
                error = -1
            endif

        end subroutine update_hms

        subroutine update_m_secs(time,error)

            ! this routine propogates a cahnge in hours, minutes or secs to the m_secs
            ! part of the time structure

            use NAN_support

            implicit none

            type (time_str),intent(INOUT)        :: time
            integer(4),intent(OUT)            :: error

            if ((time%hour >= 0)   .and. (time%hour <= 23)   .and. &
                (time%minute >= 0) .and. (time%minute <= 59) .and. & 
                (time%sec >= 0.0)  .and. (time%sec < 60.0))   then
                    
                    time%m_secs = time%sec + (time%minute * 60.0) + (time%hour* 3600.0)
                    
                    time%updated = .true.
                    error = 0

            else
                time%m_secs = NAN

                time%updated = .false.
                error = -1
            endif

        end subroutine update_m_secs



        logical function gregorian_leap_year(year)

            ! this function returns .true. if year is a gregorian
            ! leap year

            implicit none

            integer(4),intent(IN)        :: year

            gregorian_leap_year = ((modulo(year,4) == 0) .and. &
                                  .not.((modulo(year,400) .eq. 100) .or.  &
                                        (modulo(year,400) .eq. 200) .or. &
                                        (modulo(year,400) .eq. 300)))
            return

        end function gregorian_leap_year
        


        integer(4) function fixed_from_gregorian(month,day,year)

            ! this routine calculates the fixed R.D. day from the
            ! gregorian month, day, and year

            implicit none

            integer(4),intent(IN)        :: month
            integer(4),intent(IN)        :: day
            integer(4),intent(IN)        :: year

            integer(4)                    :: leap

            if (month <= 2) then 
                leap = 0
            else 
                if (gregorian_leap_year(year)) then
                    leap = -1
                else
                    leap = -2
                endif
            endif

            fixed_from_gregorian = GREG_EPOCH-1+ 365*(year-1) + floor(real(year-1)/4.0) - &
                                    floor(real(year-1)/100.0) + floor(real(year-1)/400.0) + &
                                    floor(real(367*month -362)/12.0) + leap + day

        end function fixed_from_gregorian



        integer(4) function gregorian_year_from_fixed(r_d)

            ! this routine calculates the gregorian year from the R.D. day

            integer(4),intent(IN)            :: r_d

            integer(4)                        :: temp_yr

            integer(4)                        :: d0
            integer(4)                        :: d1
            integer(4)                        :: d2
            integer(4)                        :: d3
            integer(4)                        :: d4

            integer(4)                        :: n400
            integer(4)                        :: n100
            integer(4)                        :: n4
            integer(4)                        :: n1

            ! begin execution

            d0 = r_d - GREG_EPOCH

            n400 = floor(real(d0)/real(146097))

            d1 = modulo(d0,146097)

            n100 = floor(real(d1)/real(36524))

            d2 = modulo(d1,36524)

            n4 = floor(real(d2)/real(1461))

            d3 = modulo(d2,1461)

            n1 = floor(real(d3)/real(365))

            d4 = modulo(d3,365) + 1

            temp_yr = 400 * n400 + 100*n100 + 4*n4 + n1

            if ((n100 == 4) .or. (n1 == 4)) then
                gregorian_year_from_fixed = temp_yr
            else
                gregorian_year_from_fixed = temp_yr + 1
            endif

        end function gregorian_year_from_fixed


        subroutine gregorian_from_fixed(r_d,month,day,year)

            ! this routine calculates the gregorian month, day, and year from thefixed R.D. day

            implicit none

            integer(4),intent(IN)        :: r_d
            integer(4),intent(OUT)        :: month
            integer(4),intent(OUT)        :: day
            integer(4),intent(OUT)        :: year


            integer(4)                    :: prior_days
            integer(4)                    :: correction

            ! begin execution

            year = gregorian_year_from_fixed(r_d)

            prior_days = r_d - fixed_from_gregorian(1,1,year)

            if (r_d .lt. fixed_from_gregorian(3,1,year)) then
                correction = 0
            else 
                if (gregorian_leap_year(year)) then
                    correction = 1
                else
                    correction = 2
                endif
            endif

            month = floor((12.0*(prior_days + correction) + 373.0)/367.0)

            day = r_d - fixed_from_gregorian(month,1,year) + 1

        end subroutine gregorian_from_fixed


        subroutine update_ymdt(date,error)

            !    This routine updates the year month day and time for a change
            !    in r_d

            implicit none

            type(date_str),intent(INOUT)        :: date
            integer(4),intent(OUT)                :: error

            integer(4)                            :: year
            integer(4)                            :: month
            integer(4)                            :: day

            ! first, update the date

            call gregorian_from_fixed(floor(date%r_d),month,day,year)

            date%year  = year
            date%month = month
            date%day   = day
            date%oday  = fixed_from_gregorian(month,day,year) -  &
                         fixed_from_gregorian(12,31,year-1)

            ! now, update the time

            date%time%m_secs = 86400.0 * (date%r_d - floor(date%r_d))

            call update_hms(date%time,error) 

            if (error == 0) then
                date%updated = .true.
                error = 0
            else
                date%updated = .false.
                error = -1
            endif

            date%rd_2000 = date%r_d - RD_1_1_2000

        end subroutine update_ymdt

        subroutine update_r_d(date,error)

            ! updates fixed date  and oday given a change in year, month, day, h, m, s
            ! !!currently, cannot force a change by changing date%oday!!

            implicit none

            type(date_str),intent(INOUT)        :: date
            integer(4),intent(OUT)                :: error

            
            ! first, update the time since midnight to reflect a possible
            ! change in hours, minutes, or seconds

            call update_m_secs(date%time,error)

            if (error == 0) then

                date%r_d = date%time%m_secs/86400.0 +  &
                           fixed_from_gregorian(date%month,date%day,date%year)

                date%rd_2000 = date%r_d - RD_1_1_2000

                date%oday = fixed_from_gregorian(date%month,date%day,date%year) - &
                            fixed_from_gregorian(12,31,date%year-1)

                error = 0

                date%updated = .true.

            else  ! OK, time is screwed up, but we can still calculate a day assuming time == midnight
                  ! can detect that this happened because error = 1, and date%time%updated = .false.

                date%r_d =  0.0 +  &
                            fixed_from_gregorian(date%month,date%day,date%year)

                date%rd_2000 = date%r_d - RD_1_1_2000


                date%oday = fixed_from_gregorian(date%month,date%day,date%year) - &
                            fixed_from_gregorian(12,31,date%year-1)

                error = 1

                date%updated = .true.

            endif

        end subroutine update_r_d

        ! These are all date initializers called using set_date with various parameters

        type(date_str)  function init_date()

            implicit none

            type(date_str)                :: temp_date

            integer(4)                    :: error


            temp_date%r_d = dble(0.0)

            call update_ymdt(temp_date,error)

            init_date =    temp_date

        end function init_date

        

        type(date_str) function set_date_int_r_d(r_d,error)

            implicit none
            integer(4),intent(IN)        :: r_d
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%r_d = dble(r_d)

            call update_ymdt(temp_date,error)

            set_date_int_r_d =    temp_date

        end function set_date_int_r_d


        type(date_str) function set_date_real_r_d(r_d,error)
            implicit none

            real(8),intent(IN)            :: r_d
             integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%r_d = r_d

            call update_ymdt(temp_date,error)

            set_date_real_r_d =    temp_date

        end function set_date_real_r_d


        type(date_str) function set_date_m_d_y(month,day,year,error)
            implicit none
            integer(4),intent(IN)        :: month
            integer(4),intent(IN)        :: day
            integer(4),intent(IN)        :: year
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%month            = month
            temp_date%day            = day
            temp_date%year            = year
            temp_date%time%hour        = 0
            temp_date%time%minute    = 0
            temp_date%time%sec        = 0.0

            call update_r_d(temp_date,error)

            set_date_m_d_y = temp_date

        end function set_date_m_d_y


        type(date_str) function set_date_m_d_y_int_t(month,day,year,hour,minute,sec,error)
            implicit none
            integer(4),intent(IN)        :: month
            integer(4),intent(IN)        :: day
            integer(4),intent(IN)        :: year
            integer(4),intent(IN)        :: hour
            integer(4),intent(IN)        :: minute
            integer(4),intent(IN)        :: sec
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%month            = month
            temp_date%day            = day
            temp_date%year            = year
            temp_date%time%hour        = hour
            temp_date%time%minute    = minute
            temp_date%time%sec        = real(sec)

            call update_r_d(temp_date,error)
            set_date_m_d_y_int_t = temp_date

        end function set_date_m_d_y_int_t

            type(date_str) function set_date_m_d_y_real_t(month,day,year,hour,minute,sec,error)
            implicit none
            integer(4),intent(IN)        :: month
            integer(4),intent(IN)        :: day
            integer(4),intent(IN)        :: year
            integer(4),intent(IN)        :: hour
            integer(4),intent(IN)        :: minute
            real(8),intent(IN)            :: sec
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%month            = month
            temp_date%day            = day
            temp_date%year            = year
            temp_date%time%hour        = hour
            temp_date%time%minute    = minute
            temp_date%time%sec        = sec

            call update_r_d(temp_date,error)
            set_date_m_d_y_real_t = temp_date

        end function set_date_m_d_y_real_t

        type(date_str) function set_date_m_d_y_m_secs(month,day,year,m_secs,error)

            implicit none

            integer(4),intent(IN)        :: month
            integer(4),intent(IN)        :: day
            integer(4),intent(IN)        :: year
            real(8),intent(IN)            :: m_secs
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date


            temp_date%time%m_secs = m_secs

            !    must update hms first, since update_r_d assumes change in h,m,s

            call update_hms(temp_date%time,error)

            ! now do the date part

            temp_date%month = month
            temp_date%day   = day
            temp_date%year  = year

            call update_r_d(temp_date,error)

            set_date_m_d_y_m_secs = temp_date

        end function set_date_m_d_y_m_secs

        type(date_str) function set_date_od_y_msecs(m_secs,oday,year,error)

            implicit none
            real(8),intent(IN)            :: m_secs
            integer(4),intent(IN)        :: oday
            integer(4),intent(IN)        :: year
            integer(4),intent(OUT)        :: error

            type(date_str)                :: temp_date

            temp_date%r_d = fixed_from_gregorian(12,31,year-1) + oday + m_secs/86400.0

            call update_ymdt(temp_date,error)

            set_date_od_y_msecs = temp_date

        end function set_date_od_y_msecs





        subroutine print_date(date,lu)

            type(date_str),intent(IN)        :: date
            integer(4),intent(IN)            :: lu

            if (date%year .ge. 0) then
                write(lu,10) date%r_d,date%month,date%day,date%year,date%oday
    10            format(1x,f14.6,2x,i2.2,'/',i2.2,'/',i4.4,4x,'Day# = ',i3)
            else
                write(lu,20) date%r_d,date%month,date%day,-date%year,date%oday
    20            format(1x,f14.6,2x,i2.2,'/',i2.2,'/',i4.4,' B.C.G.',4x,'Day# = ',i3)
            endif
            call print_time(date%time,lu)

        end subroutine print_date

        character(len=20) function date_string(date)

            type(date_str)        :: date
            character(len = 12) :: string
            if (date%year .ge. 0) then
                write(string,10) date%month,date%day,date%year
    10            format(i2.2,'/',i2.2,'/',i4.4)
            else
                write(string,20) date%r_d,date%month,date%day,-date%year,date%oday
    20            format(i2.2,'/',i2.2,'/',i4.4,'BC')
            endif  
            date_string = string // time_string(date%time)

        end function date_string

        character(len=19) function date_string_for_file(date)

            type(date_str)        :: date
            character(len = 10) :: string
            if (date%year .ge. 0) then
                write(string,10) date%year,date%month,date%day ! order is changed around so files sort in date order when sorted by name
    10            format(i4.4,'_',i2.2,'_',i2.2)
            else
                write(string,10) -date%year,date%month,date%day
    20            format(i4.4,'BC_',i2.2,'_',i2.2)
            endif  
            date_string_for_file = string //'_'//time_string_for_file(date%time)

        end function date_string_for_file


        character(len=8) function time_string(time)
            type(time_str)        :: time
            character(len = 8)  :: string

            write(string,10) time%hour,time%minute,floor(time%sec)
    10        format(i2.2,':',i2.2,':',i2.2)

            time_string  = string

            return

        end function time_string

        character(len=8) function time_string_for_file(time)
            type(time_str)        :: time
            character(len = 8)  :: string

            write(string,10) time%hour,time%minute,floor(time%sec)
    10        format(i2.2,'_',i2.2,'_',i2.2)

            time_string_for_file  = string

            return

        end function time_string_for_file

        ! character(len = 20) function curr_date_string(err)    

        !     type(date_str)    :: cur_date
        !     integer(4)        :: err

        !     cur_date = current_date(err)
        !     curr_date_string = date_string_for_file(cur_date)

        !     return

        ! end function curr_date_string
        
        subroutine print_time(time,lu)
        
            type(time_str),intent(IN)        :: time
            integer(4),intent(IN)            :: lu
            
            write(lu,10) time%m_secs,time%hour,time%minute,floor(time%sec)
    10        format(1x,f14.6,2x,i2.2,':',i2.2,':',i2.2)
        end subroutine print_time
        
        integer(4) function day2000(year,jday)

            implicit none
        
            integer(4),intent(in)        :: year
            integer(4),intent(in)        :: jday  

            type(date_str)                :: now
            integer(4)                    :: error

            now = set_date(0.0D0,jday,year,error)

            day2000 = nint(now%r_d - Jan_1_2000%r_d)

        end function day2000

        real(8) function rd_2000(date)

            implicit none

            type(date_str),intent(in)    :: date

            rd_2000 = date%r_d - Jan_1_2000%r_d

        end function rd_2000


        type(date_str) function set_date_2000_real4(r_d_2000,error)
            implicit none

            real(4),intent(IN)            :: r_d_2000
             integer(4),intent(OUT)        :: error

            set_date_2000_real4 = set_date(Jan_1_2000%r_d + r_d_2000,error)

        end function set_date_2000_real4


        type(date_str) function set_date_2000_real8(r_d_2000,error)
            implicit none

            real(8),intent(IN)            :: r_d_2000
             integer(4),intent(OUT)        :: error

            set_date_2000_real8 = set_date(Jan_1_2000%r_d + r_d_2000,error)

        end function set_date_2000_real8


        type(date_str) function set_date_2000_int4(r_d_2000,error)
            implicit none

            integer(4),intent(IN)        :: r_d_2000
             integer(4),intent(OUT)        :: error


            set_date_2000_int4 = set_date(Jan_1_2000%r_d + r_d_2000,error)

        end function set_date_2000_int4

        type(date_str) function set_date_2000_int2(r_d_2000,error)
            implicit none

            integer(2),intent(IN)        :: r_d_2000
             integer(4),intent(OUT)        :: error

            set_date_2000_int2 = set_date(Jan_1_2000%r_d + r_d_2000,error)

        end function set_date_2000_int2

        type(time_str) function set_time_msecs(msecs,error)

            implicit none

            real(8),intent(IN)            :: msecs
            integer(4),intent(OUT)        :: error

            type(time_str)                :: temp_time

            temp_time%m_secs = msecs
            call update_hms(temp_time,error)
            set_time_msecs = temp_time
            return

        end function set_time_msecs

        type(time_str) function set_time_hms(hours,minutes,secs,error)

            implicit none

            integer(4)                    :: hours
            integer(4)                    :: minutes
            real(8),intent(IN)            :: secs
            integer(4),intent(OUT)        :: error

            type(time_str)                :: temp_time

            temp_time%hour = hours
            temp_time%minute = minutes
            temp_time%sec    = secs
            call update_m_secs(temp_time,error)
            set_time_hms = temp_time
            return

        end function set_time_hms

        type(time_str) function add_times(time1,time2,error)

            implicit none

            type(time_str),intent(IN)    :: time1
            type(time_str),intent(IN)    :: time2
            integer(4),intent(OUT)        :: error

            type(time_str)                :: temp_time


            if (finite(time1%m_secs) .and. finite(time2%m_secs)) then
                temp_time%m_secs = modulo((time1%m_secs + time2%m_secs),86400.0D0)
                call update_hms(temp_time,error)
            else
                error = -1
                temp_time%m_secs = nan
            endif

            add_times = temp_time

            return

        end function add_times




        integer(4) function season(date,error)
        
            ! this function returns the season for a given date
            ! WINTER = dec, jan,feb
            ! SPRING = mar,apr,may
            ! SUMMER = june,july,august
            ! FALL = sept,oct,nov

            type(date_str),intent(IN)        :: date
            integer(4),intent(OUT)            :: error

            error = 0

            select case (date%month)
                case (1)
                    season = WINTER
                case (2)
                    season = WINTER
                case (3)
                    season = SPRING
                case (4)
                    season = SPRING
                case (5)
                    season = SPRING
                case (6)
                    season = SUMMER
                case (7)
                    season = SUMMER
                case (8)
                    season = SUMMER
                case (9)
                    season = FALL
                case (10)
                    season = FALL
                case (11)
                    season = FALL
                case (12)
                    season = WINTER
                case default
                    season = SEASON_UNKNOWN
                    error = 1
            end select
            return

        end function season

        
        subroutine find_month(jday,year,month,day_mon)

        ! This subroutine converts from julian day to month and day of month

            integer(2),intent(IN)            :: jday
            integer(2),intent(IN)            :: year       
            integer(4),intent(OUT)            :: month      ! number of month returned
            integer(4),intent(OUT)            :: day_mon      ! day of month


            type(date_str)                :: temp
            integer(4)                    :: error 
            integer(4)                    :: jday4
            integer(4)                    :: year4

    !        begin execution

            jday4 = jday
            year4 = year

            temp = set_date(0.0D0,jday4,year4,error)

            month    = temp%month
            day_mon = temp%day
        
        end subroutine find_month


        ! type(date_str) function current_date(error)

        !     use msflib

        !     implicit none

        !     integer(4),intent(INOUT)        :: error

        !     integer(2)                        :: iyr
        !     integer(2)                        :: imon
        !     integer(2)                        :: iday
        !     integer(2)                        :: ihr
        !     integer(2)                        :: imin
        !     integer(2)                        :: isec
        !     integer(2)                        :: i100th

        !     integer(4)                        :: jyr
        !     integer(4)                        :: jmon
        !     integer(4)                        :: jday
        !     integer(4)                        :: jhr
        !     integer(4)                        :: jmin
        !     real(8)                            :: sec

        !     call getdat(iyr, imon, iday)
        !     call gettim(ihr,imin,isec,i100th)

        !     jyr = iyr
        !     jmon = imon
        !     jday = iday
        !     jhr = ihr
        !     jmin = imin
        !     sec = isec + 0.01D0*i100th
        !     current_date = set_date(jmon,jday,jyr,jhr,jmin,sec,error)

        !     return

        ! end function current_date

        subroutine month_weights(date,month_wts,month_indices,error)
            
            type(date_str)            :: date
            real(4),dimension(12)   :: month_wts
            integer(4),dimension(2) :: month_indices
            integer(4)                :: error

            real(4),dimension(12)   :: wts
            integer(4)                :: leap
            real(8)                    :: oday_float

            integer(4)                :: month,month1,month2
            real(4)                    :: wt_month1,wt_month2

            real(4)                    :: delta_days_from_Start
            real(4)                    :: days_in_span
            
            leap = 1

            if (gregorian_leap_year(date%year)) leap = 2

            oday_float = date%oday + (date%time%m_secs/86400.0D0)-1.0
            if (finite(oday_float)) then
                do month = 0,13
                    if ((oday_float .ge. month_centers(month,leap)) .and.  &
                        (oday_float .lt. month_centers(month+1,leap)))   then

                        ! correct time period found

                        month1 = month
                        if (month1 .eq. 0)  month1 = 12
                        if (month1 .eq. 13) month1 = 1

                        month2 = month+1
                        if (month2 .eq. 0) month2 = 12
                        if (month2 .eq. 13) month2 = 1

                        
                        days_in_span = month_centers(month+1,leap) - month_centers(month,leap)

                        delta_days_from_start = oday_float - month_centers(month,leap)

                        wt_month1 = (days_in_span - delta_days_from_Start)/(days_in_span)
                        wt_month2 = 1.0 - wt_month1

                        wts = 0.0
                        wts(month1) = wt_month1
                        wts(month2) = wt_month2
                        error = 0
                        month_wts = wts
                        month_indices(1) = month1
                        month_indices(2) = month2
                        return
                    endif
                enddo
            endif

            wts = NAN
            month_wts = wts
            month_indices = 0
            error = -1
            return
        end subroutine month_weights

           subroutine hour_weights(time,hour_wts,hour_indices,error)

            save
            
            type(time_str)            :: time
            real(4),dimension(24)   :: hour_wts
            integer(4),dimension(2) :: hour_indices
            integer(4)                :: error

            real(4),dimension(24)   :: wts
            real(4)                    :: hour_float
            real(4)                    :: hour_rem

            integer(4)                :: hour,hour1,hour2
            real(4)                    :: wt_hour1,wt_hour2


            hour_float = time%m_secs/3600.0
            hour_rem   = modulo(hour_float-0.5,1.0)

            do hour = 0,25
                if (hour_float .le. (hour+0.5))   then

                    ! correct time period found

                    hour1 = hour
                    if (hour1 .eq. 0)  hour1 = 24

                    hour2 = hour+1
                    if (hour2 .eq. 25) hour2 = 1

                    wt_hour1 = (1.0 - hour_rem)
                    wt_hour2 = 1.0 - wt_hour1


                    wts = 0.0
                    wts(hour1) = wt_hour1
                    wts(hour2) = wt_hour2
                    error = 0
                    hour_wts = wts
                    hour_indices(1) = hour1
                    hour_indices(2) = hour2
                    return
                endif
            enddo

            wts = NAN
            hour_wts = wts
            hour_indices = 0
            error = -1
            return

        end subroutine hour_weights

        type(time_str) function convert_to_local_time(time,longitude,error)

            implicit none

            type(time_str),intent(IN)    :: time
            real(4),intent(IN)            :: longitude
            integer(4)                    :: error

            real(8)                        :: secs
            type(time_str)                :: temp_time

            real(8),parameter            :: DEG_TO_SECS = 240.0D0
            real(8),parameter            :: SECS_IN_DAY = 86400.0D0


            secs = longitude * DEG_TO_SECS

            if (finite(secs) .and. finite(time%m_secs)) then
                temp_time%m_secs = modulo((time%m_secs + secs),SECS_IN_DAY)
                call update_hms(temp_time,error)
            else
                error = -1
                temp_time%m_secs = nan
            endif

            convert_to_local_time = temp_time

            return

        end function convert_to_local_time

        integer(4) function calc_pentad(date,remainder)

            type(date_str),intent(IN)        :: date
            real(4),optional                :: remainder

            integer(4)                        :: p_num
            integer(4)                        :: d_num

            p_num = floor((date%r_d - RD_1_1_1978)/5.0D0)
            d_num = calc_day_num_1978(date)
            remainder = -2.5D0 + date%r_d - ((p_num*5.0D0) + RD_1_1_1978)
            calc_pentad = 1 + floor((d_num -1)/5.0)
            !pentad one contains Jan_1_1978 through Jan_5_1978
            !pentad two contains Jan_6_1978 through Jan_10_1978, etc


        end function calc_pentad

        integer(4) function calc_month(date,remainder)

            type(date_str),intent(IN)        :: date
            real(4),optional                :: remainder

            remainder = NAN
            calc_month = (date%year - 1978)*12 + date%month

        end function calc_month


        integer(4) function calc_day_num_1978(date,remainder)

            type(date_str),intent(IN)        :: date
            real(4),optional                :: remainder

            real(4) :: temp

            calc_day_num_1978 = 1 + floor(date%r_d - RD_1_1_1978)
            temp = real(date%r_d)
            temp =  temp - floor(temp)
            if (present(remainder)) remainder = temp

        end function calc_day_num_1978











        




                




    end module date_and_time

