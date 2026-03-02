$freeform
module logging
	
		use date_and_time

		integer(4),private				:: log_unit = 44
		logical(4),private				:: log_open = .false.
		character(len = 120),private	:: log_file
		character(len = 300)			:: log_str
		integer(4)						:: log_err

	contains

		subroutine init_log_file(file,error)

			character(len = 120)	:: file
			integer(4)				:: error
			integer(4)				:: ioerror


			open(unit = log_unit, file = file, iostat = io_error,ACCESS = 'APPEND',action='WRITE')
			
			if (io_error .eq. 0) then
				log_open = .true.
				log_file = file
				print *,'Opened Log File In Fortran: '//file
				error = 0
			else
				log_open = .false.
				log_file = 'Log File not Open'
				print *,'Could not open log file in Fortran: '//file
				error = -1
			endif

		end subroutine init_log_file

		subroutine write_log(a,error)

			character*(*)			:: a
			integer(4)				:: error



	10		format(a)
			write(6,10)curr_date_string(error)//': '//a
			if (log_open) then
				write(log_unit,10)curr_date_string(error)//': '//a
			endif

		end subroutine write_log
		
		subroutine log(a,error)

			character*(*)			:: a
			integer(4)				:: error



	10		format(a)
			write(6,10)curr_date_string(error)//': '//a
			if (log_open) then
				write(log_unit,10)curr_date_string(error)//': '//a
			endif

		end subroutine log
		
		
		subroutine close_log(error)
		
		
		    integer(4)				:: error
		    
		    error = 0
		    close(log_unit)
		    
		end subroutine close_log

	end module logging