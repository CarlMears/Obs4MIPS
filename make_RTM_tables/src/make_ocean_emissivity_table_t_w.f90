module make_ocean_emissivity_table_t_w 

    use, intrinsic :: iso_fortran_env, only: real32,real64
    use amsu_constants, only: AMSU_A_Freq, AMSU_A_Freq_Split_1, AMSU_A_BANDWIDTH, AMSU_A_Polarization, AMSU_NOM_EIAS, AMSU_VIEW_ANGLES
    use ocean_surface_emiss, only: surterm

    implicit none

    private

    public :: compute_ocean_emissivity_tables_amsu_dsb
    !public :: compute_ocean_emissivity_tables_amsu,compute_ocean_emissivity_tables_msu

contains

    pure elemental function cosd(x)

        real(4), intent(in) :: x
        real(4) :: cosd

        real(4), parameter :: PI_F32 = 4.0*atan(1.0)
        real(4), parameter :: DEG2RAD_F32 = PI_F32/180.0

        cosd = cos(x * DEG2RAD_F32)
    end function cosd

    pure elemental function sind(x)

        real(4), intent(in) :: x
        real(4) :: sind

        real(4), parameter :: PI_F32 = 4.0*atan(1.0)
        real(4), parameter :: DEG2RAD_F32 = PI_F32/180.0

        sind = sin(x * DEG2RAD_F32)
    end function sind

    subroutine compute_ocean_emissivity_tables_amsu_dsb(channel, num_T, num_W, T0, Delta_T, W0, Delta_W, emiss_table)

        integer(4), intent(in) :: channel
        integer(4), intent(in) :: num_T, num_W
        real(4), intent(in) :: T0, Delta_T, W0, Delta_W
        real(4), dimension(0:num_T,0:num_W,1:15), intent(out) :: emiss_table
	
	    real(4),dimension(0:num_T,0:num_W,1:15,1:2)  :: emiss_table_by_pol  ! temperature,fov,pol (vpol = 1 hpol = 2)
	

	    integer(4)							:: polarization
	    integer(4),parameter			:: amsu_num_freq = 14
	    integer(4)						:: amsu_num_per_side

	    integer(4)						:: freq_index
	    real(4),dimension(amsu_num_freq) :: amsu_freq_arr
	    real(4),dimension(amsu_num_freq) :: amsu_freq_wt
	    real(4)							:: center_freq_lower,center_freq_upper,bandwidth

	    integer(4)							:: fov,T_index,W_index,ipol
	    real(4)								:: tht,theta_view,freq,T,W,sst
	    real(4),dimension(2)				:: emiss

	   
	    polarization = AMSU_A_Polarization(channel)

        ! Trapezoidal integration

        amsu_num_per_side = amsu_num_freq/2
        center_freq_lower = AMSU_A_Freq(channel) - AMSU_A_Freq_Split_1(channel)
        center_freq_upper = AMSU_A_Freq(channel) + AMSU_A_Freq_Split_1(channel)

        bandwidth = AMSU_A_BANDWIDTH(channel)

        do freq_index = 0,amsu_num_per_side - 1
            amsu_freq_arr(freq_index+1) =				&
                    center_freq_lower -					&
                    bandwidth/2.0 +						&
                    freq_index*bandwidth/(amsu_num_per_side-1)
            amsu_freq_arr(freq_index+1+amsu_num_per_side) =				&
                    center_freq_upper -					&
                    bandwidth/2.0 +						&
                    freq_index*bandwidth/(amsu_num_per_side-1)

            ! these weights perform trapezoidal integration.

            if ((freq_index == 0) .or. (freq_index == amsu_num_per_side-1)) then
                amsu_freq_wt(freq_index+1) = 0.25/(amsu_num_per_side-1.0)
                amsu_freq_wt(freq_index+1+amsu_num_per_side) = 0.25/(amsu_num_per_side-1.0)
            else
                amsu_freq_wt(freq_index+1) = 0.5/(amsu_num_per_side-1.0)
                amsu_freq_wt(freq_index+1+amsu_num_per_side) = 0.5/(amsu_num_per_side-1.0)
            endif
        enddo

        do fov = 1,15
            tht = AMSU_NOM_EIAS(fov)
            do freq_index = 1,amsu_num_freq
                freq = amsu_freq_arr(freq_index)
                print*,freq,fov	
                do T_index = 0,num_T
                T = T0+Delta_T*T_index
                sst = T - 273.16
                do W_index = 0,num_W
                    W = W0 + Delta_W*W_index
                    call surterm(freq,tht,sst,W, emiss)
                    emiss_table_by_pol(T_index,W_index,fov,:) = emiss_table_by_pol(T_index,W_index,fov,:) + emiss*amsu_freq_wt(freq_index)
                enddo
                enddo
            enddo
        enddo

	! combine polarizations according to view angle

        do fov = 1,15
            theta_view = AMSU_VIEW_ANGLES(fov)
            do T_index = 0,num_T
                do W_index = 0,num_W
                    if (polarization .eq. 1) then !V-pol
                        emiss_table(T_index,W_index,fov)	=	 &
                            emiss_table_by_pol(T_index,W_index,fov,1)*cosd(theta_view)*cosd(theta_view) + &
                            emiss_table_by_pol(T_index,W_index,fov,2)*sind(theta_view)*sind(theta_view)
                    else if (polarization .eq. 2) then !H-pol
                        emiss_table(T_index,W_index,fov)	=	  &
                            emiss_table_by_pol(T_index,W_index,fov,2)*cosd(theta_view)*cosd(theta_view) + &
                            emiss_table_by_pol(T_index,W_index,fov,1)*sind(theta_view)*sind(theta_view)
                    endif
                enddo
            enddo
        enddo

    end subroutine compute_ocean_emissivity_tables_amsu_dsb

end module
