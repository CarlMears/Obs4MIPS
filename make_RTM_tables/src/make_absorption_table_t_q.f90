module make_absorption_table_t_q 

    use, intrinsic :: iso_fortran_env, only: real32,real64
    use nan_support
    use atmos_abs_routines, only: fdabscoeff
    use msu_constants
    use amsu_constants, only: AMSU_A_Freq, AMSU_A_Freq_Split_1, AMSU_A_BANDWIDTH, AMSU_A_Stopband

    implicit none

    private

    public :: compute_absorption_tables_amsu_dsb,compute_absorption_tables_amsu,compute_absorption_tables_msu

contains

    subroutine compute_absorption_tables_amsu_dsb(channel, ivap, ioxy, num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, &
                                    abs_table, abs_table_per_Pa)
        implicit none

        integer(4), intent(in) :: channel
        integer(4), intent(in) :: ivap
        integer(4), intent(in) :: ioxy
        integer(4), intent(in) :: num_T
        integer(4), intent(in) :: num_p
        integer(4), intent(in) :: num_q

        real(4), intent(in) :: T0
        real(4), intent(in) :: Delta_T
        real(4), intent(in) :: Delta_P
        real(4), intent(in) :: Delta_q

        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table
        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table_per_Pa

        integer(4), parameter :: amsu_num_freq = 14
        integer(4) :: amsu_num_per_side
        integer(4) :: freq_index
        integer(4) :: T_index, P_index, q_index

        real(4), dimension(amsu_num_freq) :: amsu_freq_arr
        real(4), dimension(amsu_num_freq) :: amsu_freq_wt

        real(4) :: center_freq_lower, center_freq_upper, bandwidth
        real(4) :: T, P, PV, q, freq
        real(4) :: ao, av, total_abs

        real(8), parameter :: M_W_air = 2.8966D-2
        real(8), parameter :: M_W_H2O = 1.8015324D-2
        real(8), parameter :: R_GAS = 8.3145112D0
        real(8), parameter :: g = 9.80665D0

        real(8) :: c_air
        real(8) :: c_h2o
        real(8) :: rho_dry
        real(8) :: rho_vap

        if (channel .ne. 5) then
            print *, 'Error: This subroutine is designed for channel 5 only.'
            stop
        endif

        c_air = R_GAS / M_W_AIR
        c_h2o = R_GAS / M_W_H2O

        amsu_num_per_side = amsu_num_freq / 2
        center_freq_lower = AMSU_A_Freq(channel) - AMSU_A_Freq_Split_1(channel)
        center_freq_upper = AMSU_A_Freq(channel) + AMSU_A_Freq_Split_1(channel)
        bandwidth = AMSU_A_BANDWIDTH(channel)

        do freq_index = 0, amsu_num_per_side - 1
            amsu_freq_arr(freq_index + 1) = center_freq_lower - bandwidth / 2.0 + freq_index * bandwidth / (amsu_num_per_side - 1)
            amsu_freq_arr(freq_index + 1 + amsu_num_per_side) = center_freq_upper - bandwidth / 2.0 + freq_index * bandwidth / (amsu_num_per_side - 1)

            if ((freq_index == 0) .or. (freq_index == amsu_num_per_side - 1)) then
                amsu_freq_wt(freq_index + 1) = 0.25 / (amsu_num_per_side - 1.0)
                amsu_freq_wt(freq_index + 1 + amsu_num_per_side) = 0.25 / (amsu_num_per_side - 1.0)
            else
                amsu_freq_wt(freq_index + 1) = 0.5 / (amsu_num_per_side - 1.0)
                amsu_freq_wt(freq_index + 1 + amsu_num_per_side) = 0.5 / (amsu_num_per_side - 1.0)
            endif
        enddo

        print *, amsu_freq_arr
        print *, amsu_freq_wt

        abs_table = 0.0
        abs_table_per_Pa = 0.0

        do freq_index = 1, amsu_num_freq
            freq = amsu_freq_arr(freq_index)
            print *, freq, amsu_freq_wt(freq_index)

            do T_index = 0, num_T
                T = T0 + Delta_T * T_index
                do P_index = 0, num_p
                    P = Delta_P * P_index
                    do q_index = 0, num_q
                        q = Delta_q * q_index
                        PV = P * q / (0.622 + 0.378 * q)

                        call fdabscoeff(ivap, ioxy, freq, P, T, PV, av, ao)
                        total_abs = av + ao

                        abs_table(T_index, P_index, q_index) = abs_table(T_index, P_index, q_index) + total_abs * amsu_freq_wt(freq_index)

                        rho_dry = (P - PV) / (c_air * T)
                        rho_vap = PV / (c_h2o * T)

                        abs_table_per_Pa(T_index, P_index, q_index) = real(abs_table_per_Pa(T_index, P_index, q_index) + &
                                                         amsu_freq_wt(freq_index) * total_abs * 0.001 / ((rho_dry + rho_vap) * g), real32)
                    enddo
                enddo
            enddo
        enddo

    end subroutine compute_absorption_tables_amsu_dsb

    subroutine compute_absorption_tables_amsu(channel, ivap, ioxy, num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, &
                                    abs_table, abs_table_per_Pa)

        implicit none

        integer(4), intent(in) :: channel
        integer(4), intent(in) :: ivap
        integer(4), intent(in) :: ioxy
        integer(4), intent(in) :: num_T
        integer(4), intent(in) :: num_p
        integer(4), intent(in) :: num_q

        real(4), intent(in) :: T0
        real(4), intent(in) :: Delta_T
        real(4), intent(in) :: Delta_P
        real(4), intent(in) :: Delta_q

        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table
        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table_per_Pa

        integer(4), parameter :: amsu_num_freq = 14
        integer(4) :: amsu_num_per_side
        integer(4) :: freq_index
        integer(4) :: T_index, P_index, q_index

        real(4), dimension(amsu_num_freq) :: amsu_freq_arr
        real(4), dimension(amsu_num_freq) :: amsu_freq_wt    

        real(4) :: center_freq_lower, center_freq_upper, bandwidth
        real(4) :: T, P, PV, q, freq
        real(4) :: ao, av, total_abs

        
        real(8), parameter :: M_W_air = 2.8966D-2
        real(8), parameter :: M_W_H2O = 1.8015324D-2
        real(8), parameter :: R_GAS = 8.3145112D0
        real(8), parameter :: g = 9.80665D0

        real(8) :: c_air
        real(8) :: c_h2o
        real(8) :: rho_dry
        real(8) :: rho_vap

        c_air = R_GAS / M_W_AIR
        c_h2o = R_GAS / M_W_H2O
        
        amsu_num_per_side = amsu_num_freq/2
        center_freq_lower = AMSU_A_Freq(channel) - AMSU_A_BANDWIDTH(channel)/4.0 - AMSU_A_Stopband(channel)/4.0
        center_freq_upper = AMSU_A_Freq(channel) + AMSU_A_BANDWIDTH(channel)/4.0 + AMSU_A_Stopband(channel)/4.0

        bandwidth = AMSU_A_BANDWIDTH(channel)/2.0 - AMSU_A_Stopband(channel)/2.0 
        do freq_index = 0,amsu_num_per_side - 1
            amsu_freq_arr(freq_index+1) =                &
                    center_freq_lower -                    &
                    bandwidth/2.0 +                        &
                    freq_index*bandwidth/(amsu_num_per_side-1)
            amsu_freq_arr(freq_index+1+amsu_num_per_side) =                &
                    center_freq_upper -                    &
                    bandwidth/2.0 +                        &
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
        
        abs_table = 0.0
        abs_table_per_Pa = 0.0
        do freq_index = 1,amsu_num_freq
            freq = amsu_freq_arr(freq_index)
            print*,freq,amsu_freq_wt(freq_index)
            do T_index = 0,num_T
            T = T0+Delta_T*T_index
            do P_index = 0,num_p
                    P = Delta_P*P_index
                    do q_index = 0,num_q
                        q = Delta_q*q_index
                        total_abs = 0.0

                        PV = p*q/(0.622+0.378*q)

                        call FDABSCOEFF(ivap,ioxy,freq,P,T,PV, av,ao)
                        total_abs = av + ao
                        abs_table(T_index,P_index,q_index) = abs_table(T_index,P_index,q_index) + total_abs*amsu_freq_wt(freq_index)

                        ! find density

                        rho_dry = (p-pv)/(c_air*T)
                        rho_vap = pv/(c_h2o*T)
                        abs_table_per_Pa(T_index,P_index,q_index) = real(abs_table_per_Pa(T_index,P_index,q_index) + &
                                                                    amsu_freq_wt(freq_index)*total_abs*0.001/((rho_dry + rho_vap) * g), real32)
                            
                    enddo
                enddo
            enddo
        enddo
    end subroutine compute_absorption_tables_amsu

    subroutine compute_absorption_tables_msu(channel, ivap, ioxy, num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, &
                                    abs_table, abs_table_per_Pa)

        implicit none

        integer(4), intent(in) :: channel
        integer(4), intent(in) :: ivap
        integer(4), intent(in) :: ioxy
        integer(4), intent(in) :: num_T
        integer(4), intent(in) :: num_p
        integer(4), intent(in) :: num_q

        real(4), intent(in) :: T0
        real(4), intent(in) :: Delta_T
        real(4), intent(in) :: Delta_P
        real(4), intent(in) :: Delta_q

        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table
        real(4), dimension(0:num_T,0:num_p,0:num_q), intent(out) :: abs_table_per_Pa

        integer(4), parameter :: msu_num_freq = 13
        integer(4) :: freq_index
        integer(4) :: T_index, P_index, q_index

        real(4), dimension(msu_num_freq) :: msu_freq_arr
        real(4), dimension(msu_num_freq) :: msu_freq_wt    

        real(4) :: T, P, PV, q, freq
        real(4) :: ao, av, total_abs

        real(8), parameter :: M_W_air = 2.8966D-2
        real(8), parameter :: M_W_H2O = 1.8015324D-2
        real(8), parameter :: R_GAS = 8.3145112D0
        real(8), parameter :: g = 9.80665D0

        real(8) :: c_air
        real(8) :: c_h2o
        real(8) :: rho_dry
        real(8) :: rho_vap

        c_air = R_GAS / M_W_AIR
        c_h2o = R_GAS / M_W_H2O
        
        do freq_index = 0,msu_num_freq - 1
            msu_freq_arr(freq_index+1) =                &
                    MSU_FREQUENCY(channel) -                    &
                    MSU_BANDWIDTH(channel)/2.0 +                        &
                    freq_index*MSU_BANDWIDTH(channel)/(msu_num_freq-1)
            
            ! these weights perform trapezoidal integration.

            if ((freq_index == 0) .or. (freq_index == msu_num_freq-1)) then
                msu_freq_wt(freq_index+1) = 0.25/(msu_num_freq-1.0)
                msu_freq_wt(freq_index+1+msu_num_freq) = 0.25/(msu_num_freq-1.0)
            else
                msu_freq_wt(freq_index+1) = 0.5/(msu_num_freq-1.0)
                msu_freq_wt(freq_index+1+msu_num_freq) = 0.5/(msu_num_freq-1.0)
            endif
        enddo
        
        abs_table = 0.0
        abs_table_per_Pa = 0.0
        do freq_index = 1,msu_num_freq
            freq = msu_freq_arr(freq_index)
            print*,freq,msu_freq_wt(freq_index)
            do T_index = 0,num_T
            T = T0+Delta_T*T_index
            do P_index = 0,num_p
                    P = Delta_P*P_index
                    do q_index = 0,num_q
                        q = Delta_q*q_index
                        total_abs = 0.0

                        PV = p*q/(0.622+0.378*q)

                        call FDABSCOEFF(ivap,ioxy,freq,P,T,PV, av,ao)
                        total_abs = av + ao
                        abs_table(T_index,P_index,q_index) = abs_table(T_index,P_index,q_index) + total_abs*msu_freq_wt(freq_index)

                        ! find density

                        rho_dry = (p-pv)/(c_air*T)
                        rho_vap = pv/(c_h2o*T)
                        abs_table_per_Pa(T_index,P_index,q_index) = real(abs_table_per_Pa(T_index,P_index,q_index) + &
                                                                    msu_freq_wt(freq_index)*total_abs*0.001/((rho_dry + rho_vap) * g), real32)
                            
                    enddo
                enddo
            enddo
        enddo
    end subroutine compute_absorption_tables_msu


end module make_absorption_table_t_q