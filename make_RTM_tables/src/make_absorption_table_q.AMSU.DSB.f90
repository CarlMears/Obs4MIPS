program make_absorption_table

	use nan_support
	use rtm
	use msu_constants
	use amsu_constants

	implicit none

	integer(4), parameter :: num_T = 200
	integer(4), parameter :: num_p = 110
	integer(4), parameter :: num_q = 150

	real(4), parameter :: T0 = 140.0
	real(4), parameter :: Delta_T = 1.0
	real(4), parameter :: Delta_P = 10.0
	real(4), parameter :: Delta_q = 0.001

	integer(4), parameter :: ivap = 4
	integer(4), parameter :: ioxy = 4
	integer(4), parameter :: channel = 5

	real(4), dimension(0:num_T,0:num_p,0:num_q) :: abs_table
	real(4), dimension(0:num_T,0:num_p,0:num_q) :: abs_table_per_Pa

	character(len=120) :: file

	call compute_absorption_tables_amsu_dsb(channel, ivap, ioxy, num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, &
							    abs_table, abs_table_per_Pa)

	write(file,111) channel, ioxy, ivap
111   format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\amsu_',i2.2,'_abs_table_q.',i1,'.',i1,'.dat')

	open(unit=15, file=file, form='BINARY')
	write(15) num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, ivap, ioxy, channel
	write(15) abs_table
	close(15)

	write(file,112) channel, ioxy, ivap
112   format('B:\idl_library\MSU_AMSU_simulation\method_2\data\abs_tables\amsu_',i2.2,'_abs_table_q_per_Pa.',i1,'.',i1,'.dat')

	open(unit=15, file=file, form='BINARY')
	write(15) num_T, num_p, num_q, T0, Delta_T, Delta_P, Delta_q, ivap, ioxy, channel
	write(15) abs_table_per_Pa
	close(15)

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

						call FDABSCOEFF(ivap, ioxy, freq, P, T, PV, av, ao)
						total_abs = av + ao

						abs_table(T_index, P_index, q_index) = abs_table(T_index, P_index, q_index) + total_abs * amsu_freq_wt(freq_index)

						rho_dry = (P - PV) / (c_air * T)
						rho_vap = PV / (c_h2o * T)

						abs_table_per_Pa(T_index, P_index, q_index) = abs_table_per_Pa(T_index, P_index, q_index) + &
													     amsu_freq_wt(freq_index) * total_abs * 0.001 / ((rho_dry + rho_vap) * g)
					enddo
				enddo
			enddo
		enddo

	end subroutine compute_absorption_tables_amsu_dsb

end program make_absorption_table