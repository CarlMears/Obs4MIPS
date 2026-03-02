module ocean_surface_emiss

    implicit none

contains

    subroutine surterm(freq, tht, sst, wind, emiss)
        ! surface emissivity for v and h POL from geometric optics without wind direction
        ! signal
        ! E = 1 - (1-F)*R(geo)
        ! R(geo) : geometric optic reflectivity (ATBD: 57)

    real(4) :: freq, tht, sst, wind
    real(4) :: em0(2), emiss(2), xm1_tab(2, 3), xm2_tab(2, 3), hterm(2)
    real(4) :: hcoef(0:20, 21, 46, 2, 50)
    real(4) :: surtep, wt, ym1, ym2, wind1, qfac1, qfac2, fems, gems
    integer(4) :: i1, ipol
        common /haray/ hcoef

        ! new values for m1 and m2 from TMI
        ! they are now dependent on pol
        data xm1_tab /0.0002, 0.0020, 0.0015, 0.0030, 0.0026, 0.0033/
        data xm2_tab /0.0069, 0.0060, 0.0074, 0.0066, 0.0070, 0.0066/

        surtep = sst + 273.16
        ! specular emissivity (Fresnel) for v and h
        call fdem0(freq, tht, sst, em0)

        ! F term (ATBD: 58)
        ! m1 and m2 as well as the spline point w1 are updated from ATBD using TMI results
        ! they are now different for v and h pol
        ! w1 = 3 (v) , w1 = 7 (h) , w2 = 12

        call surterm_interface(freq, tht, surtep, wind, hterm)
        ! ATBD(57) r.h.s. terms in [ ... ]

        if (freq <= 19.35) then
            i1 = 1
            wt = (freq - 10.65) / 8.7
            if (wt < 0.0) wt = 0.0
        else
            i1 = 2
            wt = (freq - 19.35) / 17.65
            if (wt > 1.0) wt = 1.0
        endif

        do ipol = 1, 2
            ym1 = (1.0 - wt) * xm1_tab(ipol, i1) + wt * xm1_tab(ipol, i1 + 1) ! m1
            ym2 = (1.0 - wt) * xm2_tab(ipol, i1) + wt * xm2_tab(ipol, i1 + 1) ! m2

            if (ipol == 1) then
                wind1 = 3.0
                qfac1 = 18.0
                qfac2 = 7.5
            else
                wind1 = 7.0
                qfac1 = 10.0
                qfac2 = 9.5
            endif

            if (wind <= wind1) then
                fems = ym1 * wind
            else
                if (wind < 12.0) then
                    fems = ym1 * wind + (ym2 - ym1) * (wind - wind1) * (wind - wind1) / qfac1
                else
                    fems = ym2 * wind - (ym2 - ym1) * qfac2
                endif
            endif

            gems = hterm(ipol)
            ! (ATBD: 57, 2nd term on rhs)

            emiss(ipol) = fems + (1.0 - fems) * (em0(ipol) + gems) ! (ATBD: 57)
        enddo

        return
    end subroutine surterm

    subroutine surterm_interface(freq, tht, surtep, wind, hterm)
        implicit none
        ! ATBD(57) r.h.s. = RGEO - R0

        real(4) :: freq, tht, surtep, wind
        real(4) :: hterm(2)
        real(4) :: hcoef(0:20, 21, 46, 2, 50)
        real(4) :: t_freq, t_tht, t_surtep, t_wind
        real(4) :: dfreq, dtht, dsurtep, dwind
        real(4) :: freq_1, tht_1, surtep_1, wind_1
        real(4) :: hint(2, 2, 2, 2), gint(2, 2, 2), fint(2, 2)
        integer(4) :: ifreq, itht, isurtep, iwind
        integer(4) :: i1, i2, k1, k2, j1, j2, l1, l2

        common /haray/ hcoef

        ifreq = int((freq + 1.0) / 2.0)
        itht = 1 + int(tht / 2.0)
        isurtep = int((surtep - 270.0) / 2.0) + 1
        iwind = int(wind)

        if (itht < 1) then
            itht = 1
            tht = 0.0
        else if (itht > 46) then
            itht = 46
            tht = 90.0
        endif

        if (ifreq < 1) then
            ifreq = 1
            freq = 1.0
        else if (ifreq > 50) then
            ifreq = 50
            freq = 99.0
        endif

        if (isurtep < 1) then
            isurtep = 1
            surtep = 270.0
        else if (isurtep > 21) then
            isurtep = 21
            surtep = 310.0
        endif

        if (iwind < 0) then
            iwind = 0
            wind = 0.0
        else if (iwind > 20) then
            iwind = 20
            wind = 20.0
        endif

        freq_1 = 2.0 * ifreq - 1.0
        tht_1 = 2.0 * (itht - 1)
        surtep_1 = 270.0 + 2.0 * (isurtep - 1)
        wind_1 = real(iwind, 4)

        dfreq = 2.0
        dtht = 2.0
        dsurtep = 2.0
        dwind = 1.0

        t_freq = (freq - freq_1) / dfreq
        t_tht = (tht - tht_1) / dtht
        t_surtep = (surtep - surtep_1) / dsurtep
        t_wind = (wind - wind_1) / dwind

        i1 = iwind
        i2 = iwind + 1
        if (i2 > 20) i2 = 20

        k1 = isurtep
        k2 = isurtep + 1
        if (k2 > 21) k2 = 21

        j1 = itht
        j2 = itht + 1
        if (j2 > 46) j2 = 46

        l1 = ifreq
        l2 = ifreq + 1
        if (l2 > 50) l2 = 50

        hint(1, 1, :, 1) = (1.0 - t_wind) * hcoef(i1, k1, j1, :, l1) + &
                           t_wind * hcoef(i2, k1, j1, :, l1)

        hint(2, 1, :, 1) = (1.0 - t_wind) * hcoef(i1, k2, j1, :, l1) + &
                           t_wind * hcoef(i2, k2, j1, :, l1)

        hint(1, 2, :, 1) = (1.0 - t_wind) * hcoef(i1, k1, j2, :, l1) + &
                           t_wind * hcoef(i2, k1, j2, :, l1)

        hint(1, 1, :, 2) = (1.0 - t_wind) * hcoef(i1, k1, j1, :, l2) + &
                           t_wind * hcoef(i2, k1, j1, :, l2)

        hint(2, 2, :, 1) = (1.0 - t_wind) * hcoef(i1, k2, j2, :, l1) + &
                           t_wind * hcoef(i2, k2, j2, :, l1)

        hint(2, 1, :, 2) = (1.0 - t_wind) * hcoef(i1, k2, j1, :, l2) + &
                           t_wind * hcoef(i2, k2, j1, :, l2)

        hint(1, 2, :, 2) = (1.0 - t_wind) * hcoef(i1, k1, j2, :, l2) + &
                           t_wind * hcoef(i2, k1, j2, :, l2)

        hint(2, 2, :, 2) = (1.0 - t_wind) * hcoef(i1, k2, j2, :, l2) + &
                           t_wind * hcoef(i2, k2, j2, :, l2)

        gint(1, :, 1) = (1.0 - t_surtep) * hint(1, 1, :, 1) + t_surtep * hint(2, 1, :, 1)
        gint(2, :, 1) = (1.0 - t_surtep) * hint(1, 2, :, 1) + t_surtep * hint(2, 2, :, 1)
        gint(1, :, 2) = (1.0 - t_surtep) * hint(1, 1, :, 2) + t_surtep * hint(2, 1, :, 2)
        gint(2, :, 2) = (1.0 - t_surtep) * hint(1, 2, :, 2) + t_surtep * hint(2, 2, :, 2)

        fint(:, 1) = (1.0 - t_tht) * gint(1, :, 1) + t_tht * gint(2, :, 1)
        fint(:, 2) = (1.0 - t_tht) * gint(1, :, 2) + t_tht * gint(2, :, 2)

        hterm(:) = (1.0 - t_freq) * fint(:, 1) + t_freq * fint(:, 2)

        return
    end subroutine surterm_interface
end module ocean_surface_emiss