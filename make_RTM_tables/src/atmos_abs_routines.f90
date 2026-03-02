
module atmos_abs_routines
      
      use dielectric, only: dielectric_meissner_wentz

      implicit none

contains
    subroutine fdabscoeff(ivap,ioxy,freq,p,t,pv, av,ao)

        !     Atmospheric Absorption:

        ! 				!     ivap = 1: Rosenkranz 1998 : NOT IMPLEMENTED HERE
        ! 				!     ivap = 2: Rosenkranz 1998 with small adjustments (Meissner, May 2002): NOT IMPLEMENTED HERE
        ! 				!     ivap = 3: Wentz, March 2002 : NOT IMPLEMENTED HERE
        ! 				!     ivap = 4: Rosenkranz 1998 modified: abh2o_rk_modified

        ! 				!     ioxy = 1: Rosenkranz 1998 : NOT IMPLEMENTED HERE
        ! 				!     ioxy = 2: Wentz 1992 : NOT IMPLEMENTED HERE
        ! 				!     ioxy = 3: Wentz 1992 modified : NOT IMPLEMENTED HERE
        ! 				!     ioxy = 4: Rosenkranz 1992 modified: fdabsoxy_1992_modified
        ! 				!     ioxy = 5: Tretyakov et al 2005 modified: fdabsoxy_tretyakov_modified 
 
        ! 				!
        ! 				!     freq   Frequency [in GHz]
        ! 				!     P      Pressure  [in h Pa]
        ! 				!     T      Temperature [in K]
        ! 				!     PV     water vapor pressure [in hPa]

        ! 				!     Output:	
        ! 				!     AV          water vapor absorption coefficient [neper/km]
        ! 				!     AO          oxygen absortption coefficient     [neper/km]
        ! 				! 
        implicit none

        real(4), parameter :: xnaper=0.2302585094 !convert db/km to naper/km

        integer(4), intent(in) :: ivap,ioxy
        real(4), intent(in) :: freq,p,t,pv
        real(4), intent(out) :: av,ao
        real(4) gamoxy,gamh2o

        if (ivap == 4) then
            call abh2o_rk_modified(p,t,pv,freq,gamh2o)
        else 
            print *, 'Error: ivap value of ', ivap, ' is not implemented in fdabscoeff'
            stop
        endif

        if (ioxy == 4) then
            call fdabsoxy_1992_modified(p,t,pv,freq, gamoxy)
        else if (ioxy == 5) then
            call fdabsoxy_tretyakov_modified(p,t,pv,freq, gamoxy)
        else 
            print *, 'Error: ioxy value of ', ioxy, ' is not implemented in fdabscoeff'
            stop
        endif

        ao=xnaper*gamoxy
        av=xnaper*gamh2o 
        return
    end subroutine fdabscoeff


      subroutine fdabsoxy_1992_modified(p,t,pv,freq, gamoxy)
      implicit none

      integer(4), parameter :: nlines=44
      integer(4),save:: istart
      integer(4) i
      real(4) p,t,pv,freq,gamoxy
      real(4) tht,pwet,pdry,ga,gasq,delta,rnuneg,rnupos,ff,zterm,apterm,sftot,xterm
      real(4) h(6,nlines)
      real(4),save:: f0(nlines),a1(nlines),a2(nlines),a3(nlines),a4(nlines),a5(nlines),a6(nlines)

      real(8) sum

      data istart/1/
      data a4/38*0., 6*0.6/

!          freq          a1      a2       a3       a5          a6

      data h/ &
     50.474238,    0.94e-6,  9.694,  8.60e-3,  0.210,  0.685, &
     50.987749,    2.46e-6,  8.694,  8.70e-3,  0.190,  0.680, &
     51.503350,    6.08e-6,  7.744,  8.90e-3,  0.171,  0.673, &
     52.021410,   14.14e-6,  6.844,  9.20e-3,  0.144,  0.664, &
     52.542394,   31.02e-6,  6.004,  9.40e-3,  0.118,  0.653, &
     53.066907,   64.10e-6,  5.224,  9.70e-3,  0.114,  0.621, &
     53.595749,  124.70e-6,  4.484, 10.00e-3,  0.200,  0.508, &
     54.130000,  228.00e-6,  3.814, 10.20e-3,  0.291,  0.375, &
     54.671159,  391.80e-6,  3.194, 10.50e-3,  0.325,  0.265, &
     55.221367,  631.60e-6,  2.624, 10.79e-3,  0.224,  0.295, &
     55.783802,  953.50e-6,  2.119, 11.10e-3, -0.144,  0.613, &
     56.264775,  548.90e-6,  0.015, 16.46e-3,  0.339, -0.098, &
     56.363389, 1344.00e-6,  1.660, 11.44e-3, -0.258,  0.655, &
     56.968206, 1763.00e-6,  1.260, 11.81e-3, -0.362,  0.645, &
     57.612484, 2141.00e-6,  0.915, 12.21e-3, -0.533,  0.606, &
     58.323877, 2386.00e-6,  0.626, 12.66e-3, -0.178,  0.044, &
     58.446590, 1457.00e-6,  0.084, 14.49e-3,  0.650, -0.127, &
     59.164207, 2404.00e-6,  0.391, 13.19e-3, -0.628,  0.231, &
     59.590983, 2112.00e-6,  0.212, 13.60e-3,  0.665, -0.078, &
     60.306061, 2124.00e-6,  0.212, 13.82e-3, -0.613,  0.070, &
     60.434776, 2461.00e-6,  0.391, 12.97e-3,  0.606, -0.282, &
     61.150560, 2504.00e-6,  0.626, 12.48e-3,  0.090, -0.058, &
     61.800154, 2298.00e-6,  0.915, 12.07e-3,  0.496, -0.662, &
     62.411215, 1933.00e-6,  1.260, 11.71e-3,  0.313, -0.676, &
     62.486260, 1517.00e-6,  0.083, 14.68e-3, -0.433,  0.084, &
     62.997977, 1503.00e-6,  1.665, 11.39e-3,  0.208, -0.668, &
     63.568518, 1087.00e-6,  2.115, 11.08e-3,  0.094, -0.614, &
     64.127767,  733.50e-6,  2.620, 10.78e-3, -0.270, -0.289, &
     64.678903,  463.50e-6,  3.195, 10.50e-3, -0.366, -0.259, &
     65.224071,  274.80e-6,  3.815, 10.20e-3, -0.326, -0.368, &
     65.764772,  153.00e-6,  4.485, 10.00e-3, -0.232, -0.500, &
     66.302091,   80.09e-6,  5.225,  9.70e-3, -0.146, -0.609, &
     66.836830,   39.46e-6,  6.005,  9.40e-3, -0.147, -0.639, &
     67.369598,   18.32e-6,  6.845,  9.20e-3, -0.174, -0.647, &
     67.900867,    8.01e-6,  7.745,  8.90e-3, -0.198, -0.655, &
     68.431005,    3.30e-6,  8.695,  8.70e-3, -0.210, -0.660, &
     68.960311,    1.28e-6,  9.695,  8.60e-3, -0.220, -0.665, &
     118.750343,  945.00e-6,  0.009, 16.30e-3, -0.031,  0.008, &
     368.498350,   67.90e-6,  0.049, 19.20e-3,  0.0,    0.0, &
     424.763124,  638.00e-6,  0.044, 19.16e-3,  0.0,    0.0, &
     487.249370,  235.00e-6,  0.049, 19.20e-3,  0.0,    0.0, &
     1715.393150,   99.60e-6,  0.145, 18.10e-3,  0.0,    0.0, &
     1773.839675,  671.00e-6,  0.130, 18.10e-3,  0.0,    0.0, &
     1834.145330,  180.00e-6,  0.147, 18.10e-3,  0.0,    0.0/

      if(istart.eq.1) then
          istart=0
          f0(:)=h(1,:)
          a1(:)=h(2,:)/h(1,:)
          a2(:)=h(3,:)
          a3(:)=h(4,:)
          a5(:)=0.001*h(5,:)
          a6(:)=0.001*h(6,:)
      endif

      tht = 300/t
      pwet=0.1*pv
      pdry=0.1*p-pwet
      xterm=1-tht

      sum = 0.
      do i=1,nlines
          ga = a3(i)*(pdry*tht**(0.8-a4(i)) + 1.1*tht*pwet)
          gasq=ga*ga
          delta=(a5(i) + a6(i)*tht)*p*tht**0.8
          rnuneg = f0(i)-freq
          rnupos = f0(i)+freq
          ff = (ga-rnuneg*delta)/(gasq+rnuneg**2) +  (ga-rnupos*delta)/(gasq+rnupos**2)
          sum = sum + ff*a1(i)*exp(a2(i)*xterm)
!print *,4,i,sum,ff,ga,rnuneg,rnupos,ff*a1(i)*exp(a2(i)*xterm)
      enddo
      if(sum.lt.0) sum=0

!     add nonresonant contribution

!     ga=5.6e-3*(pdry+1.1*pwet)*tht**0.8
      ga=5.6e-3*(pdry+1.1*pwet)*tht**1.5  !modification 1

      zterm=ga*(1.+(freq/ga)**2)
      apterm=1.4e-10*(1-1.2e-5*freq**1.5)*pdry*tht**1.5
      if(apterm.lt.0) apterm=0
      sftot=pdry*freq*tht**2 * (tht*sum + 6.14e-4/zterm + apterm)

      gamoxy=0.1820*freq*sftot
!x    if(freq.gt.37) gamoxy=gamoxy + 0.1820*43.e-10 *pdry**2*tht**3*(freq-37.)**1.7  !prior to 7/17/2015
      if(freq.gt.37) gamoxy=gamoxy + 0.1820*26.e-10 *pdry**2*tht**3*(freq-37.)**1.8  !implemented 7/17/2015.

      return
    end subroutine fdabsoxy_1992_modified
!
!     modified from fdabsoxy_1992_modified in June, 2016 to use newer line data from
!     Tretyakov et al 2005.
!
!     All "modifications" were left in place
!
!     june 11 2015 changed july 17 2015.  oxyopc adjustment above 37 ghz changed slightly. see 'O:\gmi\abs_cal\memo20.txt'

!     this module contains the oxygen absoprtion routine, the vapor absorption routine and fdaray
!     there were used for the april-june 2009 skytemp update.  see 'memo8.txt'

!     ================================================================================================================
!     ========================== modified version of Liebe 1992 oxygen model =========================================
!     ================================================================================================================

!     This is from Atmospheric 60-GHz Oxygen Spectrum:.. Liebe, Rosenkranz, Hufford, 1992
!     coded: June 2009 1992 by f.wentz
!           inputs: t, temperature (k)
!                   p, total pressure (mb)
!                   pv, water vapor pressure (mb)
!                   freq, frequency (ghz)
!           output: gamoxy, oxygen absorption coefficient (db/km)

      subroutine fdabsoxy_tretyakov_modified(p,t,pv,freq, gamoxy)
      implicit none

      integer(4), parameter :: nlines=44
      integer(4), save :: istart
      integer(4) i
      real(4) p,t,pv,freq,gamoxy
      real(4) tht,pwet,pdry,ga,gasq,delta,rnuneg,rnupos,ff,zterm,apterm,sftot,xterm
      real(4) h(6,nlines)
      real(4),save :: f0(nlines),a1(nlines),a2(nlines),a3(nlines),a4(nlines),a5(nlines),a6(nlines)

      real(8) sum

      data istart/1/
      data a4/38*0., 6*0.6/

!          freq            a1            a2           a3           a5           a6

      data h/ &
     50.474213,       0.975e-6,     9.651,       6.690e-3,    0.2566,      0.6850, &
     50.987743,       2.529e-6,     8.653,       7.170e-3,    0.2246,      0.6800, &
     51.503361,       6.193e-6,     7.709,       7.640e-3,    0.1947,      0.6729, &
     52.021427,      14.320e-6,     6.819,       8.110e-3,    0.1667,      0.6640, &
     52.542419,      31.240e-6,     5.983,       8.580e-3,    0.1388,      0.6526, &
     53.066933,      64.290e-6,     5.201,       9.060e-3,    0.1349,      0.6206, &
     53.595776,     124.600e-6,     4.474,       9.550e-3,    0.2227,      0.5085, &
     54.130024,     227.300e-6,     3.800,       9.960e-3,    0.3170,      0.3750, &
     54.671181,     389.700e-6,     3.182,      10.370e-3,    0.3558,      0.2654, &
     55.221382,     627.100e-6,     2.618,      10.890e-3,    0.2560,      0.2952, &
     55.783813,     945.300e-6,     2.109,      11.340e-3,   -0.1172,      0.6135, &
     56.264774,     543.400e-6,     0.014,      17.030e-3,    0.3525,     -0.0978, &
     56.363400,    1331.800e-6,     1.654,      11.890e-3,   -0.2378,      0.6547, &
     56.968212,    1746.600e-6,     1.255,      12.230e-3,   -0.3545,      0.6451, &
     57.612488,    2120.100e-6,     0.910,      12.620e-3,   -0.5416,      0.6056, &
     58.323875,    2363.700e-6,     0.621,      12.950e-3,   -0.1932,      0.0436, &
     58.446587,    1442.100e-6,     0.083,      14.910e-3,    0.6768,     -0.1273, &
     59.164204,    2379.900e-6,     0.387,      13.530e-3,   -0.6561,      0.2309, &
     59.590984,    2090.700e-6,     0.207,      14.080e-3,    0.6957,     -0.0776, &
     60.306057,    2103.400e-6,     0.207,      14.150e-3,   -0.6395,      0.0699, &
     60.434776,    2438.000e-6,     0.386,      13.390e-3,    0.6342,     -0.2825, &
     61.150562,    2479.500e-6,     0.621,      12.920e-3,    0.1014,     -0.0584, &
     61.800159,    2275.900e-6,     0.910,      12.630e-3,    0.5014,     -0.6619, &
     62.411221,    1915.400e-6,     1.255,      12.170e-3,    0.3029,     -0.6759, &
     62.486252,    1503.000e-6,     0.083,      15.130e-3,   -0.4499,      0.0844, &
     62.997986,    1490.200e-6,     1.654,      11.740e-3,    0.1856,     -0.6675, &
     63.568527,    1078.000e-6,     2.108,      11.340e-3,    0.0658,     -0.6139, &
     64.127777,     728.700e-6,     2.617,      10.880e-3,   -0.3036,     -0.2895, &
     64.678909,     461.300e-6,     3.181,      10.380e-3,   -0.3968,     -0.2590, &
     65.224075,     274.000e-6,     3.800,       9.960e-3,   -0.3528,     -0.3680, &
     65.764778,     153.000e-6,     4.473,       9.550e-3,   -0.2548,     -0.5002, &
     66.302094,      80.400e-6,     5.200,       9.060e-3,   -0.1660,     -0.6091, &
     66.836838,      39.800e-6,     5.982,       8.580e-3,   -0.1680,     -0.6393, &
     67.369598,      18.560e-6,     6.818,       8.110e-3,   -0.1956,     -0.6475, &
     67.900871,       8.172e-6,     7.708,       7.640e-3,   -0.2216,     -0.6545, &
     68.431007,       3.397e-6,     8.652,       7.170e-3,   -0.2492,     -0.6600, &
     68.960312,       1.334e-6,     9.650,       6.690e-3,   -0.2773,     -0.6650, &
     118.750336,     940.300e-6,     0.010,      16.640e-3,   -0.0439,      0.0079, &
     368.498260,      67.400e-6,     0.048,      16.400e-3,    0.0000,      0.0000, &
     424.763031,     637.700e-6,     0.044,      16.400e-3,    0.0000,      0.0000, &
     487.249268,     237.400e-6,     0.049,      16.000e-3,    0.0000,      0.0000, &
     715.392883,      98.100e-6,     0.145,      16.000e-3,    0.0000,      0.0000, &
     773.839478,     572.300e-6,     0.141,      16.200e-3,    0.0000,      0.0000, &
     834.145569,     183.100e-6,     0.145,      14.700e-3,    0.0000,      0.0000/

      if(istart.eq.1) then
          istart=0
          f0(:)=h(1,:)
          a1(:)=h(2,:)/h(1,:)
          a2(:)=h(3,:)
          a3(:)=h(4,:)
          a5(:)=0.001*h(5,:)
          a6(:)=0.001*h(6,:)
      endif

      tht = 300/t
      pwet=0.1*pv
      pdry=0.1*p-pwet
      xterm=1-tht

      sum = 0.
      do i=1,nlines
          ga = a3(i)*(pdry*tht**(0.8-a4(i)) + 1.1*tht*pwet)
          gasq=ga*ga
          delta=(a5(i) + a6(i)*tht)*p*tht**0.8
          rnuneg = f0(i)-freq
          rnupos = f0(i)+freq
          ff = (ga-rnuneg*delta)/(gasq+rnuneg**2) +  (ga-rnupos*delta)/(gasq+rnupos**2)
          sum = sum + ff*a1(i)*exp(a2(i)*xterm)
!print *,5,i,sum,ff,ga,rnuneg,rnupos,ff*a1(i)*exp(a2(i)*xterm)
      enddo
      if(sum.lt.0) sum=0

!     add nonresonant contribution

!     ga=5.6e-3*(pdry+1.1*pwet)*tht**0.8
      ga=5.6e-3*(pdry+1.1*pwet)*tht**1.5  !modification 1

      zterm=ga*(1.+(freq/ga)**2)
      apterm=1.4e-10*(1-1.2e-5*freq**1.5)*pdry*tht**1.5
      if(apterm.lt.0) apterm=0
      sftot=pdry*freq*tht**2 * (tht*sum + 6.14e-4/zterm + apterm)

      gamoxy=0.1820*freq*sftot
!x    if(freq.gt.37) gamoxy=gamoxy + 0.1820*43.e-10 *pdry**2*tht**3*(freq-37.)**1.7  !prior to 7/17/2015
      if(freq.gt.37) gamoxy=gamoxy + 0.1820*26.e-10 *pdry**2*tht**3*(freq-37.)**1.8  !implemented 7/17/2015.

      return
    end subroutine fdabsoxy_tretyakov_modified

!     ================================================================================================================
!     ========================== modified version of Rosenkranz water vapor model ====================================
!     ================================================================================================================


! purpose- compute absorption coef in atmosphere due to water vapor
!
!  calling sequence parameters-
!    specifications
!      name    units    i/o  descripton            valid range
!      t       kelvin    i   temperature
!      p       millibar  i   pressure              .1 to 1000
!      f       ghz       i   frequency             0 to 800
!      gamh2o  db/km     o   absorption coefficient
!
!   references-
!    p.w. rosenkranz, radio science v.33, pp.919-928 (1998).
!
!   line intensities selection threshold=
!     half of continuum absorption at 1000 mb.
!   widths measured at 22,183,380 ghz, others calculated.
!     a.bauer et al.asa workshop (sept. 1989) (380ghz).
!
!   revision history-
!    date- oct.6, 1988  p.w.rosenkranz - eqs as publ. in 1993.
!          oct.4, 1995  pwr- use clough's definition of local line
!                   contribution,  hitran intensities, add 7 lines.
!          oct. 24, 95  pwr -add 1 line.
!          july 7, 97   pwr -separate coeff. for self-broadening,
!                       revised continuum.
!          dec. 11, 98  pwr - added comments

!     the routine is a modified version of abh2o_rk_reformat.
!     this routine has been modified in the following three ways (see 'memo8.txt')
!     1.  b1(1)=1.01*b1(1)  :22 ghz line strength increase slightly
!     2.  22 ghz line shape below 22 ghz has been modified
!     3.  foreign and self broadening continuum has been adjusted
!     these modification were done June 22 2009

      subroutine abh2o_rk_modified(p,t,pv,freq,  gamh2o)
      implicit none

      integer(4), parameter :: nlines=15

      integer(4),save :: istart
      integer(4) i
      real(4) t,p,freq
      real(4),save :: b1(nlines),b2(nlines),b3(nlines),f0(nlines),b4(nlines),b5(nlines),b6(nlines)
      real(4) pv,s,base,gamh2o
      real(4) tht,pwet,pdry,ga,gasq,sftot,xterm,rnuneg,rnupos
      real(8) sum

      real(4) chi,chisq,freqsq,f0sq,u

      data istart/1/

!     line frequencies:
      data f0/22.2351, 183.3101, 321.2256, 325.1529, 380.1974, 439.1508, &
              443.0183, 448.0011, 470.8890, 474.6891, 488.4911, 556.9360, &
              620.7008, 752.0332, 916.1712/
!     line intensities at 300k:
      data b1/ .1310e-13, .2273e-11, .8036e-13, .2694e-11, .2438e-10, &
               .2179e-11, .4624e-12, .2562e-10, .8369e-12, .3263e-11, .6659e-12, &
               .1531e-08, .1707e-10, .1011e-08, .4227e-10/
!     t coeff. of intensities:
      data b2/ 2.144, .668, 6.179, 1.541, 1.048, 3.595, 5.048, 1.405, 3.597, 2.379, 2.852, .159, 2.391, .396, 1.441/
!     air-broadened width parameters at 300k:
      data b3/.0281, .0281, .023, .0278, .0287, .021, .0186, .0263, .0215, .0236, .026, .0321, .0244, .0306, .0267/
!     self-broadened width parameters at 300k:
      data b5/.1349, .1491, .108, .135, .1541, .090, .0788, .1275, .0983, .1095, .1313, .1320, .1140, .1253, .1275/
!     t-exponent of air-broadening:
      data b4 /.69, .64, .67, .68, .54, .63, .60, .66, .66, .65, .69, .69, .71, .68, .70/
!     t-exponent of self-broadening:
      data b6/.61, .85, .54, .74, .89, .52, .50, .67, .65, .64, .72, 1.0, .68, .84, .78/
!
      if(istart.eq.1) then
          istart=0
          b1=1.8281089E+14*b1/f0**2
          b5=b5/b3  !convert b5 to Leibe notation
          b1(1)=1.01*b1(1)  !modification 1
      endif

      if(pv.le.0.) then
          gamh2o=0
          return
      endif


      pwet=0.1*pv
      pdry=0.1*p-pwet
      tht = 300./t
      xterm=1-tht
      freqsq=freq*freq

      sum = 0.
      do i=1,nlines
          f0sq=f0(i)*f0(i)
          ga=b3(i)*(pdry*tht**b4(i) + b5(i)*pwet*tht**b6(i))
          gasq = ga*ga
          s = b1(i)*exp(b2(i)*xterm)
          rnuneg = f0(i)-freq
          rnupos = f0(i)+freq
          base = ga/(562500. + gasq)  !use clough's definition of local line contribution

          if(i.ne.1) then
              if(abs(rnuneg).lt.750) sum = sum + s*(ga/(gasq + rnuneg**2) - base)
              if(abs(rnupos).lt.750) sum = sum + s*(ga/(gasq + rnupos**2) - base)

          else
              chi=0

              if(freq.lt.19) then
                  u=abs(freq-19.)/16.5
                  if(u.lt.0) u=0
                  if(u.gt.1) u=1
                  chi=ga*u*u*(3-2*u)  !modification 2
              endif

              chisq=chi*chi
              sum=sum +     s*2*((ga-chi)*freqsq + (ga+chi)*(f0sq+gasq-chisq))/((freqsq-f0sq-gasq+chisq)**2 + 4*freqsq*gasq)
          endif

      enddo
      if(sum.lt.0) sum=0

!x    sftot=pwet*freq*tht**3.5*(sum +     1.2957246e-6*pdry/tht**0.5 +                    4.2952193e-5*pwet*tht**4)
      sftot=pwet*freq*tht**3.5*(sum + 1.1*1.2957246e-6*pdry/tht**0.5 + 0.425*(freq**0.10)*4.2952193e-5*pwet*tht**4) !modification 3

      gamh2o=0.1820*freq*sftot
      return
    end subroutine abh2o_rk_modified

end module atmos_abs_routines

