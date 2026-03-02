c	input:
c     oxygen absorption from rosenkranz
c     freq  frequency [in ghz]
c     p      pressure [in h pa]
c     t      temperature [in k]
c     pv     water vapor pressure  [in hpa]
c
c     output:	
c     av          water vapor absorption coefficients [neper/km]
c     ao          oxygen absortption coefficient		[neper/km]

      subroutine fdabscoeff(freq,p,t,pv, av,ao)
	implicit none

      real(4), parameter :: xnaper=0.2302585094 !convert db/km to naper/km

      real(4) freq,p,t,pv
	real(4)	av,ao
	real(4) gamoxy,gamh2o

	call fdabsoxy_1992_modified(p,t,pv,freq, gamoxy)  !gamoxy is db/km
      call abh2o_rk_modified(     p,t,pv,freq, gamh2o)  !gamh2o is db/km

	ao=xnaper*gamoxy
	av=xnaper*gamh2o 
      return
      end

