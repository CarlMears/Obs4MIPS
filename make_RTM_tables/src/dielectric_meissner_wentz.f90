module dielectric

      implicit none

      contains
    
subroutine dielectric_meissner_wentz(sst_in,s,   e0s,e1s,e2s,n1s,n2s,sig)

      implicit none
      real(4), intent(in) :: sst_in,s
      real(4), intent(out) :: e0s,e1s,e2s,n1s,n2s,sig
 
      real(4), dimension(11), parameter :: &
      x=(/ 5.7230e+00, 2.2379e-02, -7.1237e-04, 5.0478e+00, -7.0315e-02, 6.0059e-04, 3.6143e+00,  &
           2.8841e-02, 1.3652e-01,  1.4825e-03, 2.4166e-04 /)
 
 
      real(4), dimension(13), parameter ::  &
      z=(/ -3.56417e-03,  4.74868e-06,  1.15574e-05,  2.39357e-03, -3.13530e-05,  &
            2.52477e-07, -6.28908e-03,  1.76032e-04, -9.22144e-05, -1.99723e-02,  &
            1.81176e-04, -2.04265e-03,  1.57883e-004  /)  ! 2004

 
      real(4) :: e0,e1,e2,n1,n2
      real(4) :: a0,a1,a2,b1,b2
      real(4) :: sig35,r15,rtr15,alpha0,alpha1

      real(4) sst,sst2,sst3,sst4,s2

      sst=sst_in
!    if(sst.lt.-25) sst=-25  !pretects against n1 and n2 going zero for very cold water
      if(sst.lt.-30.16) sst=-30.16  !pretects against n1 and n2 going zero for very cold water

      sst2=sst*sst
      sst3=sst2*sst
      sst4=sst3*sst

      s2=s*s
 
!     pure water
      e0    = (3.70886e4 - 8.2168e1*sst)/(4.21854e2 + sst) ! stogryn et al.
      e1    = x(1) + x(2)*sst + x(3)*sst2
      n1    = (45.00 + sst)/(x(4) + x(5)*sst + x(6)*sst2)
      e2    = x(7) + x(8)*sst
      n2    = (45.00 + sst)/(x(9) + x(10)*sst + x(11)*sst2)
 
!     saline water
!     conductivity [s/m] taken from stogryn et al.
      sig35 = 2.903602 + 8.60700e-2*sst + 4.738817e-4*sst2 - 2.9910e-6*sst3 + 4.3047e-9*sst4
      r15   = s*(37.5109+5.45216*s+1.4409e-2*s2)/(1004.75+182.283*s+s2)
 
      alpha0 = (6.9431+3.2841*s-9.9486e-2*s2)/(84.850+69.024*s+s2)
      alpha1 = 49.843 - 0.2276*s + 0.198e-2*s2
      rtr15 = 1.0 + (sst-15.0)*alpha0/(alpha1+sst)
 
      sig = sig35*r15*rtr15
 
!     permittivity
      a0  = exp(z(1)*s + z(2)*s2 + z(3)*s*sst)
      e0s = a0*e0
      b1  = 1.0 + s*(z(4) + z(5)*sst + z(6)*sst2)
      n1s = n1*b1
      a1  = exp(z(7)*s + z(8)*s2 + z(9)*s*sst)
      e1s = e1*a1
      b2 = 1.0 + s*(z(10) + z(11)*sst)
      n2s = n2*b2
      a2 = 1.0  + s*(z(12) + z(13)*sst)
      e2s = e2*a2
 
      return
   end subroutine  dielectric_meissner_wentz
end module dielectric