    　program rocket
      implicit none
      real*8 :: V,V0,m,m0,k1,k2,k3,k4,h,u
      integer :: n,j
      
      !No gravity & air resistance

      !V: Speed of rocket's main body
      !m: Mass of rocket's main body
      !u: Relative speed
      !h: Step size
 
      !initial value
      !nondimensionalization
      V=3.95d0 
      m=0.8d0 
      
      !Runge Kutta
      read(*,*) h!,u
      !write(*,*) "m,V"

      do j=0,50

       k1=h*(-1)*V
       k2=h*(-1)*(V+k1/2)
       k3=h*(-1)*(V+k2/2)
       k4=h*(-1)*(V+k3)

       m=m-h
       V=V+(k1+2d0*k2+2d0*k3+k4)/6d0
        
        if (m>=0.10d0) then
         write(*,*) m,V
        end if

      end do 
       
      stop
      end
