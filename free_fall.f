      program free_fall
      implicit none

      real radius, g, h, delta_h, v1, v2
      real percentage
      character(len=50) :: string
      integer pgopen, i_again
      if (pgopen('/xwin').le.0) stop
      call pgpap(5.0, 1.0)
      call pgeras
      call pgenv(0.0,100000.0,0.0,1300.0,0.0,0)
      call pglab('Height (m)','Velocity (m/s**2)','Method comparison')
      delta_h = 0.1
      v1 = 0
      v2 = 0
      h = 0
      do while(h.le.100000)
     	h = h + delta_h
      	call first_line(v1, h, delta_h)
      	call second_line(v2, h,  delta_h)
c      	write(*,*) "h, vX§:",h, v1
c      	write(*,*) "h, v:", h, v
		percentage = ((v1-v2) / v1) * 100
c		write(*,*) "percentage: " , percentage
		if(percentage.le.-1) then
			write(*,*) "Difference is 1% when h = ", h
			write(string, *) "h=", h, "m"
	  call pgptext(6000,v1,0.0,0.0,"1 percent difference when")
	  call pgptext(6000,v1-100,0.0,0.0, string)
			call pgmove(h, 1200)
			call pgdraw(h, 1)
			goto 100
		end if
		call pgsci(1)
      	call pgpt(1,h,v1,20)
      	call pgsci(6)
      	call pgpt(1,h,v2,20)
      end do
100   read(*,*) i_again

      end

      subroutine second_line(v1, h, delta_h)
      implicit none
      real  g, h, delta_h, v1
      g = 9.8
      
      v1 = sqrt(2 * g * h)
      

      end

      subroutine first_line(v2, h, delta_h)
      implicit none
      real  a, h, delta_h, v2, G, M, R
      R = 6.37*1.d6
      M = 5.972*1.d24
      G = 6.67*1.d-11
      a = (G * M) / (R + h)**2
      v2 = sqrt(2 * a * h)
      

      end