      program cooling
      implicit none
      real t, T_coffee, T_room, r, delta_t, tmax, time_stamps(24)
      real temps_black(24), temps_milk(24)
      integer nshow, counter
      counter = 0
      call readData(time_stamps, temps_black, temps_milk)
      call Euler(t, T_coffee, T_room, r, delta_t, tmax, nshow)
      end

      subroutine Euler(t, T_coffee, T_room, r, delta_t, tmax, nshow)
      	change = -r*(T_coffee - T_room)*delta_t
      	T_coffee = T_coffee + change
      	t = t + delta_t
      end

      subroutine initial(t, T_init, T_room, r, delta_t, tmax, nshow)
      	t = 0
      	T_init = 82.3
      	T_room = 17
      	r = 1
      	delta_t = 0.1
      	tmax = 5 ! in minutes
      	tshow = 2 !time between output of data
      	nshow = NINT(tshow/delta_t) !rounds into integer
      end

      subroutine readData(time_s, temps_b, temps_m)
      	real temps_b(24)
      	real temps_m(24)
      	real time_s(24)
      	open(unit=10,file='coffee_expt.dat')
      	DO I=1,24
        	read(10,*) time_s(I), temps_m(I), temps_b(I)
        	write(*,*) time_s(I), temps_m(I), temps_b(I)
        enddo
        close(10)
      end
