      program cooling
      real t, T_coffee, T_room, r, delta_t, tmax, temps_black(23), temps_milk(23)
      integer nshow, counter
      counter = 0
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

      subroutine readData(temps_black, temps_milk
      	open(unit=10,file='coffee_expt.dat')
      	DO I=1,23
        	read(10,*) temps_black(I), temps_milk(I)
        	write(*,*) temps_black(I), temps_milk(I))
        enddo
      end

