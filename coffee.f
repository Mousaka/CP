      program cooling
      implicit none
      real r,time,t_max,T,Ts,T_ini,delta_t, getR
      integer pgopen,i,n_steps,i_again, addedMilk, zipAble

 100  write(*,*) "Cooling rate and surroundind temperature = 17:"
      read(*,*) r  !, Ts
      Ts = 17
      write(*,*)     'Time duration and initial temperature:'
      !read(*,*) t_max, T_ini
      t_max = 20
      T_ini = 90.0
      
      if(pgopen('/xwin').le.0) stop
      call pgpap (5.0,0.75)

 102  write (*,*) 'Please type in the small time interval delta_t :'
      delta_t = 0.001 !read (*,*) delta_t
      time = 0
      T = T_ini
      call pgeras
      call pgenv(0.0,t_max,70.0,T_ini+5,0,0)
      call pglab('Time (min)','Temperature (C)','Cooling Time')
      !call readData()  not needed now
      !adding milk first
      zipAble = 0
      n_steps = int(t_max/delta_t)
      call pgsci(2)
      do i=1,n_steps
        call Euler(time, T, Ts, r,delta_t)
        if (zipAble.eq.0.and.T.le.75.0) then
          call pgsci(3)
          zipAble = 1
          write(*,*) "Milk at start makes coffee sipable after ",time
        end if
        !write(*,*) "Plot plot"
        call pgpt (1,time,T,-1)
        if (i.eq.2) then
          call pgmove(time, T)
          T = T - 5
          call pgsci(4)
          call pgdraw(time, T)
          call pgsci(2)
        end if
      end do

      !Adding milk at the end
      addedMilk = 0;
      time = 0
      zipAble = 0
      n_steps = int(t_max/delta_t)
      T = T_ini
      call pgsci(6)
      do i=1,n_steps
        call Euler(time, T, Ts, r,delta_t)
        if (addedMilk.eq.0.and.T.le.80.0) then
          call pgmove(time,T)
          addedMilk = 1
          T = T-5
          call pgsci(1)
          call pgdraw(time, T)
          call pgsci(6)
        end if
        if (zipAble.eq.0.and.T.le.75.0) then
          call pgsci(7)
          write(*,*) "Milk at end makes coffee sipable after ", time
          zipAble = 1
        end if
        !write(*,*) "Plot plot"
        call pgpt (1,time,T,-1)
      end do
      call pgsci(1)

      write (*,*) 'Again ? (1=YES;0=NO)'
      read (*,*) i_again
      if (i_again.eq.1) goto 100
      call pgclos

      end

      subroutine Euler(t, T_coffee, T_room, r, delta_t)
        change = -r*(T_coffee - T_room)*delta_t
        T_coffee = T_coffee + change
        t = t + delta_t
      end

      subroutine readData()
      	real temps_black(24)
      	real temps_milk(24)
      	real time_stamps(24)
      	open(unit=10,file='coffee_expt.dat')
      	DO I=1,24
        	read(10,*) time_stamps(I), temps_milk(I), temps_black(I)
        	!write(*,*) time_stamps(I), temps_milk(I), temps_black(I)
        enddo
        close(10)
        call pgsci(5)
        call pgline(23, time_stamps, temps_milk)
        call pgpt(23, time_stamps, temps_milk, 2)
        call pgsci(8)
        call pgline(23, time_stamps, temps_black)
        call pgpt(23, time_stamps, temps_black, 2)
        return
      end

      function getR(t_stamps, temps_coffee)
      real temps_coffee(23)
      getR = 1
      end
