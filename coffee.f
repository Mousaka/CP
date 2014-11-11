      program cooling
      implicit none
      real r,time,t_max,T,Ts,T_ini,delta_t, getR
      real r_irish, r_black, time1, time2
      real time_diff
      integer pgopen,i,n_steps,i_again, addedMilk, zipAble
      integer black_greater
      if(pgopen('/xwin').le.0) stop
      call pgpap (8.0,0.8)
      call pgask(.false.)
 100  write(*,*) "Enter cooling rate coffee, temperature = 17:"
      read(*,*) r_black  !, Ts
      write(*,*) "Enter irish cooling rate guess for it to be" 
      write(*,*) "more effective to add it first"
      read(*,*) r_irish
      Ts = 17
      write(*,*)     'Time duration and initial temperature:'
      !read(*,*) t_max, T_ini
      t_max = 20
      T_ini = 90.0
 102  write (*,*) 'Please type in the small time interval delta_t :'
      delta_t = 0.001 !read (*,*) delta_t
 103  time = 0
      T = T_ini      
      call pgeras
      call pgenv(0.0,t_max,70.0,T_ini+5,0,0)
      call pglab('Time (min)','Temperature (C)','Cooling Time')

      black_greater = 1
      if(r_black.le.r_irish) black_greater = -1
      !call readData()  not needed now
      !adding milk first
      r=r_black
      zipAble = 0
      n_steps = int(t_max/delta_t)
      call pgsci(2)
      do i=1,n_steps
        call Euler(time, T, Ts, r,delta_t)
        if (zipAble.eq.0.and.T.le.75.0) then
          call pgsci(3)
          zipAble = 1
          !write(*,*) "Whiskey at start makes coffee sipable after ",time
          time1 = time
        end if
        !write(*,*) "Plot plot"
        call pgpt (1,time,T,-1)
        if (i.eq.2) then
          call pgmove(time, T)
          T = T - 5
          r = r_irish
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
      r = r_black
      T = T_ini
      call pgsci(6)
      do i=1,n_steps
        call Euler(time, T, Ts, r,delta_t)
        if (addedMilk.eq.0.and.T.le.80.0) then
          call pgmove(time,T)
          addedMilk = 1
          T = T-5
          r = r_irish
          call pgsci(1)
          call pgdraw(time, T)
          call pgsci(6)
        end if
        if (zipAble.eq.0.and.T.le.75.0) then
          call pgsci(7)
          !write(*,*) "Milk at end makes coffee sipable after ", time
          time2 = time
          zipAble = 1
        end if
        !write(*,*) "Plot plot"
        call pgpt (1,time,T,-1)
      end do
      call pgsci(1)
      time_diff = time1 - time2
      if(abs(time_diff).le.0.1) then
        write(*,*) "Adding whiskey first better when r_irish=", r_irish
      else
        if(time1.le.time2) then
          r_irish = r_irish - 0.0001
        else
          r_irish = r_irish + 0.0001
        end if
        call sleep_time(2)
        goto 103
      end if
      write (*,*) 'Again ? (1=YES;0=NO)'
      read (*,*) i_again
      if (i_again.eq.1) goto 100
      call pgclos

      end program

      subroutine Euler(t, T_coffee, T_room, r, delta_t)
        change = -r*(T_coffee - T_room)*delta_t
        T_coffee = T_coffee + change
        t = t + delta_t
      end

      subroutine sleep_time(st)
      integer st
      do i = 1, (10000000 * st)
       idummy = i**3
      enddo
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
