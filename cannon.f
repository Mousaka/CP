      program cannon
      implicit none
      real x,vx,ax, y,vy,ay, vx_ini,vy_ini, x_range,y_range,delta_t
      integer pgopen,isymbol,i_again, p_x, p_y

 100  call input_n_init(vx_ini,vy_ini,delta_t)

      if (pgopen('/xwin').le.0) stop
      call pgpap(5.0, 1.0)

      isymbol = 20
c     isymbol = 21

c      read (*,*) x_range, y_range
      x = 0.0
      y = 1.0
      vx = vx_ini
      vy = vy_ini
      call predict_size(x,vx,ax,y,vy,ay,delta_t, p_x, p_y)
      x_range = p_x + 1
      y_range = p_y + 1
      call pgeras
      call pgenv(0.0,x_range,0.0,y_range,0,0)



      do while (y.gt.0.0)
        call fly (x,vx,ax,y,vy,ay,delta_t)
        call pgpt (1,x,y,isymbol)
      end do

 102  write(*,*) 'Again ? (1=Yes;0=No)'
      read(*,*) i_again
      if (i_again.eq.1) goto 100
      if (i_again.eq.0) stop
      goto 102

      call pgclos

      end



      subroutine input_n_init(vx,vy,delta_t)
      implicit none
      real v,vx,vy,angle,ke,delta_t
      integer i_gun_powder
      write(*,*) 'How maqny packs of gun powder (integer) ?'
      read (*,*) i_gun_powder
      write (*,*) 'What is the angle of shooting ?'
      read (*,*) angle
      write(*,*) 'What is the small time-interval delta_t ?'
      read (*,*) delta_t
      angle = angle * 3.14159268/180
      ke = 10.0 * i_gun_powder
      v = sqrt(2*ke)
      vx = v*cos(angle)
      vy = v*sin(angle)
      end



      subroutine fly(x,vx,ax,y,vy,ay,delta_t)
      implicit none
      real x,vx,ax,y,vy,ay,delta_t
      real v_old_x,v_new_x,v_old_y,v_new_y

      ax = 0.0
      ay = -9.8

      v_old_x = vx
      v_old_y = vy

      v_new_x = v_old_x + ax*delta_t
      x = x + 0.5*(v_new_x + v_old_x)*delta_t

      v_new_y = v_old_y + ay*delta_t
      y = y + 0.5*(v_new_y + v_old_y)*delta_t

      vx = v_new_x
      vy = v_new_y

      end

      subroutine predict_size(x,vx,ax,y,vy,ay,delta_t, p_x, p_y)
      implicit none
      real x,vx,ax,y,vy,ay,delta_t
      real v_old_x,v_new_x,v_old_y,v_new_y
      real tx,tvx,tax,ty,tvy,tay,tdelta_t
      real tv_old_x,tv_new_x,tv_old_y,tv_new_y
      real max_y
      integer p_x, p_y

      tx = x
      tvx = vx
      tax = ax
      ty = y
      tvy = vy
      tay = ay
      tdelta_t = delta_t
      tv_old_x = v_old_x
      tv_new_x = v_new_x
      tv_old_y = v_old_y
      tv_new_y = v_new_y
      max_y = 0
      do while (y.gt.0.0)
        call fly (x,vx,ax,y,vy,ay,delta_t)
        write(*,*) y
        if(y.gt.max_y) then
            max_y = y
            write(*,*) "maxy=", max_y
        endif
      end do
      write(*,*) "maxx= ", x
      write(*,*) "maxy= ", max_y
      p_x = NINT(x)
      p_y = NINT(max_y)
      write(*,*) "nintx", p_x
      write(*,*) "ninty", p_y
      max_y = 0
      x = tx
      vx = tvx
      ax = tax
      y = ty
      vy = tvy
      ay = tay
      delta_t = tdelta_t
      v_old_x = tv_old_x
      v_new_x = tv_new_x
      v_old_y = tv_old_y
      v_new_y = tv_new_y

      end