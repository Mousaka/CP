      program pg_test
      integer pgopen, i
      real xmin, xmax, ymin, ymax, x(100), y(100)
      if (pgopen('/xwind') .le. 0) stop

      xmin =-1.0
      xmax = 1.0
      ymin = -2.0
      ymax = 2.0

      call pgenv(xmin, xmax, ymin, ymax, 0, 1)

      do i=1,100
      	x(i) = 0.1*i + xmin
        y(i) = x(i)*x(i)
      enddo


      call pgline(100, x, y)
      call pgclos
      end