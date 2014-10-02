      program pg_test
	  integer pgopen, i
	  real xmin, xmax, ymin, ymax
	  if (pgopen('/xwind') .le. 0) stop
	  call pgenv(xmin, xmax, ymin, ymax, 0, 0)

	  xmin = -5.0
	  xmax = 5.0
	  ymin = -5.0
	  ymax = 5.0

	
      call pgclos
      end