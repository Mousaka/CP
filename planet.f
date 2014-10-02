c2345678901234567890
      program test2
      integer pgopen
      real circle1_x, circle1_y, circle1_x2, circle1_y2, angle, delta 
      real cricle2_x, circle2_y, circle2_x2, circle2_y2, pi, dist, dist2, angle2, delta2
      real moon_x, moon_y, moon_x2, moon_y2, moon_dist, moon_angle, delta3

      if (pgopen('/xwin') .le. 0) stop
c     Pg paper, the size of picture. arg2 is reatio between x and y axis
      call pgpap (6.0, 0.75)
      call pgenv (0.0, 1.0, 0.0, 1.0, 1, 1)
      call pglab ('x-axis', 'y-axis', 'My test PGPLOT graph')
c     fillin some stuff, 2means empty. empty circle. outline shape

      call pgsci(7)
      call pgcirc (0.5, 0.5, 0.1)
      call pgsfs (2)
      pi = 3.14159268
      angle = 0.0
      angle2 = 0.0
      moon_angle = 0.0
      delta = pi/110.0
      delta2 = pi/200.0
      delta3 = delta2 * 12
      dist = 0.2
      dist2 = 0.4
      moon_dist = 0.1

      circle1_x = dist * cos(angle) + 0.5
      circle1_y = dist * sin(angle) + 0.5

      circle2_x = dist2 * cos(angle) + 0.5
      circle2_y = dist2 * sin(angle) + 0.5
      moon_x = moon_dist * cos(moon_angle) + circle2_x
      moon_y = moon_dist * sin(moon_angle) + circle2_y

      call pgcirc (circle1_x, circle1_y, 0.02)
      call pgsci(2)
      call pgcirc (circle2_x, circle2_y, 0.04)

 1000 continue
      angle = angle + delta
      angle2 = angle2 + delta2
      moon_angle = moon_angle + delta3
      circle1_x2 = dist * cos(angle) + 0.5
      circle1_y2 = dist * sin(angle) + 0.5

      circle2_x2 = dist2 * cos(angle2+10) + 0.5
      circle2_y2 = dist2 * sin(angle2+10) + 0.5
      moon_x2 = moon_dist * cos(moon_angle) + circle2_x2
      moon_y2 = moon_dist * sin(moon_angle) + circle2_y2
c     Everything inbetween pgbbuf and pgebuf will be done at once.
      call pgbbuf
c     Set color index 0 (black) for erasing old whie one
      call pgsci(0)
      call pgcirc (circle1_x, circle1_y, 0.02)
      call pgcirc (circle2_x, circle2_y, 0.04)
      call pgcirc (moon_x, moon_y, 0.02)
c     now white again
      call pgsci(4)
c     draw new circle at new position     
      call pgcirc (circle1_x2, circle1_y2, 0.02)

      call pgsci(3)
      call pgcirc (circle2_x2, circle2_y2, 0.04)
      call pgsci(1)
      call pgcirc (moon_x2, moon_y2, 0.02)

      call pgebuf

      circle1_x = circle1_x2
      circle1_y = circle1_y2

      circle2_x = circle2_x2
      circle2_y = circle2_y2
      moon_x = moon_x2
      moon_y = moon_y2

      do i = 1, 10000000
       idummy = i**3
      enddo

      goto 1000

      call pgclos
c     write (*,*) 'Hello, world!'
      end
