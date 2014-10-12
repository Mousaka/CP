      program Euler

      real Y(1001), X(1001), DeltaX
      integer I, N, PGOPEN
      if(PGOPEN('/xwin').le.0) STOP
      call PGENV(0.0, 10.0, 0.0, 100.0, 0, 0)
      write(*,*) "What is mesh size N ? (N less than 1000)"
      read(*,*) N
      DeltaX = (10.0 - 0.0) / N
      Y(1) = 0.0
      DO I=1, N
          X(I)= 0.0 + (I-1) * DeltaX
          Y(I+1) = Y(I) + (2*X(I))*DeltaX
      ENDDO
      open(unit=10,file='DATA.TXT')
      DO I=1,N
         WRITE(10,*) Y(I), X(I)
      enddo
      close(10)
      call PGLINE(N, X, Y)
      call PGCLOS
      end
