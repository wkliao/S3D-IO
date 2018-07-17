!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      !----< random_set_seed >-----------------------------------------
      subroutine random_set_seed
          use topology_m, only : myid
          implicit none

          integer i, seed_size
          integer, DIMENSION(:), ALLOCATABLE :: seed

          call random_seed(SIZE=seed_size)
          ALLOCATE(seed(seed_size))
          i = 1
          seed = myid + 37 * (/ (i - 1, i = 1, seed_size) /)
          call random_seed(PUT=seed)
      end subroutine random_set_seed

      !----< random_set >----------------------------------------------
      subroutine random_set
          use topology_m,  only : myid
          use variables_m, only : temp, pressure, yspecies, u
          use param_m,     only : nx, ny, nz, nsc
          implicit none

          call RANDOM_NUMBER(yspecies(1:nx,1:ny,1:nz,1:nsc+1))
          call RANDOM_NUMBER(    temp(1:nx,1:ny,1:nz))
          call RANDOM_NUMBER(pressure(1:nx,1:ny,1:nz))
          call RANDOM_NUMBER(       u(1:nx,1:ny,1:nz,1:3))
      end subroutine random_set
