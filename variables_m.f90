!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      module variables_m
      ! module for variables
      implicit none

      ! primitive variables
      double precision, allocatable :: yspecies(:,:,:,:) !mass fractions for ALL species
      double precision, allocatable ::        u(:,:,:,:) !velocity vector (non-dimensional)
      double precision, allocatable :: pressure(:,:,:)   !pressure (non-dimensional)
      double precision, allocatable ::     temp(:,:,:)   !temperature (non-dimensional)

      contains

      !----< allocate_variables_arrays() >-----------------------------
      subroutine allocate_variables_arrays(flag)
         ! allocate variables arrays
         use param_m, only : nx, ny, nz, nsc
         implicit none
         integer flag

         if (flag .EQ. 1) then
            allocate(yspecies(nx,ny,nz,nsc+1))
            allocate(       u(nx,ny,nz,3))
            allocate(pressure(nx,ny,nz))
            allocate(    temp(nx,ny,nz))
         elseif (flag .EQ. -1) then
            deallocate(yspecies)
            deallocate(u)
            deallocate(pressure)
            deallocate(temp)
         endif
      end subroutine allocate_variables_arrays

      end module variables_m

