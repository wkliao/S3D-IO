!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      module param_m
      implicit none

      integer nx_g   ! global number of grid points in x-direction
      integer ny_g   ! global number of grid points in y-direction
      integer nz_g   ! global number of grid points in z-direction
      integer nx     ! local number of grid points in x-direction
      integer ny     ! local number of grid points in y-direction
      integer nz     ! local number of grid points in z-direction
      integer npx    ! number of processors in x-direction
      integer npy    ! number of processors in y-direction
      integer npz    ! number of processors in z-direction
      integer nsc    ! number of chemical species (excluding N2)
      integer n_spec ! number of chemical species including N2

      contains

      !----< initialize_param() ---------------------------------------
      subroutine initialize_param(myid,gcomm)
         ! sets various parameters
         use mpi
         use runtime_m, only: i_time_end, run_title
         implicit none

         ! declarations passed in
         integer myid, gcomm

         ! local declarations
         integer err, iflag

         i_time_end = 5   ! number of checkpoints (also number of output files)
         run_title = 'pressure_wave_test'  ! prefix of output file names

         ! set iflag
         iflag=0
         if (myid .EQ. 0) then
            ! error trapping for number of grid points in x-direction
            if (mod(nx_g,npx) .NE. 0 ) then
               print*, ' Grid Pts in X dimension ',  nx_g,  &
                       ' are not exactly divisible by ', npx ,  &
                       ' number of PEs in the X dimension '
               iflag=1
            endif

            ! error trapping for number of grid points in x-direction
            if (mod(ny_g,npy) .NE. 0 ) then
               print*, ' Grid Pts in Y dimension ',  ny_g,  &
                       ' are not exactly divisible by ', npy ,  &
                       ' number of PEs in the Y dimension '
               iflag=1
            endif

            ! error trapping for number of grid points in x-direction
            if (mod(nz_g,npz) .NE. 0 ) then
               print*, ' Grid Pts in Z dimension ',  nz_g,  &
                       ' are not exactly divisible by ', npz ,  &
                       ' number of PEs in the Z dimension '
              iflag=1
            endif

            ! set local number of grid points
            nx = nx_g / npx
            ny = ny_g / npy
            nz = nz_g / npz

            ! set chemistry parameters based on chemkin initialization
            n_spec=11
            nsc=n_spec-1  !set number of chemical species for DNS purposes
         endif

         ! check status of error
         call MPI_Bcast(iflag,1,MPI_INTEGER,0,gcomm,err)
         if (iflag .EQ. 1) then
             call MPI_Comm_free(gcomm,err)
             call MPI_Finalize(err)
             stop
         endif

         ! broadcast local grid parameters
         call MPI_Bcast(nx, 1, MPI_INTEGER, 0, gcomm, err)
         call MPI_Bcast(ny, 1, MPI_INTEGER, 0, gcomm, err)
         call MPI_Bcast(nz, 1, MPI_INTEGER, 0, gcomm, err)

         ! broadcast chemistry parameters
         call MPI_Bcast(nsc   , 1, MPI_INTEGER, 0, gcomm, err)
         call MPI_Bcast(n_spec, 1, MPI_INTEGER, 0, gcomm, err)

      end subroutine initialize_param

      end module param_m
