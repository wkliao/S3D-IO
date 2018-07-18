!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      !----< solve_driver >--------------------------------------------
      subroutine solve_driver
          ! routine drives the solution of the governing equations
          use mpi
          use topology_m,     only: gcomm
          use runtime_m,      only: i_time, i_time_end, tstep, time
          use runtime_m,      only: time_save, time_save_inc, time_ref
          use io_profiling_m, only: set_io_hints, print_io_performance
          implicit none

          integer err
          double precision total_time, t

          total_time = 0.0

          ! set MPI-IO and PnetCDF hints
          call set_io_hints(1)

          ! initialize field
          ! (allocate variables and read restart file if restart)
          call initialize_field

          ! loop over num_writes time steps
          i_time = 0
          time_ref = 1.29889001100892417E-005
          do while (i_time < i_time_end)
             i_time = i_time + 1

             ! assign random numbers to all variables, excluded from timing
             call random_set

             call MPI_Barrier(gcomm,err)
             t = MPI_Wtime()

             call write_savefile ! checkpoint write

             total_time = total_time + MPI_Wtime() - t

             ! advance time step
             tstep = 1e-6/time_ref
             time=time+tstep

             if (time*time_ref .GT. time_save) then  !increment save time
                 time_save=time_save+time_save_inc
             endif
          enddo

          ! deallocate MPI info object
          call set_io_hints(-1)

          ! report I/O timing results
          call print_io_performance(total_time)

      end subroutine solve_driver
