!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      !----< write_savefile >------------------------------------------
      subroutine write_savefile
          ! writes data file for post-processing and restarting
          use mpi
          use runtime_m,      only: time, time_ref, run_title
          use io_profiling_m, only: dir_path
          use pnetcdf_m,      only: pnetcdf_write
          implicit none

          ! local variables
          character*10  time_ext
          character*256 filename

          ! set file extension string
          write(time_ext,'(1pe10.3)') time*time_ref

          ! set file name
          filename = trim(dir_path)//'/'//trim(run_title)//'.'// &
                     trim(adjustl(time_ext))//'.field.nc'

          ! call PnetCDF APIs to write data to file
          call pnetcdf_write(filename)

      end subroutine write_savefile

      !----< read_savefile >-------------------------------------------
      subroutine read_savefile
          ! reads restart file
          use mpi
          use runtime_m,      only: time, time_ref, run_title
          use io_profiling_m, only: dir_path
          use pnetcdf_m,      only: pnetcdf_read
          use topology_m,     only : myid
          implicit none

          ! local variables
          character*10  time_ext
          character*256 filename
          logical exist

          ! set file extension strings
          write(time_ext,'(1pe10.3)') time*time_ref

          ! set file name
          filename = trim(dir_path)//'/'//trim(run_title)//'.'// &
                     trim(adjustl(time_ext))//'.field.nc'

          ! inquire about file existence
          inquire(file=trim(filename),exist=exist)
          if (.NOT. exist) then
              if (myid .EQ. 0) then
                  print*, 'restart file does not exist ', trim(filename)
                  print*, 'skip reading restart files'
              endif
              return
          endif

          ! call PnetCDF APIs to read data from file
          call pnetcdf_read(filename)

      end subroutine read_savefile

