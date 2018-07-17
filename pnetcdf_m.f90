!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      module pnetcdf_m
      ! module for Read-Write Restart files using Parallel NetCDF
      use mpi
      use io_profiling_m
      use pnetcdf

      implicit none

      private :: handle_err

      contains

      !----< handle_err() >---------------------------------------------
      subroutine handle_err(err_msg, errcode)
          implicit none
          integer,       intent(in) :: errcode
          character*(*), intent(in) :: err_msg

          ! local variables
          integer err

          print *, 'Error: ',trim(err_msg),' ',nfmpi_strerror(errcode)
          call MPI_Abort(MPI_COMM_WORLD, -1, err)
      end subroutine handle_err

      !----< pnetcdf_write() >------------------------------------------
      subroutine pnetcdf_write(filename)
          use topology_m,  only : gcomm, npes, mypx, mypy, mypz, myid
          use param_m,     only : nx, ny, nz, nx_g, ny_g, nz_g, nsc
          use variables_m, only : temp, pressure, yspecies, u
          use runtime_m,   only : method, time, tstep, time_save, io_one_species_at_a_time
          implicit none

          ! declarations passed in
          character*(*), intent(in) :: filename

          ! local variables
          integer(MPI_OFFSET_KIND) g_sizes(4), subsizes(4), starts(4), len, put_size
          integer dimids(4), req(4), st(4), err, cmode
          integer i, ncid, yspecies_id, u_id, pressure_id, temp_id
          double precision time_start, time_end, d_time(1)

          ! create file and pass in the MPI hint
          time_start = MPI_Wtime()

          cmode = NF_CLOBBER + NF_64BIT_DATA
          err = nfmpi_create(gcomm, trim(filename), cmode, file_info, ncid)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_create', err)

          time_end = MPI_Wtime()
          openT = openT + time_end - time_start
          time_start = time_end

          ! Save timing metadata global attributes ---------------------------------
          len = 1
          d_time(1) = time
          err = nfmpi_put_att_double(ncid, NF_GLOBAL, 'time',      NF_DOUBLE, len, d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_put_att_double for time', err)
          d_time(1) = tstep
          err = nfmpi_put_att_double(ncid, NF_GLOBAL, 'tstep',     NF_DOUBLE, len, d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_put_att_double for tstep', err)
          d_time(1) = time_save
          err = nfmpi_put_att_double(ncid, NF_GLOBAL, 'time_save', NF_DOUBLE, len, d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_put_att_double for time_save', err)

          ! global array dimensionality
          g_sizes(1) = nx_g
          g_sizes(2) = ny_g
          g_sizes(3) = nz_g

          ! local subarray dimensionality
          subsizes(1) = nx
          subsizes(2) = ny
          subsizes(3) = nz

          ! start offsets of local array in global array
          ! note that Fortran array index starts with 1
          starts(1) = nx * mypx + 1
          starts(2) = ny * mypy + 1
          starts(3) = nz * mypz + 1
          starts(4) = 1

          ! define X-Y-Z dimensions of the global array
          err = nfmpi_def_dim(ncid, 'x',   g_sizes(1), dimids(1))
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_dim on x', err)
          err = nfmpi_def_dim(ncid, 'y',   g_sizes(2), dimids(2))
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_dim on y', err)
          err = nfmpi_def_dim(ncid, 'z',   g_sizes(3), dimids(3))
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_dim on z', err)

          ! define 4th dimension and variable yspecies
          g_sizes(4) = nsc + 1
          err = nfmpi_def_dim(ncid, 'nsc', g_sizes(4), dimids(4))
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_dim on nsc', err)
          err = nfmpi_def_var(ncid, 'yspecies', NF_DOUBLE, 4, dimids, yspecies_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_var on yspecies', err)

          ! define 4th dimension and variable u
          g_sizes(4) = 3
          err = nfmpi_def_dim(ncid, 'three', g_sizes(4), dimids(4))
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_dim on three', err)
          err = nfmpi_def_var(ncid, 'u', NF_DOUBLE, 4, dimids, u_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_var on u', err)

          ! define variable pressure
          err = nfmpi_def_var(ncid, 'pressure', NF_DOUBLE, 3, dimids, pressure_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_var on pressure', err)

          ! define variable temp
          err = nfmpi_def_var(ncid, 'temp', NF_DOUBLE, 3, dimids, temp_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_def_var on temp', err)

          ! end of define mode
          err = nfmpi_enddef(ncid)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_enddef', err)

          if (info_used .EQ. MPI_INFO_NULL) then
              err = nfmpi_get_file_info(ncid, info_used)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_get_file_info', err)
          endif

          if (method .EQ. 1) then  ! using nonblocking APIs
              !---- write array yspecies
              subsizes(4) = nsc + 1
              err = nfmpi_iput_vara_double(ncid, yspecies_id, starts, subsizes, yspecies, req(1))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iput_vara_double on yspecies', err)

              !---- write array u
              subsizes(4) = 3
              err = nfmpi_iput_vara_double(ncid, u_id, starts, subsizes, u, req(2))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iput_vara_double on u', err)

              !---- write array pressure
              err = nfmpi_iput_vara_double(ncid, pressure_id, starts, subsizes, pressure, req(3))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iput_vara_double on pressure', err)

              !---- write array temp
              err = nfmpi_iput_vara_double(ncid, temp_id, starts, subsizes, temp, req(4))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iput_vara_double on temp', err)

              err = nfmpi_wait_all(ncid, 4, req, st)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_wait_all', err)
          else ! using blocking APIs
              !---- write array yspecies
              if (io_one_species_at_a_time) then
                  subsizes(4) = 1
                  do i = 1, nsc + 1
                     starts(4) = i
                     err = nfmpi_put_vara_double_all(ncid, yspecies_id, starts, subsizes, yspecies(:,:,:,i))
                     if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on yspecies', err)
                  enddo
              else
                  subsizes(4) = nsc + 1
                  err = nfmpi_put_vara_double_all(ncid, yspecies_id, starts, subsizes, yspecies)
                  if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on yspecies', err)
              endif

              !---- write array u
              if (io_one_species_at_a_time) then
                  subsizes(4) = 1
                  do i = 1, 3
                     starts(4) = i
                     err = nfmpi_put_vara_double_all(ncid, u_id, starts, subsizes, u(:,:,:,i))
                     if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on u', err)
                  enddo
              else
                  subsizes(4) = 3
                  err = nfmpi_put_vara_double_all(ncid, u_id, starts, subsizes, u)
                  if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on u', err)
              endif

              !---- write array pressure
              err = nfmpi_put_vara_double_all(ncid, pressure_id, starts, subsizes, pressure)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on pressure', err)

              !---- write array temp
              err = nfmpi_put_vara_double_all(ncid, temp_id, starts, subsizes, temp)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_put_vara_double_all on temp', err)
          endif

          err = nfmpi_inq_put_size(ncid, put_size)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_put_size', err)

          write_amount = write_amount + put_size
          write_num    = write_num + 4

          time_end = MPI_Wtime()
          writeT = writeT + time_end - time_start

          err = nfmpi_close(ncid)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_close', err)

          closeT = closeT + MPI_Wtime() - time_end

      end subroutine pnetcdf_write

      !----< pnetcdf_read() >-------------------------------------------
      subroutine pnetcdf_read(filename)
          use topology_m,  only : gcomm, npes, mypx, mypy, mypz, myid
          use param_m,     only : nx, ny, nz, nx_g, ny_g, nz_g, nsc
          use variables_m, only : temp, pressure, yspecies, u
          use runtime_m,   only : method, time, tstep, time_save, io_one_species_at_a_time

          implicit none

          ! declarations passed in
          character*(*), intent(in) :: filename

          ! local variables
          integer err, cmode, req(4), st(4)
          integer i, ncid, yspecies_id, u_id, pressure_id, temp_id
          integer(MPI_OFFSET_KIND) g_sizes(4), subsizes(4), starts(4), get_size
          double precision time_start, time_end, d_time(1)

          ! open file and pass in the MPI hint
          time_start = MPI_Wtime()

          cmode = NF_NOWRITE
          err = nfmpi_open(gcomm, trim(filename), cmode, file_info, ncid)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_open', err)

          time_end = MPI_Wtime()
          openT = openT + time_end - time_start
          time_start = time_end

          ! Get timing metadata global attributes ---------------------------------
          d_time(1) = time
          err = nfmpi_get_att_double(ncid, NF_GLOBAL, 'time', d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_get_att_double for time', err)
          d_time(1) = tstep
          err = nfmpi_get_att_double(ncid, NF_GLOBAL, 'tstep', d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_get_att_double for tstep', err)
          d_time(1) = time_save
          err = nfmpi_get_att_double(ncid, NF_GLOBAL, 'time_save', d_time)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_get_att_double for time_save', err)

          ! global array dimensionality
          g_sizes(1) = nx_g
          g_sizes(2) = ny_g
          g_sizes(3) = nz_g

          ! local subarray dimensionality
          subsizes(1) = nx
          subsizes(2) = ny
          subsizes(3) = nz

          ! start offsets of local array in global array
          ! note that Fortran array index starts with 1
          starts(1) = nx * mypx + 1
          starts(2) = ny * mypy + 1
          starts(3) = nz * mypz + 1
          starts(4) = 1

          ! inquire variable yspecies id
          err = nfmpi_inq_varid(ncid, 'yspecies', yspecies_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_varid on yspecies', err)

          ! inquire variable u id
          err = nfmpi_inq_varid(ncid, 'u',        u_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_varid on u', err)

          ! inquire variable pressure id
          err = nfmpi_inq_varid(ncid, 'pressure', pressure_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_varid on pressure', err)

          ! inquire variable temp id
          err = nfmpi_inq_varid(ncid, 'temp',     temp_id)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_varid on temp', err)

          if (method .EQ. 1) then  ! using nonblocking APIs
              !---- read array yspecies
              subsizes(4) = nsc + 1
              err = nfmpi_iget_vara_double(ncid, yspecies_id, starts, subsizes, yspecies, req(1))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iget_vara_double on yspecies', err)

              !---- read array u
              subsizes(4) = 3
              err = nfmpi_iget_vara_double(ncid, u_id, starts, subsizes, u, req(2))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iget_vara_double on u', err)

              !---- read array pressure
              err = nfmpi_iget_vara_double(ncid, pressure_id, starts, subsizes, pressure, req(3))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iget_vara_double on pressure', err)

              !---- read array temp
              err = nfmpi_iget_vara_double(ncid, temp_id, starts, subsizes, temp, req(4))
              if (err .ne. NF_NOERR) call handle_err('nfmpi_iget_vara_double on temp', err)

              err = nfmpi_wait_all(ncid, 4, req, st)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_wait_all', err)
          else ! using blocking APIs
              !---- read array yspecies
              if (io_one_species_at_a_time) then
                  subsizes(4) = 1
                  do i = 1, nsc + 1
                     starts(4) = i
                     err = nfmpi_get_vara_double_all(ncid, yspecies_id, starts, subsizes, yspecies(:,:,:,i))
                     if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on yspecies', err)
                  enddo
              else
                  subsizes(4) = nsc + 1
                  err = nfmpi_get_vara_double_all(ncid, yspecies_id, starts, subsizes, yspecies)
                  if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on yspecies', err)
              endif

              !---- read array u
              if (io_one_species_at_a_time) then
                  subsizes(4) = 1
                  do i = 1, 3
                     starts(4) = i
                     err = nfmpi_get_vara_double_all(ncid, u_id, starts, subsizes, u(:,:,:,i))
                     if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on u', err)
                  enddo
              else
                  subsizes(4) = 3
                  err = nfmpi_get_vara_double_all(ncid, u_id, starts, subsizes, u)
                  if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on u', err)
              endif

              !---- read array pressure
              err = nfmpi_get_vara_double_all(ncid, pressure_id, starts, subsizes, pressure)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on pressure', err)

              !---- read array temp
              err = nfmpi_get_vara_double_all(ncid, temp_id, starts, subsizes, temp)
              if (err .ne. NF_NOERR) call handle_err('nfmpi_get_vara_double_all on temp', err)
          endif

          err = nfmpi_inq_get_size(ncid, get_size)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_inq_get_size', err)

          read_amount = read_amount + get_size
          read_num    = read_num + 4

          time_end = MPI_Wtime()
          readT = readT + time_end - time_start

          err = nfmpi_close(ncid)
          if (err .ne. NF_NOERR) call handle_err('nfmpi_close', err)

          closeT = closeT + MPI_Wtime() - time_end

      end subroutine pnetcdf_read

      end module pnetcdf_m
