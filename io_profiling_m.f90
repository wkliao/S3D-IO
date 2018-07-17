!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      module io_profiling_m
      ! module for Read-Write Restart files
      use mpi
      implicit none

      character*256 :: dir_path      ! directory name for output files
      integer file_info, info_used
      integer cb_nodes               ! collective buffering nodes

      integer          read_num      ! number of read calls
      integer          write_num     ! number of write calls
      double precision read_amount   ! total read amount
      double precision write_amount  ! total write amount
      double precision io_amount     ! total I/O amount
      double precision openT, writeT, readT, closeT
                       ! time for open, write, read, and close

      contains

      ! ----------------------------------------------------------------
      ! print the MPI info objects to stdout
      ! ----------------------------------------------------------------
      subroutine print_io_hints(info)
          implicit none
          integer, intent(in) :: info

          ! local variables
          character*(MPI_MAX_INFO_VAL) key, value
          integer                      i, nkeys, valuelen, err
          logical                      flag

 1001     FORMAT('    ',A32,' = ',A)
          call MPI_Info_get_nkeys(info, nkeys, err)
          print *, '---- MPI file info used ----'
          do i=0, nkeys-1
              key(:) = ' '
              call MPI_Info_get_nthkey(info, i, key, err)
              call MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, flag, err)
              call MPI_Info_get_valuelen(info, key, valuelen, flag, err)
              value(valuelen+1:) = ' '
              if (key(len_trim(key):len_trim(key)) .EQ. char(0)) &
                  key(len_trim(key):) = ' '
              print 1001, trim(key), trim(value)
          enddo
          print *
      end subroutine print_io_hints

      ! ----------------------------------------------------------------
      ! get the file striping information from the MPI info objects
      ! ----------------------------------------------------------------
      subroutine get_file_striping(info, striping_factor, striping_unit)
          implicit none
          integer, intent(in)  :: info
          integer, intent(out) :: striping_factor
          integer, intent(out) :: striping_unit

          ! local variables
          character*(MPI_MAX_INFO_VAL) key, value
          integer                      i, nkeys, valuelen, err
          logical                      flag

          call MPI_Info_get_nkeys(info, nkeys, err)
          do i=0, nkeys-1
              key(:) = ' '
              call MPI_Info_get_nthkey(info, i, key, err)
              call MPI_Info_get(info, key, MPI_MAX_INFO_VAL, value, flag, err)
              call MPI_Info_get_valuelen(info, key, valuelen, flag, err)
              value(valuelen+1:) = ' '
              if (key(len_trim(key):len_trim(key)) .EQ. char(0)) &
                  key(len_trim(key):) = ' '
              if (trim(key) .EQ. 'striping_factor') &
                  read(value, '(i10)') striping_factor
              if (trim(key) .EQ. 'striping_unit') &
                  read(value, '(i10)') striping_unit
          enddo
      end subroutine get_file_striping

      !----< set_io_hints() >-------------------------------------------
      subroutine set_io_hints(flag)
          implicit none
          integer, intent(in) :: flag

          ! local variables
          integer       err
          character*16  int_str

          ! free up info and file type
          if (flag .EQ. -1) then
              if (file_info .NE. MPI_INFO_NULL) then
                  call MPI_Info_free(file_info, err)
                  file_info = MPI_INFO_NULL
              endif
              return
          endif

          read_amount  = 0.0
          write_amount = 0.0
          io_amount    = 0.0
          read_num     = 0
          write_num    = 0
          openT        = 0.0
          writeT       = 0.0
          readT        = 0.0
          closeT       = 0.0
          info_used    = MPI_INFO_NULL

          ! set MPI I/O hints for performance enhancement
          call MPI_Info_create(file_info, err)

          ! disable ROMIO data sieving
          call MPI_Info_set(file_info, 'romio_ds_write',    'disable', err)
          call MPI_Info_set(file_info, 'romio_ds_read',     'disable', err)
          call MPI_Info_set(file_info, 'romio_no_indep_rw', 'true',    err)

          ! set the number of aggregate I/O nodes (for advanced users)
          write(int_str,'(I16)') cb_nodes
          ! call MPI_Info_set(file_info, 'cb_nodes', int_str, err)

          ! set PnetCDF hints
          ! call MPI_Info_set (file_info, 'nc_header_align_size',      '512',    err)
          ! call MPI_Info_set (file_info, 'nc_var_align_size',         '512',    err)
          ! call MPI_Info_set (file_info, 'nc_header_read_chunk_size', '262144', err)

      end subroutine set_io_hints

      !----< print_io_performance() >-----------------------------------
      subroutine  print_io_performance(io_time)
          use topology_m, only : gcomm, myid, npes
          use param_m,    only : nx_g, ny_g, nz_g
          use runtime_m,  only : method, restart
          implicit none
          double precision, intent(inout) :: io_time

          ! local variables
          double precision d_tmp, read_bandwidth, write_bandwidth, io_bandwidth
          integer          striping_factor, striping_unit, err

          call MPI_Reduce(openT,        d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, gcomm, err)
          openT     = d_tmp
          call MPI_Reduce(writeT,       d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, gcomm, err)
          writeT    = d_tmp
          call MPI_Reduce(readT,        d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, gcomm, err)
          readT     = d_tmp
          call MPI_Reduce(closeT,       d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, gcomm, err)
          closeT    = d_tmp
          call MPI_Reduce(io_time,      d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, gcomm, err)
          io_time   = d_tmp
          call MPI_Reduce(read_amount,  d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, gcomm, err)
          read_amount = d_tmp
          call MPI_Reduce(write_amount, d_tmp, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, gcomm, err)
          write_amount = d_tmp
          io_amount = read_amount + write_amount

          if (myid == 0) then
              io_bandwidth    = 0.0
              read_bandwidth  = 0.0
              write_bandwidth = 0.0
              if (io_time > 0) io_bandwidth    = io_amount    / io_time
              if (readT   > 0) read_bandwidth  = read_amount  / readT
              if (writeT  > 0) write_bandwidth = write_amount / writeT
              io_bandwidth    = io_bandwidth    / 1048576.0
              io_amount       = io_amount       / 1073741824.0
              read_bandwidth  = read_bandwidth  / 1048576.0
              read_amount     = read_amount     / 1073741824.0
              write_bandwidth = write_bandwidth / 1048576.0
              write_amount    = write_amount    / 1073741824.0

              striping_factor = 0
              striping_unit   = 0
              if (info_used .NE. MPI_INFO_NULL) then
                  call print_io_hints(info_used)
                  call get_file_striping(info_used, striping_factor, striping_unit)
              endif

 2000         FORMAT(A)
 2001         FORMAT(A, A)
 2002         FORMAT(A ,I7)
 2003         FORMAT(A ,I7, A)
 2004         FORMAT(A ,F10.2, A)
 2005         FORMAT(A, i7,' x',i7,' x',i7)

              write(6,*) '++++ I/O is done through PnetCDF ++++'
              if (method .EQ. 0) then
                  write(6,*) 'I/O method          : blocking APIs'
              else
                  write(6,*) 'I/O method          : nonblocking APIs'
              endif
              if (restart) then
                  write(6,*) 'Run with restart    : True'
              else
                  write(6,*) 'Run with restart    : False'
              endif
              write(6, 2002) ' No. MPI processes   : ', npes
              write(6, 2005) ' Global array size   : ', nx_g, ny_g, nz_g
              write(6, 2001) ' output file path    : ',trim(dir_path)
              write(6, 2002) ' file striping count : ',striping_factor
              write(6, 2003) ' file striping size  : ',striping_unit, ' bytes'
              write(6, 2000) ' -----------------------------------------------'
              write(6, 2004) ' Time for open       :  ',openT,      ' sec'
              write(6, 2004) ' Time for read       :  ',readT,      ' sec'
              write(6, 2004) ' Time for write      :  ',writeT,     ' sec'
              write(6, 2004) ' Time for close      :  ',closeT,     ' sec'
              write(6, 2003) ' no. read  calls     :  ',read_num, '    per process'
              write(6, 2003) ' no. write calls     :  ',write_num,'    per process'
              write(6, 2004) ' total read  amount  :  ',read_amount ,  ' GiB'
              write(6, 2004) ' total write amount  :  ',write_amount , ' GiB'
              write(6, 2004) ' read  bandwidth     :  ',read_bandwidth ,  ' MiB/s'
              write(6, 2004) ' write bandwidth     :  ',write_bandwidth , ' MiB/s'
              write(6, 2000) ' -----------------------------------------------'
              write(6, 2004) ' total I/O   amount  :  ',io_amount , ' GiB'
              write(6, 2004) ' total I/O   time    :  ',io_time , ' sec'
              write(6, 2004) ' I/O   bandwidth     :  ',io_bandwidth, ' MiB/s'
          endif

          if (info_used .NE. MPI_INFO_NULL) &
              call MPI_Info_free(info_used, err)

      end subroutine  print_io_performance

      end module io_profiling_m
