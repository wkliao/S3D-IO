!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      !----< main >-----------------------------------------------------
      program main
         use mpi
         use param_m,    only: npx, npy, npz
         use param_m,    only: initialize_param
         use topology_m, only: gcomm, npes, myid, initialize_topology
         implicit none

         integer err
         logical isArgvRight

         call MPI_Init(err)
         call MPI_Comm_rank(MPI_COMM_WORLD, myid, err)
         call MPI_Comm_size(MPI_COMM_WORLD, npes, err)

         call MPI_Comm_dup(MPI_COMM_WORLD, gcomm,err)

         call read_command_line_arg(isArgvRight)
         if (.NOT. isArgvRight)  goto 999

         ! initialize parameters: nx, ny, nz, nsc, n_spec
         call initialize_param(myid, gcomm)

         ! initialize MPI process topology
         call initialize_topology(npx,npy,npz)

         ! main computation task is here
         call MPI_Barrier(gcomm,err)
         call solve_driver

 999     call MPI_Comm_free(gcomm,err)
         call MPI_Finalize(err)
      end program main

      !----< read_command_line_arg >------------------------------------
      subroutine read_command_line_arg(isArgvRight)
         use mpi
         use param_m,        only: nx_g, ny_g, nz_g, npx, npy, npz, nsc
         use param_m,        only: initialize_param
         use runtime_m,      only: method, restart, io_one_species_at_a_time
         use topology_m,     only: gcomm, npes, myid, initialize_topology
         use io_profiling_m, only: dir_path
         implicit none

         character(len=128) executable
         logical isArgvRight

         ! declare external functions
         integer IARGC

         ! local variables for reading command-line arguments
         character(len = 256) :: argv(9)
         integer i, argc, int_argv(7), err
         integer(MPI_OFFSET_KIND) io_size

         ! Only root process reads command-line arguments
         if (myid .EQ. 0) then
            isArgvRight = .TRUE.
            call getarg(0, executable)
            argc = IARGC()
            if (argc .NE. 9) then
               print *, 'Usage: ',trim(executable), &
               ' nx_g ny_g nz_g npx npy npz method restart dir_path'
               isArgvRight = .FALSE.
            else
               do i=1, argc-2
                  call getarg(i, argv(i))
                  read(argv(i), FMT='(I16)') int_argv(i)
               enddo
               call getarg(argc-1, argv(argc-1))
               read(argv(argc-1), FMT='(L)') restart

               call getarg(argc, argv(argc))
               dir_path = argv(argc)

               nx_g   = int_argv(1)
               ny_g   = int_argv(2)
               nz_g   = int_argv(3)
               npx    = int_argv(4)
               npy    = int_argv(5)
               npz    = int_argv(6)
               method = int_argv(7)
            endif
         endif

         ! broadcast if arguments are valid
         call MPI_Bcast(isArgvRight, 1, MPI_LOGICAL, 0, gcomm, err)
         if (.NOT. isArgvRight) return

         call MPI_Bcast(nx_g,       1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(ny_g,       1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(nz_g,       1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(npx,        1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(npy,        1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(npz,        1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(method,     1, MPI_INTEGER,   0, gcomm, err)
         call MPI_Bcast(restart,    1, MPI_LOGICAL,   0, gcomm, err)
         call MPI_Bcast(dir_path, 256, MPI_CHARACTER, 0, gcomm, err)

         io_size = 8
         io_size = io_size * nx_g
         io_size = io_size * ny_g
         io_size = io_size * ny_g
         io_size = io_size / npes
         if (io_size .GT. 2147483647) then
             if (myid .EQ. 0) then
                 print*, '******** Error ********'
                 print*, '   Array size per process is too large for 4-byte integers'
                 print*, '   Please use a smaller array size. Exit...'
                 print*, '******** Error ********'
             endif
             call MPI_Finalize(err)
             STOP
         endif

         io_one_species_at_a_time = .FALSE.
         io_size = io_size * (11 + 3 + 2)
         if (io_size .GT. 2147483647) then
             if (myid .EQ. 0) then
                 print*, '******** Warning ********'
                 print*, 'Warning: Array size is too large for 4-byte integers'
                 print*, 'Warning: I/O is now performed one species at a time'
                 if (method .EQ. 1) &
                     print*, 'Warning: Switch to blocking I/O method'
                 print*, '******** Warning ********'
             endif
             method = 0
             io_one_species_at_a_time = .TRUE.
         endif

      end subroutine read_command_line_arg

