!
!  Copyright (C) 2013, Northwestern University
!  See COPYRIGHT notice in top-level directory.
!

      module topology_m
      ! module for topology variables
      use mpi
      implicit none

      integer gcomm
      integer npes          ! total number of processors
      integer myid          ! rank of local processor
      integer mypx, mypy, mypz

      contains

      !----< initialize_topology() >-----------------------------------
      subroutine initialize_topology(npx,npy,npz)
          ! routine initializes some MPI stuff and the Cartesian MPI grid
          implicit none
          integer npx, npy, npz

          integer err

          ! check for npes compatibility
          if (npx*npy*npz .NE. npes) then
 1000        format(' npx*npy*npz is not equal to npes, npx = ',  &
                    i5,' npy = ', i5, ' npz = ', i5, ' npes = ', i5)
             if (myid .EQ. 0) then
                print 1000, npx,npy,npz,npes
             endif
             call MPI_Comm_free(gcomm,err)
             call MPI_Finalize(err)
             stop
          endif

          ! initialize Cartesian grid
          mypz = myid/(npx*npy)
          mypx = mod(myid-(mypz*npx*npy), npx)
          mypy = (myid-(mypz*npx*npy))/npx

          ! print*,'myid=',myid,' mypx=',mypx,' mypy=',mypy,' mypz=',mypz
      end subroutine initialize_topology

      end module topology_m
