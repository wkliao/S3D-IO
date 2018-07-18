#
#  Copyright (C) 2013, Northwestern University
#  See COPYRIGHT notice in top-level directory.
#
#
# Please change the following variables:
#    MPIF90        -- MPI Fortran compiler
#    FCFLAGS       -- Compile flag
#    PNETCDF_DIR   -- PnetCDF library installation directory
#

MPIF90       = mpif90
FCFLAGS      = -Wall -g
PNETCDF_DIR  = $(HOME)/PnetCDF

COMPILE_F90  = $(MPIF90) $(FCFLAGS) $(INC) -c
LINK         = $(MPIF90) $(FCFLAGS)
INC          = -I$(PNETCDF_DIR)/include
LIBS         = -L$(PNETCDF_DIR)/lib -lpnetcdf

SRCS = runtime_m.f90 \
       param_m.f90 \
       topology_m.f90 \
       variables_m.f90 \
       io_profiling_m.f90 \
       pnetcdf_m.f90 \
       init_field.f90 \
       io.f90 \
       random_number.f90 \
       solve_driver.f90 \
       main.f90

OBJS = $(SRCS:.f90=.o)
MODS = $(SRCS:.f90=.mod)

TARGET = s3d_io.x

all: $(TARGET)

%.o:%.f90
	$(COMPILE_F90) $<

$(TARGET): $(OBJS)
	$(LINK) $(OBJS) -o $(TARGET) $(LIBS)

PACKAGE_NAME = s3d-io-pnetcdf-1.2.1

PACKING_LIST = $(SRCS) Makefile README.md COPYRIGHT RELEASE_NOTE

dist:
	/bin/rm -rf $(PACKAGE_NAME) $(PACKAGE_NAME).tar.gz
	mkdir $(PACKAGE_NAME)
	cp $(PACKING_LIST) $(PACKAGE_NAME)
	tar -cf $(PACKAGE_NAME).tar $(PACKAGE_NAME)
	gzip $(PACKAGE_NAME).tar
	/bin/rm -rf $(PACKAGE_NAME)

clean:
	/bin/rm -f $(OBJS) $(MODS) $(TARGET)

distclean: clean
	/bin/rm -rf $(PACKAGE_NAME).tar.gz $(PACKAGE_NAME)
