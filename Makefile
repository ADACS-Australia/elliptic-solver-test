# Useful as an escape hatch if libpetsc.so was not compiled properly
# and has missing symbols from external libraries.
ifeq ($(PETSC_PC_STATIC), yes)
  STATIC = "--static"
else
  STATIC =
endif

PETSC_PC = $(shell pkg-config --path petsc || pkg-config --path PETSc || if [ -n "${PETSC_DIR}" ] && [ -d "${PETSC_DIR}" ]; then find "${PETSC_DIR}" -type f -iname petsc.pc 2>/dev/null | head -n 1; fi)

# We attempt to find the correct flags for PETSc using pkg-config,
# however each system may have different configurations, so these
# variables can be overridden by the user if necessary.
PETSC_INCLUDE ?= $(shell pkg-config --cflags $(PETSC_PC) 2>/dev/null)
PETSC_LDFLAGS ?= $(shell pkg-config $(STATIC) --libs $(PETSC_PC) 2>/dev/null)

CFLAGS := $(PETSC_INCLUDE)
LDFLAGS := $(PETSC_LDFLAGS)

# Compiler and flags
ifeq ($(MPI), yes)
  FC := mpifort
  CFLAGS += -DMPI
else
  FC := gfortran
endif

FFLAGS := -O2 -Wall -Wextra -fcheck=all -g

# Program name
TARGET = sparse_solver_test

# Source files
SRCS = mpi_utils.F90 datatype.f90 hormone_routines.f90 tools.f90 petsc_routines.F90 setup.f90 solver.f90 main.f90

# Object files (automatically generated from source files)
OBJS1 = $(SRCS:.f90=.o)
OBJS = $(OBJS1:.F90=.o)

# Default target
all: $(TARGET)

# Rule to build the target executable
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# Rule to compile Fortran source files into object files
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# Rule to compile Fortran source files into object files
%.o: %.F90
	$(FC) $(FFLAGS) $(CFLAGS) -c -o $@ $<

# Clean up build files
clean:
	rm -f $(TARGET) *.o *.mod

# Phony targets (not actual files)
.PHONY: all clean

print-petsc:
	@echo "PETSC_PC: $(PETSC_PC)"
	@echo "PETSC_INCLUDE: $(PETSC_INCLUDE)"
	@echo "PETSC_LDFLAGS: $(PETSC_LDFLAGS)"
