# Compiler and flags
FC = gfortran

PETSC_DIR = /Users/david/miniforge3/envs/petsc

FFLAGS = -O2 -Wall -Wextra -fcheck=all -g  # Debugging and optimization flags

CFLAGS = -I$(PETSC_DIR)/include
LDFLAGS = -L$(PETSC_DIR)/lib -lpetsc

# Program name
TARGET = sparse_solver_test

# Source files
SRCS = datatype.f90 hormone_routines.f90 tools.f90 petsc_solver.F90 setup.f90 solver.f90 main.f90

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
