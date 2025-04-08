# Useful as an escape hatch if libpetsc.so was not compiled properly
# and has missing symbols from external libraries.
ifeq ($(PETSC_PC_STATIC), yes)
  STATIC = "--static"
else
  STATIC =
endif

PETSC_PC = $(shell pkg-config --path petsc || pkg-config --path PETSc || if [ -n "${PETSC_DIR}" ] && [ -d "${PETSC_DIR}" ]; then find "${PETSC_DIR}" -type f -iname petsc.pc 2>/dev/null | head -n 1; fi)
PETSC_INCLUDE ?= $(shell pkg-config --cflags $(PETSC_PC) 2>/dev/null)
PETSC_LDFLAGS ?= $(shell pkg-config $(STATIC) --libs $(PETSC_PC) 2>/dev/null)

# Compiler and flags
FC := gfortran
FFLAGS := -O2 -Wall -Wextra -fcheck=all -g  # Debugging and optimization flags

CFLAGS := $(PETSC_INCLUDE)
LDFLAGS := $(PETSC_LDFLAGS)

# Program name
TARGET = sparse_solver_test

# Source files
SRCS = datatype.f90 hormone_routines.f90 tools.f90 petsc_routines.F90 setup.f90 solver.f90 main.f90

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
