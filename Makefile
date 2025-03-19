# Compiler and flags
FC = gfortran
FFLAGS = -O2 -Wall -Wextra -fcheck=all -g  # Debugging and optimization flags
LDFLAGS =  # Additional linker flags (if needed)

# Program name
TARGET = sparse_solver_test

# Source files
SRCS = sparse_solver.f90

# Object files (automatically generated from source files)
OBJS = $(SRCS:.f90=.o)

# Default target
all: $(TARGET)

# Rule to build the target executable
$(TARGET): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# Rule to compile Fortran source files into object files
%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

# Clean up build files
clean:
	rm -f $(OBJS) $(TARGET) *.mod

# Phony targets (not actual files)
.PHONY: all clean