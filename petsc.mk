pkgconf = $(shell command -v pkgconf || command -v pkg-config)

ifdef $(PETSC_STATIC)
  PETSC_STATIC = --static
else
  PETSC_STATIC =
endif

# Check if pkgconf is installed
ifneq ($(pkgconf),)
  petsc.pc = $(shell $(pkgconf) --exists petsc && pkgconf --path petsc)
else
  $(error Could not find pkgconf or pkg-config, please install it in order to continue building with PETSc.)
endif

# If petsc.pc is empty, try finding it under PETSC_LIBDIR
ifeq ($(petsc.pc),)
  ifeq ($(PETSC_LIBDIR),)
    $(error Could not find petsc.pc, please add it to PKG_CONFIG_PATH, or retry with PETSC_LIBDIR.)
  else
    petsc.pc.tmp = $(PETSC_LIBDIR)/pkgconfig/PETSc.pc
    ifneq (,$(wildcard $(petsc.pc.tmp)))
      petsc.pc = $(petsc.pc.tmp)
    else
      $(error $(petsc.pc.tmp) does not exist. Please check your PETSC_LIBDIR.)
    endif
  endif
endif

# Find the 'petscvariables' file
petscvariables = $(shell $(pkgconf) --variable=libdir $(petsc.pc))/petsc/conf/variables

# Extract PETSC_FC_INCLUDES, which is contains the include paths for the Fortran compiler.
# (On some systems, the Fortran module files are in a different directory than the C header files,
# and pkgconf does not provide a way to get them.)
# PETSC_FPPFLAGS = $(shell $(MAKE) -f $(petscvariables) -s -E 'dummy:; @echo $$(PETSC_FC_INCLUDES)')
PETSC_FPPFLAGS = $(shell $(MAKE) --no-print-directory --silent petscvariables=$(petscvariables) -f petsc.fcincludes.mk)

# Get the library flags needed for linking
PETSC_LIBS = $(shell $(pkgconf) --libs $(PETSC_STATIC) $(petsc.pc))
