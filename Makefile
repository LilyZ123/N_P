# Fortran Compiler
FC = gfortran

# Compiler Flags
FFLAGS = -O2 -Wall -Wno-tabs -free -g

# Source Files
SOURCES = ascrv.f90 curno.f90 fert.f90 harvestop.f90 harvkillop.f90 \
plantop.f90 readchm.f90 readmgt.f90 readsol.f90 readsub.f90\
nminrl.f90 varinit.f90 operatn.f90 rootfr.f90 sched_mgt.f90 jdt.f90 main.f90

# Object Files
OBJECTS = $(SOURCES:.f90=.o)

# Executable Name
EXECUTABLE = main

# Compilation Rule
all: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
	$(FC) $(FFLAGS) -c -o $@ $<

clean:
	rm -f $(EXECUTABLE) $(OBJECTS)