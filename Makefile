# Fortran Compiler
FC = gfortran

# Compiler Flags
FFLAGS = -O2 -Wall -Wno-tabs -free -g


# Source Files
 SOURCES = modpar.f90 readchm.f90 readmgt.f90 readsol.f90 \
           nminrl.f90 
#   main.f90 ascrv.f90 tillfactor.f90 rootfr.f90 \
#           curno.f90 irrigate.f90 \
#       newtillmix.f90 tstr.f90 swu.f90 varinit.f90 fert.f90 plantop.f90 \
#       apply.f90 burnop.f90 graze.f90 harvestop.f90 \
#           harvkillop.f90 irrsub.f90 sched_mgt.f90 \
#           operatn.f90 anfert.f90 killop.f90 confert.f90 \
#           nuts.f90 nfix.f90 nup.f90 npup.f90 ndenit.f90 grow.f90 \
#           nlch.f90 nrain.f90 orgn.f90 \
#           pminrl.f90 solp.f90 nminrl.f90 psed.f90 \
#           nitvol.f90 plantmod.f90


# Object Files
OBJECTS = $(SOURCES:.f90=.o)

# Executable Name
EXECUTABLE = SOIL_NP

# Compilation Rule
all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
        $(FC) $(FFLAGS) -o $@ $^

%.o: %.f90
        $(FC) $(FFLAGS) -c -o $@ $<


clean:
        rm -f $(EXECUTABLE) $(OBJECTS)