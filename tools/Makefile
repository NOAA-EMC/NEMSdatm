CDF=/apps/netcdf/4.3.0-intel
NWPROD=/scratch3/NCEPDEV/nwprod/lib
NEMSIO=$(NWPROD)/nemsio/v2.2.3
#####################################################################
# compiler options
#####################################################################
FOPT = -O
#FOPT = -C

F90 = ifort
#F90 = gfortran -O3 
#FOPT = -march=native -fPIC -fconvert=big-endian 

#opt1 = -Ddebug

optall = $(opt1) $(opt2)

#####################################################################
# 
#####################################################################

OBJS = param.o GFS_diagnostics.o kinds.o charstrings.o cdf.o sigmavars.o \
sfcvars.o gfstonc_sfc.o gfstonc_sig.o rdnemsio.o sigma2nc.o sfc2nc.o \
 write_sigmacdf.o setup_outcdf.o tm_secs_from_bc.o fieldmatch.o find_t850.o


nemsio2nc: $(OBJS) 
	$(F90) $(FOPT) -o nemsio2nc $(OBJS) -L$(NEMSIO) -lnemsio_v2.2.3 -L$(NWPROD) -lbacio_4 -lw3nco_d -L$(CDF)/lib -lnetcdff -lnetcdf 

%.o: %.F90
	$(F90) $(FOPT) $(optall) -c -I$(CDF)/include -I$(NEMSIO)/include $<
#cpp $(optall) -I$(CDF)/include $*.F90>$*.i

clean:
	/bin/rm -f nemsio2nc *.o *.i *.mod
