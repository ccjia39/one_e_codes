#
# Makefile for DOM examples
#
default: all
all: getsta coll sigma
#
#---------------------------
MKLROOT=/opt/intel/oneapi/mkl/2024.1
MK=$(FLIB_ROOT)/fortran.mk
include $(MK)
#---------------------------
#
# Uncomment the following line for debugging support
#
LIBS=$(LIB_PREFIX)$(LIB_STD) -lflib  ${MKLROOT}/lib/libmkl_lapack95_ilp64.a  -L${MKLROOT}/lib -lmkl_intel_ilp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl

LDFLAGS = -O3 -fopenmp #-parallel

getsta: general.o tools.o newtypes.o matrices.o centerlib.o fdn.o setup.o inputlib.o cgto.o overlap.o special_functions.o f11.o nuclear.o computemat.o diagolib.o getsta.o
	$(FC) $(LDFLAGS) -o GetSta general.o tools.o newtypes.o matrices.o centerlib.o fdn.o setup.o inputlib.o cgto.o overlap.o special_functions.o f11.o nuclear.o computemat.o diagolib.o getsta.o $(LIBS)

coll: general.o abmlib.o linearinterp.o  splineinterp.o tools.o setup.o newtypes.o matrices.o inputlib.o centerlib.o fdn.o colllib.o cgto.o colldyn.o  overlap.o special_functions.o f11.o nuclear.o  collint.o  coll.o
	$(FC) $(LDFLAGS) -o Coll general.o abmlib.o linearinterp.o  splineinterp.o  tools.o setup.o newtypes.o matrices.o inputlib.o centerlib.o fdn.o colllib.o cgto.o colldyn.o overlap.o special_functions.o f11.o nuclear.o collint.o  coll.o $(LIBS)

sigma: general.o newtypes.o  sigma.o
	$(FC) $(LDFLAGS) -o Sigma general.o newtypes.o  sigma.o $(LIBS)
#
clean: 
	rm -f GetSta  Coll  *.o *.$(MOD_EXT)
#

tar:
	tar cvf Coll_CPC.tar *.f* makefile xmlf90-1.2g.tgz Example RunExample.sh










