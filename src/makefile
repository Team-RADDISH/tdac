# makefile for tdac

FC=mpif90
FFLAGS=

OBJS_PDAF = m_pdaf.o
LIB_PDAF = -L/Users/tkoskela/pdaf/PDAF-D_V1.15/lib -lpdaf-d

TARGET:../tdac.x

.SUFFIXES:
.SUFFIXES: .f90 .o

m_params.o: m_params.f90
m_llw2d.o: m_llw2d.f90 m_params.o
m_matrix.o: m_matrix.f90
m_pdaf.o: m_pdaf.f90 m_params.o

.f90.o:
	$(FC) $(FFLAGS) -c $< -o $@

../tdac.x: $(OBJS_PDAF) m_params.o m_llw2d.o m_matrix.o tdac.f90
	$(FC) $(FFLAGS) -o $@ $^ $(LIB_PDAF)

clean:
	/bin/rm -f *.o *.mod *~ ../tdac.x
