FC = gfortran
OBJS=modules.o
FFLAGS=

all: ass6
ass6: 
	latex ass6Writeup.tex
	dvipdf ass6Writeup.dvi
driver: modules.o main.o
	$(FC) -o driver main.o modules.o

check: driver 
	driver < data.txt

clean:
	rm -rf *.aux *.log *.o

#########################################################################
%.o: %.f
	$(FC) $(FFLAGS) -c $<

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

%.o: %.F
	$(FC) $(FFLAGS) -c $<
