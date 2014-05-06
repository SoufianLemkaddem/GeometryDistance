FC = gfortran
FFLAGS = -Wall -Wextra -march=native -g -O3 -fopenmp
LDFLAGS = -g
LIBS = -llapack -lblas -fopenmp

FFLAGS += $(shell pkg-config --cflags plplotd-f95)
LIBS += $(shell pkg-config --libs plplotd-f95)


COMPILE = $(FC) $(FFLAGS)
LINK = $(FC) $(LDFLAGS)

OBJS += initializers.o
OBJS += point.o
OBJS += plotters.o
OBJS += geometry.o
OBJS += test_points.o

geometry: $(OBJS)
	$(LINK) -o $@ $^ $(LIBS)	

%.o: %.f90
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	$(RM) myprog $(OBJS) *.mod

