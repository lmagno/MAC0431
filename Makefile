NAME = Ondas
SOURCE = $(NAME).f95
OUTPUT = $(NAME).out

MODDIR = ./Módulos/
MODULES = Entrada Saida
MODFILES = $(addprefix $(MODDIR), $(MODULES:=.f95))

FFLAGS = -Wall -fdefault-real-8 -g3 -Wimplicit-interface -fopenmp # Usar double como default

.PHONY: all clean

$(OUTPUT): $(MODFILES) $(SOURCE)
	gfortran $(FFLAGS) $(MODFILES) $(SOURCE) -o $(OUTPUT)

clean:
	rm -f $(OUTPUT) *.mod
