# Makefile : INSACITY

all:
	lazbuild insacity.lpi
	
clean:
	rm -rf *.o *.ppu *.res units/ backup/ lib/
