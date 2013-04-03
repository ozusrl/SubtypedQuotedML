
all:
	cd src/staged && make depend && make

run:
	cd src/staged && make depend && make
	./src/staged/staged

clean:
	cd src/staged && make clean

clean_all:
	cd src/staged && make clean
	cd src/toy && make clean
	cd src/gromit && make clean
	cd src/wallace && make clean
