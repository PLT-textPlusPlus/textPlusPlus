Installing libharu
tar -xvsf libharu-X.X.X.tar.gz
cd libharu-X.X.X

Run the configuration scipt
./configure

	-If there is no configuration script
		./buildconf.sh --force
	-Then try running ./configure again

Build the Library and install
make clean
make
make install

To run the following example
gcc pdf.c grid\_sheet.c -o pdf -lhpdf




