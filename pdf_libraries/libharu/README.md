Before Installation the following dependecies are required\
sudo apt-get install automake\
sudo apt-get install autoconf\
sudo apt-get install zlib1g-dev\
sudo apt-get install libpng-dev\
sudo apt-get install libtool\
\

Installing libharu\
tar -xvsf libharu-X.X.X.tar.gz\
cd libharu-X.X.X\
\
Run the configuration scipt\
./configure\

	-If there is no configuration script
		./buildconf.sh --force
	-Then try running ./configure again

Build the Library and install\
make clean\
make\
make install\
\
If there are errors about not being able to find the \*.so file or \
a missing directory. Please execute the following:\
ldconfig /usr/local/lib\


To run the following example\
gcc pdf.c grid\_sheet.c -o pdf -lhpdf\




