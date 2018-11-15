# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : textPlusPlus.native printbig.o

# "make textPlusPlus.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

textPlusPlus.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind textPlusPlus.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff

# Testing the "printbig" example

printbig : printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# Building the tarball

TESTS = \
  hello

FAILS = \
  

TESTFILES = $(TESTS:%=test-%.mc) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.mc) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags textPlusPlus.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c \
	$(TESTFILES:%=tests/%) 

textPlusPlus.tar.gz : $(TARFILES)
	cd .. && tar czf textPlusPlus/textPlusPlus.tar.gz \
		$(TARFILES:%=textPlusPlus/%)
