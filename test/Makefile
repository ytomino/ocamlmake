HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

ifneq ($(findstring mingw,$(TARGET)),)
 EXEEXT=.exe
else
 EXEEXT=
endif

OCAMLMAKE=ocamlmake$(EXEEXT)

OCAMLC=ocamlc.opt

OCAMLMAKE_EXEC=ocamlrun -b ./$(OCAMLMAKE)

TESTS=mli addmli two twomli twoaddmli runmli

.PHONY: test teardown clean $(TESTS)
test: $(TESTS)
	make teardown

$(OCAMLMAKE): ../source/ocamlmake.ml
	$(OCAMLC) -c -g -w Aer -o ./ocamlmake.cmo $<
	$(OCAMLC) -o $@ unix.cma ocamlmake.cmo

mli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
	echo "val plus: int -> int -> int" > test_a.mli
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

addmli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M
	echo "val plus: int -> int -> int" > test_a.mli
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

two: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let _ = Test_b.plus 1 2 > test_a.ml
	echo let plus x y = x + y > test_b.ml
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

twomli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let _ = Test_b.plus 1 2 > test_a.ml
	echo "val plus: int -> int -> int" > test_b.mli
	echo let plus x y = x + y > test_b.ml
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

twoaddmli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let _ = Test_b.plus 1 2 > test_a.ml
	echo let plus x y = x + y > test_b.ml
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M
	echo "val plus: int -> int -> int" > test_b.mli
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

runmli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
	echo "val plus: int -> int -> int" > test_a.mli
	$(OCAMLMAKE_EXEC) -run test_a.ml
	$(OCAMLMAKE_EXEC) -M

teardown:
	-rm .ocamlmake test* *.cm? *.o

clean: teardown
	-rm $(OCAMLMAKE)
