ifneq ($(ComSpec),)
OCAMLMAKE=ocamlmake.exe
RM=del
else
OCAMLMAKE=ocamlmake
RM=rm
endif

OCAMLMAKE_EXEC=ocamlrun -b $(OCAMLMAKE)

TESTS=mli addmli two twomli twoaddmli runmli

.PHONY: test teardown clean $(TESTS)
test: $(TESTS)
	make teardown

$(OCAMLMAKE): ../source/ocamlmake.ml
	ocamlc -c -g -w Aer -o ./ocamlmake.cmo $<
	ocamlc -o $@ unix.cma ocamlmake.cmo

mli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
ifneq ($(ComSpec),)
	echo val plus: int -^> int -^> int > test_a.mli
else
	echo "val plus: int -> int -> int" > test_a.mli
endif
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

addmli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M
ifneq ($(ComSpec),)
	echo val plus: int -^> int -^> int > test_a.mli
else
	echo "val plus: int -> int -> int" > test_a.mli
endif
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
ifneq ($(ComSpec),)
	echo val plus: int -^> int -^> int > test_b.mli
else
	echo "val plus: int -> int -> int" > test_b.mli
endif
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
ifneq ($(ComSpec),)
	echo val plus: int -^> int -^> int > test_b.mli
else
	echo "val plus: int -> int -> int" > test_b.mli
endif
	$(OCAMLMAKE_EXEC) test_a.ml
	$(OCAMLMAKE_EXEC) -M

runmli: $(OCAMLMAKE)
	make teardown
	echo ====$@====
	echo let plus x y = x + y > test_a.ml
ifneq ($(ComSpec),)
	echo val plus: int -^> int -^> int > test_a.mli
else
	echo "val plus: int -> int -> int" > test_a.mli
endif
	$(OCAMLMAKE_EXEC) -run test_a.ml
	$(OCAMLMAKE_EXEC) -M

teardown:
	-$(RM) .ocamlmake test* *.cm? *.o

clean: teardown
	-$(RM) $(OCAMLMAKE)
