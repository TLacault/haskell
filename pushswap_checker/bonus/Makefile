SRC			=	pushswap.hs

BINARY_NAME	=	pushswap

BINARY_TESTS_NAME	=	test

CFLAGS		=	-W -Wall -Wextra

T_LDFLAGS	=	-lcriterion  --coverage

all:
	ghc -o $(BINARY_NAME) $(CFLAGS) $(SRC)

in:
	ghci -o $(BINARY_NAME) $(CFLAGS) $(SRC)

clean:
	rm -f *.o
	rm -f *.dyn_o
	rm -f *.hi
	rm -f *.dyn_hi
	rm -f *~

fclean:		clean test_clean
	rm -f $(BINARY_NAME)

re:		fclean all

test_clean:		fclean
	rm -f $(BINARY_TESTS_NAME)

tests_compile:
	gcc -o $(BINARY_TESTS_NAME) $(TEST) $(CFLAGS)

tests_run:		tests_compile
	./$(BINARY_TESTS_NAME)

tests_run_branches:		tests_compile
	./$(BINARY_TESTS_NAME)
