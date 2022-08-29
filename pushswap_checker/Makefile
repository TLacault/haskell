##
## EPITECH PROJECT, 2021
## haskell
## File description:
## Makefile
##

SRC			=	pushswap_checker.hs

BINARY_NAME	=	pushswap_checker

BINARY_TESTS_NAME	=	test

CFLAGS		=	-W -Wall -Wextra

T_LDFLAGS	=	-lcriterion  --coverage

all:
	ghc -o $(BINARY_NAME) $(CFLAGS) $(SRC)
	make -C bonus/

in:
	ghci -o $(BINARY_NAME) $(CFLAGS) $(SRC)

clean:
	rm -f *.o
	rm -f *.dyn_o
	rm -f *.hi
	rm -f *.dyn_hi
	rm -f *~
	make clean -C bonus/

fclean:		clean test_clean
	rm -f $(BINARY_NAME)
	make fclean -C bonus/

re:		fclean all

test_clean:		fclean
	rm -f $(BINARY_TESTS_NAME)
	make test_clean -C bonus/

tests_compile:
	gcc -o $(BINARY_TESTS_NAME) $(TEST) $(CFLAGS)

tests_run:		tests_compile
	./$(BINARY_TESTS_NAME)
	make tests_run -C bonus/

tests_run_branches:		tests_compile
	./$(BINARY_TESTS_NAME)
