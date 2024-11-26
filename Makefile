##
## EPITECH PROJECT, 2024
## Glados
## File description:
## Makefile
##

all:
	stack build
	./scripts/create_symlink_lisp.sh

clean:
	stack clean
	rm -f run-lisp

fclean:
	stack clean --full
	rm -f run-lisp

re: fclean all

install:
	./scripts/install-hook.sh
	./scripts/install_ghcup.sh
	./scripts/config_ghcup.sh

tests:
	stack test ; python3 functionnal-tests/ftests.py

.PHONY: all clean fclean re
