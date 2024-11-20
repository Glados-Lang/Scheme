##
## EPITECH PROJECT, 2024
## Glados
## File description:
## Makefile
##

all:
	stack build

clean:
	stack clean

fclean:
	stack clean --full

re: fclean all

install:
	./scripts/install-hook.sh
	./scripts/config_ghcup.sh

.PHONY: all clean fclean re
