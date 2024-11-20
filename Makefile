##
## EPITECH PROJECT, 2024
## Glados
## File description:
## Makefile
##

all:
	stack build

clean:
	rm -f *~
	rm -f #*#

fclean: clean

re: fclean all

.PHONY: all clean fclean re
