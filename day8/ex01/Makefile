# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: ydumaine <ydumaine@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2025/04/03 11:19:13 by ydumaine          #+#    #+#              #
#    Updated: 2025/06/02 10:48:14 by ydumaine         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME = a.out
# Order is important for the linking
SOURCES = app.ml  main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

all: .depend $(NAME)

$(NAME): opt byt
	rm -f $(NAME)
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt

byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OBJS_OPT = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	$(CAMLC) -o $@ $(OBJS)

$(NAME).opt: $(OBJS_OPT)
	$(CAMLOPT) -o $@ $(OBJS_OPT)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.ml.cmi:
	$(CAMLC) -c $<

clean : 
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f *.o
	rm -f .depend

fclean: clean
	rm -rf $(NAME)
	rm -rf $(NAME).byt
	rm -rf $(NAME).opt

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend

.phony: all clean fclean depend re