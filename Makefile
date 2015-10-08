# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: afaucher <afaucher@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/11 15:29:28 by afaucher          #+#    #+#              #
#    Updated: 2015/10/08 17:19:31 by afaucher         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

RM = /bin/rm
SRC = src
SOURCES = data.scala \
		  expertSystem.scala \
		  lexer.scala \
		  parser.scala \
		  tree.scala \

S = scala
SC = scalac
TARGET = .
CP = .
NAME = ExpertSystem
NO_COLOR=\x1b[0m
OK_COLOR=\x1b[32;01m
WARN_COLOR=\x1b[33;01m


all: $(NAME)

$(NAME):
	@echo "$(OK_COLOR)Compiling...$(NO_COLOR)"
	@$(SC) -cp $(CP) -d $(TARGET) $(SOURCES)

clean:
	@echo "$(WARN_COLOR)"
	$(RM) -rf expertSystem
	@echo "$(NO_COLOR)"

fclean: clean

re: fclean all
