SHELL := /bin/bash
NAME  := InterimReport

$(NAME).pdf: $(NAME).tex
	pdflatex $(NAME).tex

.phony: clean
clean:
	rm -f $(NAME).{aux,log,out,pdf,toc}
