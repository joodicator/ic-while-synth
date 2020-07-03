SHELL := /bin/bash
NAME  := FinalReport

$(NAME).pdf: $(NAME).tex
	pdflatex $(NAME).tex

.phony: clean
clean:
	rm -f $(NAME).{aux,log,out,pdf,toc,cb,cb2}

.phony: almostclean
almostclean:
	rm -f $(NAME).{aux,log,out,toc,cb,cb2}
