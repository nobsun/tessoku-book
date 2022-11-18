NAME	= document
STYL    = slide
VIEWER  = qpdfview

all	: $(NAME).pdf

ifeq ($(STYL),slide)
# %.pdf	: %.md
# 	pandoc -f markdown -t beamer -V theme:Singapore -H slide.tex -o $@ --pdf-engine=lualatex $<
%.pdf	: %.md
	pandoc -f markdown -t beamer -V theme:Madrid -H slide.tex -o $@ --pdf-engine=lualatex $<
else
%.pdf	: Makefile %.tex
	platex $(NAME)
	dvipdfmx $(NAME)
	cp $@ $(NAME)`date +%Y%m%d`.pdf
endif

clean	:
	-rm *~ *.nav *.out *.snm *.dvi *.aux *.log *.bib *.bbl *.blg *.toc *.ptb

preview	: $(NAME).pdf
	$(VIEWER) $< &
