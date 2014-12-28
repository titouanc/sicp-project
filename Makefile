SRC = lib fsm graphics virtualpet main
FIGURES = $(shell grep includegraphics *.tex | cut -d '{' -f 2 | cut -d '}' -f 1)

UTIL = ./utils/armpit.py

.PHONY: all run clean mrproper repl

all: $(addsuffix .uploaded,${SRC}) report.pdf
run: all
	${UTIL} -f main.scm

repl:
	${UTIL} -i

clean:
	rm -f *.uploaded

mrproper: clean
	${UTIL} -r

%.uploaded: %.scm
	${UTIL} -u $< && touch $@

%.eps: %.dot
	dot -Teps $< > $@

%.png: %.dot
	dot -Tpng $< > $@

%.pdf: %.tex ${FIGURES}
	pdflatex $<

