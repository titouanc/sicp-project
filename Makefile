SRC = lib fsm graphics virtualpet main
FIGURES = $(shell grep includegraphics *.tex | cut -d '{' -f 2 | cut -d '}' -f 1)

UTIL = ./utils/armpit.py

.PHONY: all run clean

all: $(addsuffix .uploaded,${SRC}) report.pdf
run: all
	${UTIL} -f main.scm

clean:
	rm -f *.uploaded

%.uploaded: %.scm
	${UTIL} -u $< && touch $@

%.eps: %.dot
	dot -Teps $< > $@

%.pdf: %.tex ${FIGURES}
	pdflatex $<

