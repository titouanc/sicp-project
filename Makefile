SRC = main $(shell ./make-deps.sh main.scm)
FIGURES = $(shell grep includegraphics *.tex | cut -d '{' -f 2 | cut -d '}' -f 1)
ARCHIVE = SICP.tichrist.tar.gz
UTIL = ./utils/armpit.py

.PHONY: all run clean mrproper repl upload

all: ${ARCHIVE} report.pdf
upload: $(addsuffix .uploaded,${SRC})
run: upload
	${UTIL} -f main.scm

repl:
	${UTIL} -i

clean:
	rm -f *.uploaded *.log *.aux *.toc *.out

mrproper: clean
	${UTIL} -r
	rm -f report.pdf ${ARCHIVE}

${ARCHIVE}: $(addsuffix .scm,${SRC}) ${UTIL} report.pdf Makefile make-deps.sh
	tar c $^ | gzip > $@

%.uploaded: %.scm
	${UTIL} -u $< && touch $@

%.eps: %.dot
	dot -Teps $< > $@

%.png: %.dot
	dot -Tpng $< > $@

%.pdf: %.tex ${FIGURES}
	pdflatex $< #ToC
	pdflatex $<

