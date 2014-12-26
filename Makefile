SRC = lib fsm graphics virtualpet main

UTIL = ./armpit.py

.PHONY: all run clean

all: $(addsuffix .uploaded,${SRC})
run: all
	${UTIL} -f main.scm

clean:
	rm -f *.uploaded

%.uploaded: %.scm
	${UTIL} -u $< && touch $@