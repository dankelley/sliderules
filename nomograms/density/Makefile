SOURCES=$(wildcard *.R)
OBJECTS=$(SOURCES:.R=.out)
all: $(OBJECTS)
%.out: %.R
	R --no-save < $< > $@
clean: force
	-rm *.out *.png *.pdf *~
force:
