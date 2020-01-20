all:01_model_selection.out README.pdf sound_speed_circular_1.out

%.out: %.R
	Rscript $< &> $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
clean: force
	-rm -rf *.out *~ README.pdf
force:
