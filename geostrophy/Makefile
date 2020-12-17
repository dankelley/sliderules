all:geostrophy_circular_1.out README.pdf
%.out: %.R
	Rscript $< > $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
clean: force
	-rm -rf *.out *~ README.pdf
force:
