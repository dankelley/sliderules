all: fig.out README.md README.pdf
%.out: %.R
	Rscript $< > $@
%.md: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "md_document")'
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
clean: force
	-rm -rf *.out *~ README.pdf
force:
