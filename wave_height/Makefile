all:README.md README.pdf 00_formula.out 01_model_selection.out

%.md: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "md_document")'
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
%.out: %.R
	Rscript $< &> $@
	##Rscript $< mockup > $@
clean: force
	-rm -rf *.out *~ README.pdf
force:
