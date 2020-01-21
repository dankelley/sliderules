all:README.pdf
%.pdf: %.Rmd
	pandoc -o README.md README.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
clean: force
	-rm -rf *.out *~ README.pdf
force:
