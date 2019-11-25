all:00.out 01.out 02.out 03.out 04.out sigma_theta_ruler_1.out README.pdf
%.out: %.R
	Rscript $< > $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("'$<'", "pdf_document")'
clean: force
	-rm -rf *.out *~ README.pdf
force:
