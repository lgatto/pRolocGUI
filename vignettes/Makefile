
all:
	"$(R_HOME)/bin/Rscript" -e "library(methods);library(knitrBootstrap); library(rmarkdown);render('pRolocGUI.Rmd', bootstrap_document(theme='Flatly'))"

clean:
	rm -f *.md
	rm -f pRolocGUI.R
	rm -f .build.timestamp
	rm -f .Rhistory

allclean: clean
	rm -f *html
