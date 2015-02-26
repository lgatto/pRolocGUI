
all:
	"$(R_HOME)/bin/Rscript" -e "library(methods);library(knitrBootstrap); knit_bootstrap('pRolocGUI.Rmd', show_code=TRUE, boot_style='Flatly')"

clean:
	rm -f *.md
	rm -f pRolocGUI.R
	rm -f .build.timestamp
	rm -f .Rhistory

allclean: clean
	rm -f *html
