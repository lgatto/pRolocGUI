
all:
	"$(R_HOME)/bin/Rscript" -e "library(knitrBootstrap); knit_bootstrap('pRolocGUI.Rmd', show_code=TRUE, boot_style='Flatly')"

setvars:
ifeq (${R_HOME},)
R_HOME= $(shell R RHOME)
endif

clean:
	rm -f *.md
allclean: clean
	rm -f *html
