PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC := $(shell basename $(PWD))

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN ?= $(shell dirname "`which R`")

.PHONY: help

help:
	@echo "\nExecute development tasks for $(PKGNAME)\n"
	@echo "Usage: \`make <task>\` where <task> is one of:"
	@echo ""
	@echo "Development Tasks"
	@echo "-----------------"
	@echo " deps Install dependencies for package development"
	@echo " docs Invoke roxygen to generate Rd files in a seperate"
	@echo " directory"
	@echo " vignette Build a copy of the package vignette"
	@echo " cleanvignette Build the vignette and remove tmp files (.aux, .log etc.)"
	@echo " build Invoke docs and then create a package"
	@echo " check Invoke build and then check the package"
	@echo " install Invoke build and then install the result"
	@echo " clean Remove tmp files (.aux, .log etc.)"
	@echo ""
	@echo "Packaging Tasks"
	@echo "---------------"
	@echo " release Cleans and builds package"
	@echo ""
	@echo "Using R in: $(RBIN)"
	@echo "Set the RBIN environment variable to change this."
	@echo ""


#------------------------------------------------------------------------------
# Development Tasks
#------------------------------------------------------------------------------
deps:
	"$(RBIN)/R" --vanilla --slave -e "install.packages(c('roxygen'))"


docs:
	rm man/*;\
	cp ../DocFiles.$(PKGNAME)/*.Rd man/
	cd ..;\
	"$(RBIN)/R" --no-restore --slave -e "library(roxygen); roxygenize('$(PKGSRC)', '$(PKGSRC)', use.Rd2=TRUE, overwrite=TRUE, unlink.target=FALSE, copy.package=FALSE)"


vignette:
	cd inst/doc;\
	"$(RBIN)/R" CMD Sweave $(PKGNAME).Rnw;\
	R CMD pdflatex $(PKGNAME).tex

build: docs
	cd ..;\
	"$(RBIN)/R" CMD build --no-vignettes $(PKGSRC)

install: build
	cd ..;\
	"$(RBIN)/R" CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	"$(RBIN)/R" CMD check --no-tests $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	find . -type f -iname "*.aux"  -exec rm {} \; 
	find ./inst/doc -type f -iname "*-*"  -exec rm {} \; 
	find . -type f -iname "*.tex"  -exec rm {} \; 
	find . -type f -iname "*.log"  -exec rm {} \; 
	find . -type f -iname "*.out"  -exec rm {} \; 
	find . -type f -iname "Rplots.pdf" -exec rm {} \;

cleanvignette: vignette clean

#------------------------------------------------------------------------------
# Packaging Tasks
#------------------------------------------------------------------------------
release: clean check
