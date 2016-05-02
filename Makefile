RMDFILES = $(wildcard *.Rmd) $(wildcard raw/*.Rmd)
MDFILES = $(RMDFILES:.Rmd=.md)
RENDER = ./render

%.md: %.Rmd
	$(RENDER) $< $@
html: $(MDFILES)
all: html
.PHONY: pull push clean
pull:
	git pull origin gh-pages
push:
	git push origin gh-pages
