# 1996: Hans Georg Schaathun <georg@hamilton.nhh.no>
#
# Sample LaTeX Makefile for GNU make.

# This Makefile depends on the file patterns, which will define pattern
# rules for LaTeX files.

TEX   = latex
BIB   = bibtex
DVIPS = dvips
DVI   = xdvi
LPR   = lpr

TEXFILES = matlab.tex shazam.tex modell.tex main.tex preamble.tex
AUXFILES = matlab.aux shazam.aux modell.aux
MFILES   = *.m
TABLES   = reg-conf.tex out.tex data.tex

TEXINPUTS = .:./input:

%.ps: %.dvi
	$(DVIPS) -t a4 -o $@ $<

%.dvi: %.tex
	$(TEX) $<

balder.dvi:

texput.dvi:
	$(TEX)

clean:
	/bin/rm -f *.log *.aux *.dvi *.bak *~ *.lpr

distclean: clean
	/bin/rm -f *.ps
