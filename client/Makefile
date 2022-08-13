

export TEXINPUTS=.:texinputs/:

all: magus.pdf adv.pdf

%.pdf: %.tex
	pdflatex $<

cieran.ttl:
	http :3000/cs/cieran/1218/Spring > $@
graph.ttl:
	http :3000/graph > $@
