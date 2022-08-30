

export TEXINPUTS=.:texinputs/:

all: magus.pdf adv.pdf

%.pdf: %.tex
	pdflatex $<

cieran.ttl: .force
	http :3000/cs/cieran/1218/Summer > $@
graph.ttl: .force
	http :3000/graph > $@
.force:

chargen.log: .force
	http :3000/show/chargen/cieran > $@

