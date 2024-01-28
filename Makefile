all: charactersheet.pdf charactersheet.html

.force:

charactersheet.md: .force
	cabal run armchar-cli -- -c Test/sylvain.ttl -s Test/saga.ttl -o charactersheet.md -O charactersheet.triples

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<
