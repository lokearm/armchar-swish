all: charactersheet.pdf charactersheet.html

.force:

%.md: Test/%.ttl .force
	cabal run armchar-cli -- -c $< -s Test/saga.ttl -o $@ -O $*.triples

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<
