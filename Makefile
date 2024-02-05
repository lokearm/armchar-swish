all: grog.md sylvain.md
diff: grog.diff sylvain.diff marcus.diff
formats: charactersheet.pdf charactersheet.html

.force:

O=Ontology/resources.ttl Ontology/arm.ttl

%.md: Test/%.ttl .force $O
	cabal run armchar-cli -- -c $< -s Test/saga.ttl -o $@ -O $*.triples

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<

Ontology/%.ttl: .force
	( cd Ontology ; $(MAKE) $*.ttl )

%.diff: %.md
	diff $< Test/$< | tee $@
