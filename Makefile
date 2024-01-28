all: charactersheet.pdf charactersheet.html

%.pdf: %.md
	pandoc -o $@ $<
%.html: %.md
	pandoc -o $@ $<
