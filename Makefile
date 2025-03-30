
I=$(HOME)/Hobby/github/hibernia/
J=$(HOME)/Hobby/github/ars-magica.github.io/
D=`find dist-newstyle -name doc | head`/html/armchar-swish/armchar/

.force:

bin/armchar: .force
	cabal install armchar  --overwrite-policy=always --installdir=./bin
doc: .force
	cabal haddock armchar  

install: bin/armchar doc
	cp --copy-contents $< $I/bin
	ls -l $J
	rsync -av $D $J/doc/

		
