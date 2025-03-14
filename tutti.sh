

mkdir -p GameStart
cabal run armchar -- -s Data/hibernia.json -g GameStart
cd GameStart
for i in *.md
do
   o=`basename "$i" .md`.pdf
   pandoc -o "$o" "$i"
done
