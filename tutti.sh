

mkdir -p GameStart Current
cabal run armchar -- -s Data/hibernia.json -g GameStart -D Current -t "Summer 1255" -d LongSheet

for d in GameStart Current LongSheet
do
    cd "$d"
    for i in *.md
    do
       o=`basename "$i" .md`.pdf
       pandoc -o "$o" "$i"
    done
cd ..
done
