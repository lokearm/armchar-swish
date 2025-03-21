

mkdir -p Hibernia/GameStart Hibernia/Current/LongSheet
cabal run armchar -- -s Data/hibernia.json 

for d in Hibernia/GameStart Hibernia/Current Hibernia/Current/LongSheet
do
   ( cd "$d"
     for i in *.md
     do
        o=`basename "$i" .md`.pdf
        pandoc -o "$o" "$i"
     done
   )
done
