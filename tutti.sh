

cabal run armchar -- -s Data/hibernia.json 

exit 0

for d in Hibernia/GameStart Hibernia/"Summer 1254" Hibernia/"Summer 1254"/LongSheet
do
   ( cd "$d"
     for i in *.md
     do
        o=`basename "$i" .md`.pdf
        pandoc -o "$o" "$i"
     done
   )
done
