
time http put :3000/adv < ../Test/adv.json > put.log
time python3 client.py magus2.tex
time python3 adv.py adv2.tex

# diff magus.tex magus2.tex
# diff adv.tex adv2.tex

echo "Comparing advancement"
diff baseline-adv2.tex adv2.tex && echo "advancement log is OK"
echo "Comparing character sheet"
diff baseline-magus2.tex magus2.tex && echo "character sheet is OK"
