
time http put :3000/char < ../Test/char.json > char.log
time python3 client.py magus3.tex
time python3 adv.py adv3.tex

diff magus.tex magus3.tex
diff adv.tex adv3.tex

# diff baseline-adv2.tex adv2.tex && echo "advancement log is OK"
# diff baseline-magus2.tex magus2.tex && echo "character sheet is OK"
