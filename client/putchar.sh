
time http put :3000/char < ../Test/char.json > char.log
time python3 client.py magus3.tex

diff magus2.tex magus3.tex

diff baseline-magus3.tex magus3.tex && echo "character sheet is OK"
