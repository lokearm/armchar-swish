
export TEXINPUTS=.:texinputs/:

time python3 client.py
time python3 adv.py

diff baseline-adv.tex adv.tex && echo "character sheet is OK"
diff baseline-magus.tex magus.tex && echo "advancement log is OK"

# pdflatex magus
# pdflatex adv
