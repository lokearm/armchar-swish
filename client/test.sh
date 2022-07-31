
export TEXINPUTS=.:texinputs/:

time python3 client.py
time python3 adv.py

diff baseline-adv.tex adv.tex && echo "advancement log is OK"
diff baseline-magus.tex magus.tex && echo "character sheet is OK"

# pdflatex magus
# pdflatex adv
