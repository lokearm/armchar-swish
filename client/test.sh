
export TEXINPUTS=.:texinputs/:
python3 client.py
pdflatex magus
python3 adv.py
pdflatex adv
