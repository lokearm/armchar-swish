
def label(i):
    lab = i.get("arm:hasLabel","")
    det = i.get("arm:hasDetail","")
    ins = i.get("arm:instanceLabel","")
    if (lab != "") & (ins != ""):
        lab += " - " + ins
    elif ins:
        lab = ins
    if not lab: lab = "???"
    if det:
        # lab += " (" + det + ")"
        pass
    return lab
