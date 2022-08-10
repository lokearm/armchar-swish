#! /usr/bin/env python3

import http.client, urllib.parse
import json
import sys

fn = "adv.tex"
print (sys.argv)
if ( len(sys.argv) > 1 ):
    fn = sys.argv[1]
print ( "Filename: " + fn )

def get(conn,path):
   conn.request("GET", path)
   response = conn.getresponse()
   print(path,response.status, response.reason)
   data = response.read()
   return json.loads(data)

conn = http.client.HTTPConnection("localhost:3000")

output = [ "\\documentclass{scrartcl}",
           "\\title{Advancement Log}",
           "\\author{armchar example}",
           "\\begin{document}",
           "\\maketitle" ]

y = get(conn,"/adv/cieran" )

output.append( "\\begin{description}" )

for i in y:
    c = i["advancementcontents"] 
    print (c)
    output.append( f'  \\item[{c.get( "arm:atSeason", "")} {c.get("arm:inYear","-")}]' )
    output.append( f'    {c.get( "arm:hasAdvancementDescription", "")}' )
    output.append( '' )
    output.append( f'    {c.get( "arm:hasAdvancementTypeString", "")} awards {c.get( "arm:awardsXP", "?")}xp' )

    ts = i.get("advancementtraits",{})
    output.append( "    \\begin{itemize}" )
    for t in ts:
        nxp = t.get( "arm:addedXP", "")
        if nxp != "": nxp = f": {nxp}xp"
        output.append( f'      \\item {t.get("arm:hasLabel","???")}{nxp}' )
    output.append( "    \\end{itemize}" )
    ts = i.get("advancementitems",{})
    print(ts)
    if ts:
        output.append( "    Possesions:" )
        output.append( "    \\begin{itemize}" )
        for t in ts:
           nxp = t.get( "arm:hasQuantity", "")
           if nxp != "": nxp = f" ({nxp})"
           output.append( f'      \\item {t.get("arm:hasLabel","???")}{nxp}' )
           output.append( f'      \\item {t.get("arm:hasDescription","")}' )
        output.append( "    \\end{itemize}" )

output.append( "\\end{description}" )

output.append(  "\\end{document}" )
f = open( fn, "w" )
for line in output:
    f.write(line+"\n")
f.close()
