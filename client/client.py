#! /usr/bin/env python3

import http.client, urllib.parse
import json
import sys

fn = "magus.tex"
print (sys.argv)
if ( len(sys.argv) > 1 ):
    fn = sys.argv[1]
print ( "Filename: " + fn )

# cstring = "cieran/1217/Autumn"
cstring = "cieran/1218/Spring"

def get(conn,path):
   conn.request("GET", path)
   response = conn.getresponse()
   print(path,response.status, response.reason)
   data = response.read()
   return json.loads(data)

conn = http.client.HTTPConnection("localhost:3000")

output = [ "\\documentclass{armsheet}", "\\begin{magus}" ]

y = get(conn,"/char/" + cstring )

if y.__contains__( "arm:hasPlayer" ): 
       output.append( f"\\player{{{y['arm:hasPlayer']}}}" )
if y.__contains__( "arm:hasSaga" ): 
       output.append( f"\\saga{{{y['arm:hasSaga']}}}" )
if y.__contains__( "arm:hasBirthYear" ): 
       output.append( f"\\born{{{y['arm:hasBirthYear']}}}" )
if y.__contains__( "arm:hasGender" ): 
       output.append( f"\\gender{{{y['arm:hasGender']}}}" )
if y.__contains__( "arm:hasCovenantName" ): 
       output.append( f"\\covenant{{{y['arm:hasCovenantName']}}}" )
if y.__contains__( "arm:hasSagaTitle" ): 
       output.append( f"\\saga{{{y['arm:hasSagaTitle']}}}" )
if y.__contains__( "arm:hasName" ): 
       output.append( f"\\name{{{y['arm:hasName']}}}" )
if y.__contains__( "arm:hasNationality" ): 
       output.append( f"\\nationality{{{y['arm:hasNationality']}}}" )
if y.__contains__( "arm:hasProfession" ): 
       output.append( f"\\concept{{{y['arm:hasProfession']}}}" )
if y.__contains__( "arm:hasAge" ): 
       output.append( f"\\age{{{y['arm:hasAge']}}}" )
if y.__contains__( "arm:inYear" ): 
       s = y.get("arm:atSeason","")
       if s == "":
          output.append( f"\\year{{{y['arm:inYear']}}}" )
       else:
          output.append( f"\\year{{{s} {y['arm:inYear']}}}" )

#   "arm:hasAlmaMater": "Stonehenge",


y = get(conn,"/ability/" + cstring  )

ab = [ (i.get("arm:hasLabel","???"),i.get("arm:hasSpeciality","-"),i.get("arm:hasScore","-"),i.get("arm:hasXP","-")) for i in y ]
ab.sort()


output.append( "\\begin{abilities}" )
for i in ab:
    output.append( f"  \\Anability[{i[1]}]{{{i[0]}}}{{{i[2]}}}{{{i[3]}}}" )
output.append( "\\end{abilities}" )


y = get(conn,"/virtue/" + cstring )
y += get(conn,"/flaw/" + cstring )

output.append( "\\begin{vf}" )
for i in y:
    output.append( f"  \\vfLine{{{i.get('arm:hasLabel','???')}}}{{{i.get('arm:hasScore','?')}}}" )
output.append( "\\end{vf}" )

y = get(conn,"/pt/" + cstring )
output.append( "\\begin{personality}" )
for i in y:
    output.append( f"  \\aPtrait{{{i.get('arm:hasLabel','???')}}}{{{i.get('arm:hasScore','?')}}}" )
output.append( "\\end{personality}" )

y = get(conn,"/equipment/" + cstring )
output.append( "\\begin{equipment}" )
for i in y:
    q = i.get('arm:hasQuantity','')
    if q != "": q = f" ({q})"
    output.append( f"  \\eqPiece{{{i.get('arm:hasLabel','???')}{q}}}{{{i.get('arm:hasLoad','')}}}" )
output.append( "\\end{equipment}" )

y = get(conn,"/characteristic/" + cstring )
for i in y:
    output.append( f'\Characteristic{{{i.get("arm:hasScore","?")}}}{{{i.get("arm:hasAbbreviation","?").lower()}}}' )

y = get(conn,"/art/" + cstring )

for i in y:
    output.append( f'\AnArt{{{i.get("arm:hasLabel","?").lower()}}}{{{i.get("arm:hasScore","?")}}}{{{i.get("arm:hasXP","?")}}}{{{i.get("arm:hasVis","-")}}}' )

y = get(conn,"/spell/" + cstring )
output.append( "\\begin{grimoire}" )
for i in y:
    masteryscore = i.get('arm:hasScore','-')
    mxp = i.get('arm:hasXP','')
    if mxp != "": masteryscore = f"{masteryscore} ({mxp})"
    notes = f"{{{i.get('arm:hasRangeString','???')}/{i.get('arm:hasDurationString','???')}/{i.get('arm:hasTargetString','???')}}}"
    masteryoption = i.get('arm:hasMasteryOptionString','')
    if not isinstance(masteryoption,str):
        masteryoption = "".join( masteryoption )
    if masteryoption != "":
       notes += "; " + masteryoption
    f = i.get('arm:hasFormString',"??")[:2]
    f2 = i.get('arm:hasFormRequisiteString',"")
    if isinstance(f2,str):
        f2 = f2[:2]
    else:
        f2 = "".join( [ s[:2] for s in f2 ] )
    if f2 != "": f = f + "("+f2+")"
    t = i.get('arm:hasTechniqueString',"??")[:2]
    t2 = i.get('arm:hasTechniqueRequisiteString',"")
    if isinstance(t2,str):
        t2 = t2[:2]
    else:
        t2 = "".join( [ s[:2] for s in t2 ] )
    if t2 != "": t = t + "("+t2+")"
    tefo = t + f
    output.append( f"  \\Aspell{{{i.get('arm:hasLabel','???')}}}{{{tefo}}}{{{i.get('arm:hasLevel','?')}}}{{{i.get('arm:hasCastingScore','?')}}}{{{masteryscore}}}{{{notes}}}" )
output.append( "\\end{grimoire}" )

y = get(conn,"/combat/" + cstring )
output.append( "\\begin{combat}" )
for i in y:
    output.append( f"\weapon:{i.get('arm:hasLabel','??')}:"
                    + f"init={i.get('arm:hasInit','-')},"
                    + f"atk={i.get('arm:hasAtk','-')};"
                    + f"def={i.get('arm:hasDfn','-')},"
                    + f"dam={i.get('arm:hasDam','-')},"
                    + f"rng={i.get('arm:hasWeaponRange','-')},"
                    + f"str={i.get('arm:hasStr','-')}." )

output.append( "\\end{combat}" )

output.append(  "\\end{magus}" )
f = open( fn, "w" )
for line in output:
    f.write(line+"\n")
f.close()
