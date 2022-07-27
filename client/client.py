#! /usr/bin/env python3

import http.client, urllib.parse
import json

conn = http.client.HTTPConnection("localhost:3000")
conn.request("GET", "/ability/cieran/1217/Autumn" )

response = conn.getresponse()
print(response.status, response.reason)
data = response.read()
y = json.loads(data)

ab = [ (i.get("arm:hasLabel","???"),i.get("arm:hasSpeciality","-"),i.get("arm:hasScore","-"),i.get("arm:hasXP","-")) for i in y ]
ab.sort()

output = [ "\documentclass{armsheet}", "\\begin{magus}" ]

output.append( "\\begin{abilities}" )
for i in ab:
    output.append( f"  \\Anability[{i[1]}]{{{i[0]}}}{{{i[2]}}}{{{i[3]}}}" )
output.append( "\\end{abilities}" )

output.append(  "\\end{magus}" )

conn.request("GET", "/virtue/cieran/1217/Autumn" )

response = conn.getresponse()
print(response.status, response.reason)
data = response.read()
y = json.loads(data)

f = open("magus.tex", "w")
for line in output:
    f.write(line+"\n")
f.close()
