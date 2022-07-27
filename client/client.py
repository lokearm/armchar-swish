import http.client, urllib.parse
import json

conn = http.client.HTTPConnection("localhost:3000")
conn.request("GET", "/ability/cieran/1217/Autumn" )

response = conn.getresponse()
print(response.status, response.reason)
data = response.read()
print(data)
y = json.loads(data)

ab = [ (i.get("arm:hasLabel","???"),i.get("arm:hasSpeciality","-"),i.get("arm:hasScore","-"),i.get("arm:hasXP","-")) for i in y ]
ab.sort()

for i in ab:
    print( i )

conn.request("GET", "/virtue/cieran/1217/Autumn" )

response = conn.getresponse()
print(response.status, response.reason)
data = response.read()
print(data)
y = json.loads(data)
for i in y:
    print( i )
