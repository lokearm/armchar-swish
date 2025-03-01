---
title: Testing the Web Server
tags: 
   - armchar/server
---


## Design notes

+ see also [[index]]
+ [[Tests]] showing current features of the web API
+ [[DataStructure]] 

## Testing the Server

The easiest way to run the program is via cabal:
```
cabal run
```

It starts a web server on port 3000.  
The most convenient way to test it is probably the
[HTTPie](https://httpie.io/) tool.
```
% http get :3000/
HTTP/1.1 200 OK
Content-Type: text/plain; charset=utf-8
Date: Thu, 21 Apr 2022 20:51:46 GMT
Server: Warp/3.3.20
Transfer-Encoding: chunked

Test a get call - available paths for get:
  /    (this page)
  /graph
  /initial
```

Testing PUT calls is a bit tricky, as `http` does not read from the terminal,
but the following does work (as an example):
```
cat | http put :3000/adv
{
        "advancementcontents": {
            "arm:atSeason": "Autumn",
            "arm:awardsXP": 16,
            "arm:hasAdvancementDescription": "Studies Mentem L16 Q13 +3",
            "arm:hasAdvancementIndex": 100,
            "arm:hasAdvancementType": {
                "prefixedid": "arm:Reading"
            },
            "arm:hasAdvancementTypeString": "Reading",
            "arm:inYear": 1217
        },
        "advancementid": "<https://hg.schaathun.net/armchar/character/cieran#autumn1217adv>",
        "advancementtraits": [
            {
                "arm:addedXP": 16,
                "arm:hasLabel": "Mentem",
                "arm:isSpecialTrait": {
                    "prefixedid": "arm:AccelleratedTrait"
                },
                "arm:prefixedid": "_:73",
                "arm:traitClass": {
                    "prefixedid": "armr:mentem"
                }
            }
        ]
    }
```
Obviously, if you have a JSON test file, you can redirect from file instead.

## Test Scripts

A couple of simple client scripts are provided in the `client` subdirectory.
There are python scripts to generate off-line character sheets, a
simple `put.sh` script to test updating of advancements API, and
`putchar.sh` to test updating of other character data.


## Overview

+ Main  - the current version includes a lot of debug displays,
  in addition to the main feature from ArM.WebServices
+ ArM.WebServices OK defines the scotty action which is used in Main.
+ ArM.Load (OK) loads the graph and applies initial reasoners from 
  ArM.Rules and ArM.Rules.*
    - this results in three graphs (g,schema,res), where g contains the 
      character data
+ ArM.Character 
    - This contains the data types and functions to 
        1. load character data from the graph g (above)
	2. advance the characters and produce new graph for each
	   advancement period, containing the character sheet at that time.
    - internal implementation is in ArM.Character.*
+ ArM.CharacterMap is the data store for character sheets.
    - `Main` uses `ArM.Character.getAllCS` to create all the character sheets.
    - These character sheets are stored using `ArM.CharacterMap.insertListS`
    - Before storage, a reasoner is applied to the character sheet
      to add inferred information to make queries easier. 
+ ArM.CharacterQuery OK  This is a very simple set of functions
  to get data a graph from the data store (above).
+ ArM.JSON This module contains all the code to create JSON
  from the objects defined in other modules.
    - **TODO** implement `FromJSON`
+ Auxiliary modules
    + ArM.BlankNode  OK  This is a simple monad to produce unique
      blank nodes based on a serial number.  This is used in ArM.Character
    + ArM.KeyPair  OK  This module collects semi-flexible functions
      to query graphs.  These are used in ArM.Character
+ ArM.Resources This names resources used in the project.
  This is not used consistently
+ ArM.Rules  - Reasoners and their rules.
  See docstring comments for details

## TODO

+ [ ] Handle multiple characters and file output.  #armchar/server/backlog

## References

+ [STM](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board#logging-sessions-cookies-authentication-etc.)  tutorial
