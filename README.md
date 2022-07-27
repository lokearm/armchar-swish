# armchar-swish

ArM character server implementation using Haskell and Swish.

The program, `armchar-swish`, is currently only for testing.
It has no real features.

## Design notes

+ [Design Notes](docs/DesignNotes.md)
+ [Ontology](Ontology/README.md)
+ [Tests](docs/Tests.md) showing current features of the web API
+ [Data Structure](docs/DataStructure.md) 

## Testing

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

## Problems

There are several limitations in Swish compared to Jena
as far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
    - However, reasoning is expensive and only a fraction of the OWL
      and RDFS inferences are actually useful for us.
+ No ready to use function to apply rulesets.
    - However, such generic functions could be expensive.
    - A recursive function `fwdApplyListR` has been implemented
      to solve this problem and it seems to work well.
+ Rules cannot easily be defined in a separate file in a separate
  rules language.  The focus of Swish has been the script language.
    - Hence, the 'ArM.Rules' module is clunky
+ No JSON-LD support
    - However, not using JSON-LD may make the client a lot easier
      to implement
+ No noValue clause
    - However, the noValue clause makes the reasoner expensive.
    - Coding the inference without noValue is more efficient.

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
+ ArM.Rules  The reasoner(s) is a bit of a mess
    + ArM.Rules.Aux
    + ArM.Rules.FullGraph
    + ArM.Rules.RDFS
    + ArM.Rules.Resource
    + ArM.Rules.Schema

## TODO

### Project 1.  Managing a single character

1. Discuss Web API 
2. Test and review
3. Web Server - put advancement resource
4. Generate documentation
5. Spell String Rules
6. Make LaTeX
    1.  Pull metadata
    2.  Pull Characteristics

### Project 2.  Managing a covenant/saga

1.  Library resources
    - link books to advancements
    - check for conflicts
2.  Joint advancement log (view)
3.  Other shared resources
4.  Finances
    - low maintenance covenfolk

## References

+ [STM](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board#logging-sessions-cookies-authentication-etc.)  tutorial
