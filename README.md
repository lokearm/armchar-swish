# armchar-swish

ArM character server implementation using Haskell and Swish.

The program, `armchar-swish`, is currently only for testing.
It has no real features.

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


## Problems

There are several limitations in Swish compared to Jena
as far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
    - However, reasoning is expensive and only a fraction of the OWL
      and RDFS inferences are actually useful for us.
+ No ready to use function to apply rulesets.
    - However, such generic functions could be expensive.
    - A recursive function 'fwdApplyListR' has been implemented
      to solve this problem and it seems to work well.
+ Rules cannot easily be defined in a separate file in a separate
  rules language.  The focus of Swish has been the script language.
    - Hence, the 'ArM.Rules' module is clunky
+ No JSON-LD support
    - However, not using JSON-LD may make the client a lot easier
      to implement
+ No noValue clause
    - However, the noValue clause make the reasoner expensive.
    - Coding the inference without noValue is more efficient.


## Graph Processing

The following diagram shows the preparation of the graph in 
the `Load` module.
The arrow labels are function names.
The «raw» data objects correspond to files.
Many of the transformations are not fully implemented yet, but
the main principles have been demonstrated.

```
               prepareCS
raw character  --------> preliminary graph --
                                            |
           prepareSchema                    |
raw schema ------------> schema graph ------| merge
                                            |
                    prepareInitialCharacter |
                                            v
raw resources                            Character Graph
    |                  merge                |
    -----------------------------------------
    prepareResources     |
                         | prepareGraph
                         v
               initial graph from ArM.Load.getGraph
                         |
                         | advanceCharacter
                         v
                 character sheet per season
                         |
                         | schemaReasoner
                         v
                 character sheet for client queries
```

+ `prepareCS` makes only a few inferences to simplify future queries
+ `prepareSchema` does subclass inference and similar rules
+ `prepareInitialCharacter` makes the CharacterSheet from the Character
    - character inherit data from covenant or saga
+ `prepareGraph` copies data from the resource graph to make generic
  descriptions available directly in the character sheet
    - trait inherit description from class
    - **TODO** implied traits
      (E.g. Virtues granting abilities or other virtues.)
+ `advanceCharacter` does not currently use `RDFGraph`.
    - The character sheets are generated an internal Haskell type
    - We consider putting the character sheet back into the graph. 
    - Doing the reasoning on the graph is too costly in this case
+ `schemaReasoner` closes the graph under RDFS subClassOf and
  subPropertyOf relations.  It also deduces subproperties of hasTrait,
  by using the class of the object.  **not tested**

### Graphs in use

1.  Character as Loaded from File
    - Base Character
    - Initial Character Sheet
    - Advancement per Season
2.  Resources
3.  ArM Schema (mainly for use with OWL/RDFS reasoners)
5.  Derived Character Sheet with implied traits
4.  Derived Character Sheet per Season
    - this must be generated after the implied traits
    - implied trais may be advanced later

## TODO

### Project 1.  Managing a single character

1. Discuss Web API 
4. Test and review
5. JSON from advancements
6. Profiling
    - speed up reasoner
7. Introduce [STM](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board#logging-sessions-cookies-authentication-etc.) 
     - Web Server - put advancement resource
     - **important** current implementation will suffer from racing conditions
       if multiple clients connect
9. Generate documentation
10. Make LaTeX
    1.  Pull metadata
    2.  Pull Characteristics
11. Spell String Rules

### Project 2.  Managing a covenant/saga

1.  Library resources
    - link books to advancements
    - check for conflicts
2.  Joint advancement log (view)
3.  Other shared resources
4.  Finances
    - low maintenance covenfolk


## Data Management Proposal

1.  Divide the data into editable chunks, e.g.
    - Each character advancement is one chunk
        - pregame advancements may possibly be split later
    - Metadata is one chunk (possibly divided later)
    - Initial character sheet is one or more chunks
        - e.g. characteristics; virtues and flaws; native language; personality traits
2.  Each chunk has 
    1. associated Haskell Datatype
    2. a well-defined query producing a resource graph
    3. one-to-one mapping between the query result graph and the Haskell datatype
    3. one-to-one mapping between the query result graph and JSON
3.  The editor client can do HTTP GET and PUSH on the chunk
4.  On GET the JSON is returned
5.  On PUT the resource is replaced
    - LDGraph delete to remove the old resource
    - LDGraph merge to insert the new graph

## Overview

+ Advancement.hs
+ BlankNode  OK  This is a simple monad to produce unique
  blank nodes based on a serial number
+ Character.hs
+ CharacterMap.hs
+ CharacterQuery OK  This is a very simple set of functions
  to get data from a graph containing a single character sheet.
  It uses the trait data structure form Internal.Trait.
+ Internal.Trait  NB  This should be refactored.  Several
  modules depend on it.
+ JSON OK This module contains all the code to create JSON
  from the objects defined in other modules.
+ Load OK Loads the graph and applies initial reasoners
+ Query  OKish  This module collects semi-flexible functions
  to query graphs.  These are used in other modules
+ Resources.hs This names resources used in the project.
  This is not used consistently
+ Rules  The reasoner(s) is a bit of a mess
    + Rules.Aux
    + Rules.FullGraph
    + Rules.RDFS
    + Rules.Resource
    + Rules.Schema
