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
As far as I can tell:

+ No prebuilt OWL and RDFS reasoners.  
+ No ready to use function to apply rulesets.
+ Rules cannot easily be defined in a separate file in a separate
  rules language.  The focus of Swish has been the script language.
+ No JSON-LD support
+ No noValue clause

## Needs for Reasoning

+ Implied Traits defined in Ontology.
  (E.g. Virtues granting abilities or other virtues.)
+ Copy data from other resources
    - abilities inherit description from class
    - character inherit data from covenant or saga
+ Filter on classes.
+ Query

## Graphs

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
    |              merge                    |
    -----------------------------------------
                     |
                     | [implied traits and trait descriptions]
                     v
             initial character sheet
                     |
                     | advanceCharacter
                     v
             character sheet per season
```

In the figure, brackets `[]` indicate description of 
funcions not yet implemented.
Other arrow labels are function names.
The «raw» data objects correspond to files.


## TODO

1. Make game start character (via advancement)
1. Fix the isSpecialTrait property which comes out as URL
1. Discuss Web API 
1. Test and review
    - graph generator
    - advancement code
1. JSON from advancements
6. Web Server - authentication
5. Web Server - put advancement resource
6. Hand-code XP/score calculation rules.
7. Make both hasTrait and subproperties
4. Generate documentation
8. Make LaTeX
    1.  Pull metadata
    2.  Pull Characteristics
9. Spell String Rules

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
