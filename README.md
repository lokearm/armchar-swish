# armchar-swish

ArM character server implementation using Haskell and Swish.

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
4.  Derived Character Sheet per Season
5.  Derived Character Sheet with implied traits

## TODO

1. get traits for (initial) character sheet
2. Implement advanceCharacter
    - should resulting sheets be pushed into the graph?
3. Make JSON
4. Web Server - get character sheet
5. Web Server - put advancement resource
6. Hand-code XP/score calculation rules.
7. Make both hasTrait and subproperties
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
