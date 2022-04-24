# Design Notes for armchar-swish

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

## Editable Resources

1.  The Character
    - this is called Metadata in the Haskell code
2.  The Initial CharacterSheet
    - these are traits in place before the advancement process starts
    - Metadata copied from Character should be ignored
3.  The Advancements
    - each Advancement is a separate resource

Additionally, Trait must be handled as part of the Initial CharacterSheet
and of Advancement

## Status

For all the editable resources we have functions for

- Query (retrieval from RDFGraph)
- Internal Haskell Data Type
- JSON Output

We need

1.  JSON Parser
2.  Conversion to RDFGraph 
    - representing those triples that should be overwritten on update
    - same conversion can be used on the old resource (from Query)
      and the new resource (from JSON)
3.  Update of the underlying RDFGraph, this is a delete + addGraphs operation
