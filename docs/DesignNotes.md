---
title: Design Notes for armchar-swish
---

# Design Notes for armchar-swish

1.  The data model is formulated in RDF.
    + Small subset of OWL and RDFS to make the reasoner fast
2.  Each character is stored in a separate file.
    + thus the user can refer to the character as a file, with no need
      to remember a URI
3.  Data files should be stored in a git repo for version control
    + this is not yet implemented
    + could be based on examples from gitit
4.  A complete data model needs to manage
    + one (possibly more) covenants
    + multiple characters
    + possibly a saga
        + the saga can contain a list of the covenants included


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

