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

## Reasoner

+ We separate three domains
	+ Persistence; i.e. the graph as stored on file
	+ Calculation; which includes different inference graphs used in analysis
	+ Presentation; i.e. output formats (e.g. Markdown) produced as one-way inference from calculation domain objects
+ There is no need for two-way inference between calculation and presentation
+ We currently only support one-way inference from presisistence to calculation.
	+ Two-way inference would be desirable, particularly for interactive applications, but is tricky to support


## Editable Resources

1.  The Character
    - this is called Metadata in the Haskell code
2.  The Advancements
    - each Advancement is a separate resource
    - the baseAdvancement adds traits before the early childhood, typically virtues and flaws

## Status

For all the editable resources we have functions for

- Query (retrieval from RDFGraph)
- Internal Haskell Data Type
- JSON Output

