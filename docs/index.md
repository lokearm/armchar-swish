---
tags:
  - armchar
aliases:
  - "#armchar"
title: armchar
---

# armchar

ArM character generator implemented in declarative programming
using Haskell.  This is still early stage development, and I am
experimenting with different approaches.

1. Using ontologies and swish
    1. [[Webserver]] Model
    1. [[CLI with Swish]]
2. [[CLI using JSON]]


## Ars Magica  Notes

+ [[Character Generation Process]]
## Technical Notes

+ On #haskell
    + [[Profiling]]

## Design with the Ontology Approach

+ [[Webserver]] aiming for ReSTful support for a javascript client
    + the client subdirectory contains a simple python client
	+ The main line of development at the moment is a standalone CLI interface.
+ [[CLI Design]].  The following pages are not necessarily exclusive to CLI, but have been written in that context and are up to date.
	+ [[Loading RDF Graphs]]
	+ [[Design Notes]]
	+ [[Backend Modules]]
	+ [[CLI Data Model.canvas|CLI Data Model]]  including the Markdown output.  This is accurate and fairly complete as of 2024-02-03
	+ [[CLI Workflow.canvas|CLI Workflow]] 
		+ **Outdated**
		+ This is rather crude, but contains some more low level detail than the above.
	+ [[Advancement]]
+ Basic principls, common for all user interfaces.
	+ Business [[Logic]]
	+ [[Swish-vs-Jena]]
	+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [[DataTypes]]
	+ This is written with reference to the web server, but some parts may still be informative wrt CLI
+ [[Ontology]]
