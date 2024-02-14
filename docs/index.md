# armchar-swish

ArM character generator implemented in declarative programming
using Haskell and Swish.  Still in early stages of construction.

**Note** There are several concurrent approaches.
+ [[Webserver]] aiming for ReSTful support for a javascript client
    + the client subdirectory contains a simply python client
+ The main line of development at the moment is a standalone CLI interface.

The focus in this repository is a command line tool generating
markdown sheets from an RDF graph (turtle files).

## Design notes

+ [[CLI Design]].  The following pages are not necessarily exclusive to CLI, but have been written in that context and are up to date.
	+ [[Loading RDF Graphs]]
	+ [[Design Notes]]
	+ [[Backend Modules]]
	+ [[CLI Data Model.canvas|CLI Data Model]]  including the Markdown output.  This is accurate and fairly complete as of 2024-02-03
	+ [[CLI Workflow.canvas|CLI Workflow]] 
		+ This is rather crude, but contains some more low level detail than the above.
	+ [[Advancement]]
+ Basic principls, common for all user interfaces.
	+ Business [[Logic]]
	+ [[Swish-vs-Jena]]
	+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [[DataTypes]]
	+ This is written with reference to the web server, but some parts may still be informative wrt CLI
+ [[Ontology]]

## Command line tool

```
( cd Ontology ; make )
cabal run armchar-cli -- -c Test/sylvain.ttl -s Test/saga.ttl -o charactersheet.md
```

There are test files in the `Test` directory.  Please note that the
saga files depend on ontology files and relative paths.  The commands must
be run from the root of the repository.

