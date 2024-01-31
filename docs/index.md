# armchar-swish

ArM character generator implemented in declarative programming
using Haskell and Swish.  Still in early stages of construction.

**Note** There are several concurrent approaches.
+ [[Webserver]] aiming for ReSTful support for a javascript client
    + the client subdirectory contains a simply python client
+ The main line of development at the moment is a standalone CLI interface.

The focus in this repository is a command line tool generating
markdown sheets from an RDF graph (turtle files).

## TODO Lists

+ [[Covenant]] Support
+ [ ] Calculate Lab Totals    #cli/backlog
+ [ ] Calculate Combat Totals    #cli/backlog
+ [x] Add Header to Markdown output    #cli/backlog @completed(2024-01-31T15:56:40+01:00)

## Design notes

+ [[CLI Design]]
	+ [[CLI Workflow.canvas|CLI Workflow]]
+ [[DesignNotes]]
+ Business [[Logic]]
+ [[DataTypes]]
+ [[DataStructure]] 
+ [[Ontology]]
+ [[Swish-vs-Jena]]

## Command line tool

```
( cd Ontology ; make )
cabal run armchar-cli -- -c Test/sylvain.ttl -s Test/saga.ttl -o charactersheet.md
```

There are test files in the `Test` directory.  Please note that the
saga files depend on ontology files and relative paths.  The commands must
be run from the root of the repository.

