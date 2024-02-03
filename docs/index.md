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

+ [ ] [[Covenant]] Support #kanban/backlog 
+ [ ] Calculate Lab Totals    #cli/backlog
+ [x] Calculate Combat Totals    #cli/backlog @completed(2024-02-03T18:38:11+01:00)
+ [ ] Generate Character Sheets at different points in time #cli/backlog 
	+ [ ] Show season on Markdown sheet
+ [ ] Make a grog example and output  #cli/selected 
	+ [ ] Make archer grog with 20 arrows
+ [x] Add equipment to output   #cli/selected 
+ [ ] Revise equipment list  #cli/selected 
	+ [ ] Quantity, Load etc.
	+ [ ] Sort equipment list
	+ [ ] More detailed description
	+ [ ] Review Ontology and use of classes
	+ [ ] Add vis to sample character and have it work
+ [x] Add Header to Markdown output    #cli/backlog @completed(2024-01-31T15:56:40+01:00)
+ [ ] #bug Labels do not appear for natural weapons  #cli/selected 

## Design notes

+ [[CLI Design]].  The following pages are not necessarily exclusive to CLI, but have been written in that context and are up to date.
	+ [[Backend Modules]]
	+ [[CLI Data Model.canvas|CLI Data Model]]  including the Markdown output.  This is accurate and fairly complete as of 2024-02-03
	+ [[CLI Workflow.canvas|CLI Workflow]] 
		+ This is rather crude, but contains some more low level detail than the above.
+ Basic principls, common for all user interfaces.
	+ [[DesignNotes]]
	+ Business [[Logic]]
	+ [[Swish-vs-Jena]]
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

