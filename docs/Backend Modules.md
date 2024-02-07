

+ Types.MapState (used by CLI, not by Webserver)
	+ MapState type holds all the data loaded from disk
	+ `loadSaga` loads the saga RDF file and returns a MapState object
		+ it also loads schema and resources as defined in the saga graph
		+ **Rule inferences used** `prepareSchema`  and `prepareResources`
	+ `loadChar` loads a character RDF file and returns a CharGen object
		+ uses MapState object to build the CharGen object
+ ArM.Types.Saga
+ ArM.Character.CharGen
	+ `makeCharGen` creates the CharGen object from schema, resource, and character graphs
	+ CharStage type
	+ CharGen type
	+ **Rule inferences used** `makeGraph`  and `prepareRecord`
+ ArM.Types.SheetObject
	+ SheetObject type holds all the data, completely calculated, to make a sheet at a given point in time.
	+ **Rule inference used** `calculateSheet`
+ ArM.MarkDown.CharacterSheet
	+ This module produces markdown output from SheetObject instances.

# Code Review 

+ [x] BlankNode
    + Simple monad to create distinct blank nodes
+ [ ] Character.CharGen
+ [ ] CharacterQuery
    + [ ] Rename to Character.Query?
+ [x] Debug.*
    + Simple functions to print diagnostic output
+ [x] Internal.Aux
    + Implements `uniqueSort` 
+ [x] IO
    + simple function to read and parse a turtle file from disk
+ [ ] KeyPair
+ [ ] Markdown.CharacterSheet
+ [x] (not used) Markdown.Debug
+ [ ] Resources
    + Simple definitions of RDF resources.  
    + Some are important.
    + Some are barely used and could be expanded for each use 
+ [ ] Rules.*
+ [ ] Rules
    + wrapper for most submodules under Rules
+ Server.*
    + This is the WebServer, not currently developed
+ Types.*

+ [ ] Should more things be moved under Internal?
    + [ ] IO
    + [ ] BlankNode
