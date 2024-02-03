

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
+ [ ] which rules are used to build CharGen?
+