

+ Types.MapState
	+ MapState type holds all the data loaded from disk
	+ `loadSaga` loads the saga RDF file and returns a MapState object
		+ it also loads schema and resources as defined in the saga graph
	+ `loadChar` loads a character RDF file and returns a CharGen object
		+ uses MapState object to build the CharGen object
+ ArM.Types.Saga
+ ArM.Character.CharGen
	+ `makeCharGen` creates the CharGen object from schema, resource, and character graphs
	+ CharStage type
	+ CharGen type
+ 