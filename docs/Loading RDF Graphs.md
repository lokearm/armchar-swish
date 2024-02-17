

## Steps of the loading process

1. Load the MapState object
	1. Load the Saga 
		1. Load the graph from the saga file (`ArM.Types.MapState`)
		2. Extract the Saga object from the RDF Graph (`ArM.Types.Saga`)
	2. Extract the Covenant object from the same graph.  Note that the covenant is a character and uses largely the same functions.
	3. Load the schema and resources as specified in the Saga object  (`ArM.Types.MapState`)
		3. Augment the schema and resources using the inference rules
	4. Construct the MapState object containing Saga, Schema, and Resources (`ArM.Types.MapState`)
3. Load the character graphs as specified in the Saga object
	1. Load Character objects using the FromRDF type class (`ArM.Types.Character`)
	2. Apply advancements, building CharGen objects  (`ArM.Character.CharGen`)
	3. Insert the CharGen objects in the MapState Map field 

## Present assumptions

1. The saga is the top-level object and links all other objects.
2. The saga has *one* covenant
3. The saga may have any number of characters, each in a separate file including both the character resource and the advancement resources.
4. The saga has one schema which may comprise multiple files
5. The saga has one resource database which may comprise multiple files.