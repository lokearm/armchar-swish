

## Steps of the loading process

1. Load the MapState object
	1. Load the Saga 
		1. Load the graph from the saga file (`ArM.Types.MapState`)
		2. Extract the Saga object from the RDF Graph (`ArM.Types.Saga`)
	2. Load the schema and resources as specified in the Saga object  (`ArM.Types.MapState`)
		3. Augment the schema and resources using the inference rules
	3. Construct the MapState object containing Saga, Schema, and Resources (`ArM.Types.MapState`)
3. Load the character graphs as specified in the Saga object
	1. Load Character objects using the FromRDF type class (`ArM.Types.Character`)
	2. Apply advancements, building CharGen objects  (`ArM.Character.CharGen`)
	3. Insert the CharGen objects in the MapState Map field 