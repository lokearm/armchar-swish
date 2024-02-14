

## Steps of the loading process

1. Load the Saga including schema and resources
	1. Load the Saga object from the saga file (`ArM.Types.Saga`)
	2. Load the schema and resources as specified
	3. Augment the schema and resources using the inference rules
	4. Construct the MapState object containing Saga, Schema, and Resources (`ArM.Types.MapState`)
2. Load the character graphs as specified in the Saga object
	1. Load Character objects using the FromRDF type class (`ArM.Types.Character`)
	2. Apply advancements, building CharGen objects  (`ArM.Character.CharGen`)
	3. Insert the CharGen objects in the MapState Map field 