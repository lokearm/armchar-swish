

## Steps of the loading process

1. Load the SagaFile
2. Load the LoadedSaga
	1. This loads the scema and resources
3. Load characters into the LoadedSaga
4. Make the Saga object
	1. parse the covenant graph and advance
	2. parse character graphs and advance characters

## Present assumptions

1. The saga is the top-level object and links all other objects.
2. The saga has *one* covenant
3. The saga may have any number of characters, each in a separate file including both the character resource and the advancement resources.
4. The saga has one schema which may comprise multiple files
5. The saga has one resource database which may comprise multiple files.