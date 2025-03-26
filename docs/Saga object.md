---
tags:
  - armchar/json
---

+ How do we manage the Saga?
	+ Advancement
	+ Characters at different stages?
	+ Joint advancement?
	+ Merging of error reports
+ saga is always stored with an initial state
+ advance saga
	+ advance characters in season $N$ first
	+ then advance covenant for season $N$
	+ need to advance one season at a time, in case of inter-dependencies


+ SagaState
	+ season
	+ active and future characters
	+ covenants
	+ retired characters 
	+ (retired covenants)
+ Saga
	+ title
	+ rootDir
	+ list of SagaState - selected snapshots
		+ most recent first
	+ databases 
		+ spells
		+ weapons
		+ armour
