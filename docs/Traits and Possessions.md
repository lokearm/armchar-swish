

+ Common Framework for Possession (Equipment) and Trait
	+ same function `advanceTraitList`
+ CombatOption is currently neither
	+ CombatOption should be a trait because it can be added or removed
	+ it never changes directly
	+ the details are always inferred from other traits
+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [x] How do we deal with multipurpose weapons?   #cli/selected
	+  e.g. throwing axe used as hatchet
	+ lance used as two-handed long spear
	+ need separate Weapon instances, since the stats are different
+ [x] Fix the inference rules for Combat Option #cli/selected
	+ [x]  Record skill with weapon
	+ [x] Default skill score of 0 when unskilled @completed(2024-02-03T18:31:10+01:00)
+ [ ] Infer combat option from weapon (this requires a blind node) #cli/backlog 
+ [ ] Mark unskilled combat option specially  #cli/backlog 
+ [x] Add class for Natural Weapons #cli/backlog
+ [ ] Clean up subclasses of Possession #cli/backlog
+ [ ] Do we want to separate Weapon with stats from Weapon as equipment?  #kanban/backlog 
	+ This would allow us to link one piece of weaponry with different weapons using different skills