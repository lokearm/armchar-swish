

+ Common Framework for Possession (Equipment) and Trait
	+ same function `advanceTraitList`
+ CombatOption is currently neither
+ [[Traits and Possessions in RDF.canvas|Traits and Possessions in RDF]]
+ [x] How do we deal with multipurpose weapons?   #cli/selected
	+  e.g. throwing axe used as hatchet
	+ lance used as two-handed long spear
	+ need separate Weapon instances, since the stats are different
+ [ ] Fix the inference rules for Combat Option #cli/selected
	+ [ ]  Record skill with weapon
	+ [ ] Infer combat option from weapon (this requires a blind node)
	+ [ ] Default skill score of 0 when unskilled
	+ [ ] Mark unskilled specially
+ [ ] Add class for Natual Weapons #cli/backlog
+ [ ] Clean up subclasses of Possession #cli/backlog