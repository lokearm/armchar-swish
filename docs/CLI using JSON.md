---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `ArmChar.hs`
+ Dedicated Libraries: `ArM/Char`
+ Also reuses some libraries from the swish model


+ Types
	+ Persistent representation
		+ `ProtoTrait` represents advancement of a trait
		+ `Advancement` represents advancement of a character or covenant
			+ includes a list of `ProtoTrait` advancing individual traits
			+ includes a time (season) or a stage (pregame)
			+ may include a narrative
			+ may include appearance, if it changes
		+ Character represents a character as stored on file (JSON)
			+ glance (brief metadata)
			+ metadata
			+ pregameAdvancement
			+ charAdvancement
+ CharacterState
	+ metadata from Character
	+ state of traits
		+ traits
		+ season
	+ store past and future advancements?
+ CharacterSheet
	+ metadata from Character
	+ season
	+ calculated traits
	+ store past and future advancements?