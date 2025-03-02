---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `ArmChar.hs`
+ Dedicated Libraries: `ArM/Char`
+ Also reuses some libraries from the swish model
+ See also [[Character Generation Process]]
+ 

+ [ ] Step 1.  Working Companion
	+ [ ] Implement Affinity
	+ [ ] Implement Puissant
	+ [ ] Implement XP validation
	+ [ ] Implement VF calculation 
	+ [ ] Implement Characteristics Validation

+ Virtue and Flaws affecting advancement
	+ Affinity
		+ MultiplyXP (Ability "Stealth") 1.5
	+ Warrior, etc.
		+ BonusXP "Later Life" 50 "from Warrior"
	+ Supernatural abilities
		+ ImpliedTrait ( defaultTrait { ability = "Second Sight", xp = 5 })
	+ Skilled Parens
		+ BonusXP "Apprenticeship" 60 "from Skilled Parens"
		+ BonusSpells "Apprenticeship" 60 "from Skilled Parens"
	+ Linguist
	+ Flawless Magic
		+ flag
+ Virtue and Flaws trait calculation
	+ Puissant

+ Modules
	+ `Char.Trait`
		+ Trait
			+  VF (virtues and flaws) may affect other traits
			+ Often affected by virtues and flaws
				+ Ability 
				+  Art
			+ Characteristic
				+ affected by virtues and flaws
				+ may handle manually, but gets the point count wrong
			+  Spell
				+ **NB** Flawless magic
			+ Handle manually
				+  Reputation
				+  PTrait
				+  Confidence
				+  OtherTrait (Warping, Decrepitude)
		+ ProtoTrait
	+ `Char.Character`
		+ Character
			+ CharacterConcept
			+ CharacterState
		+ Advancement
		+ function to create a full starting character
	+ `Char.Advancement`
		+ functions for ingame advancement
	+ `Char.Markdown`
		+ class and instnces for markdown output
+ Types
	+ Character
		+ contains
			+ CharacterConcept (permanent metadata)
			+ CharacterState (current stats)
			+ Pregame Advancement
			+ Past Advancement (most recent first)
			+ Future Advancement (next one first)
		+ advancing a character moves an Advancement from future to past and updates the state accordingly
		+ The character can be persisted at any stage of advancement
		+ Prior states can be recovered by redoing calculations from scratch
		+ Future states can be computed by advancing one Advancement at a time
	+ Constituent types
		+ `ProtoTrait` represents advancement of a trait
		+ `Trait` represents a trait with computed scores
		+ `Advancement` represents advancement of a character or covenant
			+ includes a list of `ProtoTrait` advancing individual traits
			+ includes a time (season) or a stage (pregame)
			+ may include a narrative
			+ may include appearance, if it changes
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