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
+ See also [[JSON Char Gen Process.canvas|JSON Char Gen Process]]
+ 

+ [x] Step 1.  Working Companion CharGen
	+ [x] Implement Affinity
	+ [x] Implement Puissant
	+ [x] Include affinity in score calculation
+ [ ] Step 2. Advancement log
	+ [ ] Store augmented Advancement objects
	+ [ ] Markdown output
+ [ ] Step 3. Calculations for validation purposes
	+ [ ] Implement XP Calculation
	+ [ ] Implement VF calculation
	+ [ ] Characteristics Calculation
+ [ ] Step 4. Calculations for validation purposes
	+ [ ] Implement XP validation
	+ [ ] Implement VF validation
	+ [ ] Implement Characteristics Validation
+ [ ] Step 5.  Companion Avancement
+ [ ] Step 6.  CharacterState in JSON
+ [ ] Step 7. Cleanup
	+ [ ] Remove null entries from JSON output
+ [ ] Step 8.  Magi char gen
	+ [ ] Spell data type
	+ [ ] Read Spell data from CSV
	+ [ ] Art and spell markdown
+ [ ] Step 9. Covenant
	+ [ ] Covenant data model - analogous to Character
	+ [ ] Book resource
	+ [ ] Library
	+ [ ] Read books from CSV
+ [ ] Step 10. Covenant advancement
	+ [ ] Covenant advancement
	+ [ ] Covenant members
+ Virtue and Flaws affecting advancement
	+ Affinity
		+ MultiplyXP (Ability "Stealth") 1.5
			+ Implied trait - new multiplyXP attribute
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
			+ Implied trait - new bonusScore attribute

+ Advancement process.
    + Uses State from previous season and Advancement from current season.
        + The State includes virtues and flaws which may modify behaviour.
        + However, implied traits are effective immediately.
    + Advancement may be amended
        + additional XP
        + `prepareAdvancement :: CharacterState -> Advancement -> Advancement`
    + Virtues and flaws add implied traits
        + `inferTraits :: CharacterState -> [ProtoTrait] -> [ProtoTrait]`
        + This handles affinities and puissant
        + Alternative approaces
		    + ProtoTrait may be amended
		        + affinity
		        + `computeAdvancement :: CharacterState -> ProtoTrait -> ProtoTrait`
		    + Trait may be amended
		        + bonusScore
    + Advance trait $\to$  `advance`
+ Validation
	+ note that validation must be based on the augmented advancements
		+ hence the augmented advancements must be stored
        
+ Ideas
	+ [Data.Map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) could be used for CharacterState
	+ How do we represent Puissant (ability), Deft (art), etc.
		+ `appliesTo :: Maybe TraitKey`
		+ `detail :: Maybe String`
		+ `detail = Just "Creo"` $\sim$ `appliesTo :: Just $ Art "Creo"`
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
+ Advancement
	+ as entered by user
		+ season 
		+ totalXP
		+ uses (book)
		+ changes 
	+ maybe InferedAdvancement
	+ derived
		+ inferedTraits
		+ inferedTotalXP
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
