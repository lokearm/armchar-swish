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
+ See also [[JSON Char Gen Process.canvas|JSON Char Gen Process]] (canvas)

## Command Line Syntax

```
armchar -- -c eogan.json -o eogan.md -O test.md -j test.json   && pandoc -o eogan.pdf eogan.md  
```

+ `-c FILE` read the character from `FILE`
+ `-o FILE` write a formatted character sheet at Game Start to `FILE`
+ `-j FILE` write character state as calculated at Game Start to `FILE` in JSON
+ Debug options
	+ `-O FILE` write markdown of the character before the sheet is computed

## Roadmap

+ [x] Step 1.  Working Companion CharGen
	+ [x] Implement Affinity
	+ [x] Implement Puissant
	+ [x] Include affinity in score calculation
+ [x] Step 2. Advancement log
	+ [x] Store augmented Advancement objects
	+ [x] Markdown output
+ [x] Step 3. Calculations for validation purposes
	+ [x] Implement XP Calculation
	+ [x] Implement VF Calculation
	+ [x] Characteristics Calculation
+ [x] Step 4. Calculations for validation purposes
	+ [x] Implement XP validation
	+ [x] Implement VF validation
	+ [x] Implement Characteristics Validation
+ [x] Step 5. Augment pregame advancements with different limits
	+ [x] Outline functions for pregame and ingame augmentation
	+ [x] Implement functions for pregame augmentation
		+  Need to pass the initial Virtue/flaw list on to the entire process
		+ Should not use CharacterState for the pregame augmentation
	+ [x] XP limits for different advancement types
	+ [x] extra XP from virtues
	+ [x] Improved characteristics
	+ [x] XP for later life
+ [x] Step 6. Refactor and treat grogs differently
	+ [x] Introduce a CharacterSheet object which can be passed along advancement processing
	+ [x] Virtues and flaws for grogs
+ [x] Step 7.  Clean up character sheet
	+ [x] Move characteristics limit to CharacterSheet
	+ [x] Markdown for CharacterSheet
+ [ ] Step 8.  Companion Advancement
	+ [x] advanceCharacter function
	+ [x] Fix ordering on CharTime
	+ [x] Test output
	+ [x] Pretty advancement output
	+ [ ] Fix XP validation on in game advancement
+ [ ] Step 9. Finishing touches
	+ [ ] Warping
	+ [ ] Decrepitude
	+ [ ] Confidence
	+ [ ] Aging
	+ [ ] Age
+ [ ] Step 10.  More metadata
	+ [x] Quirk
	+ [x] Appearance
	+ [ ] Sort metadata in output 
+ [ ] Step 11.  Review markdown output 
	+ [ ] Character Sheet in Markdown
	+ [ ] Advancement Log in Markdown
	+ [ ] Char Gen Validation in Markdown
	+ [ ] Error report in Markdown
	+ [ ] Validate character build
+ [ ] Step 12.  CharacterState in JSON
	+ [ ] Remove null entries from JSON output
	+ [ ] Review Character State output
	+ [ ] Read CharacterState from JSON
+ [ ] Step 13.  Magi char gen
	+ [ ] Spell data type
	+ [ ] Read Spell data from CSV
	+ [ ] Art and spell markdown
+ [ ] Step 14. Covenant
	+ [ ] Covenant data model - analogous to Character
	+ [ ] Book resource
	+ [ ] Library
	+ [ ] Read books from CSV
+ [ ] Step 15. Covenant advancement
	+ [ ] Covenant advancement
	+ [ ] Covenant members

+ Virtue and Flaws trait calculation
	+ Linguist
	+ Puissant
			+ Implied trait - new bonusScore attribute
	+ Skilled Parens
		+ BonusXP "Apprenticeship" 60 "from Skilled Parens"
		+ BonusSpells "Apprenticeship" 60 "from Skilled Parens"

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
    + Advance trait $\to$  `advance`
+ Validation
	+ note that validation must be based on the augmented advancements
		+ hence the augmented advancements must be stored
+ Advancement Types
	+ Pregame
		+ Virtues and Flaws
			+ 0 years
		+ Characteristics
			+ 0 years
		+ Early Childhood
			+ 5 years
		+ Later Life
			+ $X$ years
		+ Apprenticeship
			+ 15 years
		+ P/G Years
			+ $X$ years
	+ Ingame Season
		+ Adventure - fixed SQ + Independent study
		+ Practice - fixed SQ + Independent study
		+ Teaching - validate SQ against teacher
		+ Training - validate SQ against trainer
		+ Reading - get SQ from book
		+ Vis study - fixed SQ + free study
		+ Exposure - fixed SQ
	+ SQ bonuses
		+ correspondent
		+ study bonus
	 
       
+ Specail cases to review
	+ **NB** Flawless magic
+ Modules
	+ `Char.Trait`
		+ Trait
		+ ProtoTrait
	+ `Char.Character`
		+ Character
			+ CharacterConcept
			+ CharacterState
		+ Advancement
	+ `Char.Advancement`
	+ `Char.Markdown`
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
+ CharacterState
	+ state of traits
		+ traits
		+ season
+ CharacterSheet
	+ Frontend for CharacterState
