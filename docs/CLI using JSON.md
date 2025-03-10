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

+ [ ] Phase 1. Companion/Grog
	+ [ ] Step 1. Finishing touches and bug fixes
		+ [x] Fix XP validation on in game advancement
		+ [x] Confidence
		+ [x] Age
		+ [x] Sort metadata in output 
		+ [ ] Fix CharacterType (not Magus)
		+ [x] Move CharacterType to concept section (not state)
	+ [ ] Step 2.  Review markdown output 
		+ [ ] Character Sheet in Markdown
		+ [ ] Advancement Log in Markdown
		+ [ ] Char Gen Validation in Markdown
		+ [ ] Error report in Markdown
		+ [ ] Validate character build
+ [ ] Phase 2. Magus
	+ [ ] Step 1.  Magi char gen
		+ [ ] Sample file with Cieran
		+ [ ] Spell data type
		+ [ ] Read Spell data from CSV
		+ [ ] Art and spell markdown
	+ [ ] Step 2. Finishing touches
		+ [ ] Warping
		+ [ ] Decrepitude
		+ [ ] Aging
	+ [ ] Step 3.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Review Character State output
		+ [ ] Read CharacterState from JSON
+ [ ] Phase 3. Covenant
	+ [ ] Step 6. Covenant
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
