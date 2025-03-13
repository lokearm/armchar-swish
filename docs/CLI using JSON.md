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
	+ [ ] Extract error report in Markdown
	+ [ ] Validate character build
	+ [ ] Clean up MD Character Sheets (pregame vs ingame)
	+ [ ] Refactor and document code
+ [ ] Phase 2. Magus
	+ [x] Step 2. Casting Score
		+ [x] Look up arts and stats
		+ [x] Compute Casting Score
		+ [x] Print Casting Score on character sheet
	+ [ ] Step 3. Review Cieran
		+ [ ] Fix Cieran build
		+ [ ] Review Cieran's ingame advancements
		+ [ ] Add in-game advancements
	+ [ ] Step 4. Lab Total
		+ [ ] Compute Lab Totals
		+ [ ] Print lab totals on character sheet
	+ [ ] Step 5. Finishing touches
		+ [ ] Warping
		+ [ ] Decrepitude
		+ [ ] Aging
		+ [ ] advance age on Winter seasons
		+ [ ] Comment field on traits
	+ [ ]  P/G Char Gen
+ [ ] Phase 3. Covenant
	+ [ ] Step 1. Covenant
		+ [ ] Covenant data model - analogous to Character
		+ [ ] Book resource
		+ [ ] Library
		+ [ ] Read books from CSV
	+ [ ] Step 2. Covenant advancement
		+ [ ] Covenant advancement
		+ [ ] Covenant members
	+ [ ] Step 3. Saga.  Organise files
		+ [ ] Saga object
+ [ ] Phase 4. Polish
	+ [ ] Step 1. Compute Combat Stats
	+ [ ] Step 2. Virtues and Flaws - Special cases
		+ [ ] Flawless magic
		+ [ ] Linguist
		+ [ ] Great/Poor Characteristic
	+ [ ] Step 3.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
	+ [ ] Pretty print spells for reference

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
+ Advancement Types
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
