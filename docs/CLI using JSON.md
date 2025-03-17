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
+ See also [[JSON Char Gen Process.canvas|JSON Char Gen Process]] (canvas
+ [[Types for JSON]]


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
+ [ ] Phase 1. Review and bugfix
	+ [ ] Validate Eogan build
	+ [ ] Flawless magic
	+ [ ] Pretty print grimoire with details from DB
	+ [ ] Review Cieran's ingame advancements
	+ [ ] Update Characteristic
		+ [ ] advanceTrait function
		+ [ ] Cieran's changes
	+ [ ] Warping
		+ [ ] Check for LR and warp in Winter
		+ [ ] Check warping for Cieran
	+ [ ] Aging
		+ [ ] validate presence of aging roll in Winter
		+ [ ] check aging points on characteristics
		+ [ ] Record apparent age
		+ [ ] Decrepitude
	+ [ ] Compute Combat Stats
+ [ ] Phase 3. Covenant
	+ [ ] Step 1. Covenant
		+ [ ] Covenant data model - analogous to Character
		+ [ ] Book resource
		+ [ ] Library
		+ [ ] Read books from CSV
	+ [ ] Step 2. Covenant advancement
		+ [ ] Covenant advancement
		+ [ ] Covenant members
	+ [ ] Develop [[Saga object]]
	+ [ ] Hyperlinked web site
+ [ ] Phase 4. Polish
	+ [ ] Refactor and document code
	+ [ ] Comment field on traits
	+ [ ]  P/G Char Gen
	+ [ ] Remove trait when advancing
	+ [ ] Step 3. Virtues and Flaws - Special cases
		+ [ ] Linguist
		+ [ ] Great/Poor Characteristic
		+ [ ] Inventive Genius
		+ [ ] Infer Second Sight from Strong Faerie Blood
	+ [ ] Step 4.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
	+ [ ] Pretty print spells for reference
	+ [ ] Count xp total (ingame) for validationparallel
	+ [ ] Process characters in 
+ [ ] Phase 5. Integer XP
	+ [ ] handle Correspondent (which goes beyond cap)

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
