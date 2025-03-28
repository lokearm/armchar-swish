---
tags:
  - armchar/json/cli
aliases:
  - "#armchar/json/cli"
---
+ Main file: `ArmChar.hs`
+ Dedicated Libraries: `ArM/Char`
+ Also reuses some libraries from the swish model
+ Design notes
	+ [[Character Generation Process]]
	+ [[JSON Char Gen Process.canvas|JSON Char Gen Process]] (canvas)
	+ [[Combat Stats]]
	+ [[Types for JSON]]



## Roadmap
+ [ ] Phase 1. Review and bugfix
	+ [ ] Review
		+ [ ] Print and Double-Check Cieran
		+ [ ] Print and Double-Check Eogan
	+ [ ] Web site
		+ [ ] Page and directory structure for github pages
		+ [ ] Build website in github runners
	+ [ ] Finishing touches
		+ [ ] Vis accounting
		+ [ ] Speciality in combat totals
	+ [ ] Refactor
		+ [ ] Clean up Saga module
+ [ ] Phase 2. Improvements
	+ [ ] Step 3. Characteristics
		+ [ ] Great/Poor Characteristic (current warning)
		+ [ ] Handle multiple instances of the same virtue/flaw
	+ [ ] Step 6. Decrepitude
		+ [ ] make old grog test file
		+ [ ] verify
+ [ ] Phase 3. Covenant
	+ [ ] Covenant data model - analogous to Character
	+ [ ] Step 1. Covenant
		+ [ ] Book resource
		+ [ ] Library
		+ [ ] Read books from CSV
	+ [ ] Step 2. Covenant advancement
		+ [ ] Covenant advancement
		+ [ ] Covenant members
+ [ ] Phase 4. Polish
	+ [ ] Refactor and document code
	+ [ ] Comment field on traits
	+ [ ]  P/G Char Gen
	+ [ ] Remove trait when advancing
	+ [ ] Step 3. Virtues and Flaws - Special cases
		+ [ ] Linguist
		+ [ ] Inventive Genius
		+ [ ] Infer Second Sight from Strong Faerie Blood
	+ [ ] Step 4.  CharacterState in JSON
		+ [ ] Remove null entries from JSON output
		+ [ ] Read CharacterState from JSON
	+ [ ] Count xp total (ingame) for validationparallel
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
