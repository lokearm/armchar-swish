---
title: CLI Design
---

+ [[CLI Data Model.canvas|CLI Data Model]]
+ [[Traits and Possessions]]



# CLI version 1.0

The first version of CLI will solve one simple problem; take a character description in turtle and produce a character sheet in Markdown.

+ [x] #bug Labels do not appear for natural weapons  #cli/selected 
	+ [x] New property/subclass to process Natural Weapons as Traits
	+ [x] Infer Combat Option from Trait or Possession
+ [ ] Revise equipment list  #cli/selected 
	+ [ ] Sort equipment list
	+ [ ] More detailed description
	+ [x] Review Ontology and use of classes
	+ [x] Add vis to sample character and have it work
	+ [x] check and fix addQuantity
	+ [x] display Quantity
	+ [x] check other things that may require 
+ [x] Test non-standardised items of equipment  #cli/selected 
	+ [x] Does quantities keep them separate?
	+ [x] Do they display correctly?
+ [x] Test removal of quantities  #cli/selected 
	+ [x] Basic removal
	+ [x] What happens when quantity is reduced to zero?
+ [ ] Virtues/Flaws taken more than once #cli/backlog 
+ [x] #bug add quantities of same type of item
	+ [x] `Types/Trait` need to recalculate `hasQuantity` upon advancement
+ [ ] Make a grog example and output  #cli/selected 
	+ [x] Make archer grog with 20 arrows
	+ [x] Test removal of arrows
	+ [ ] Make Andrew (grog.ttl) rules compliant
+ [x] Format vis display #cli/selected 
	+ [x] Define and format label
	+ [x] Fix quantity summation (separate task) and validate the vis display
+ [x] #bug  SkillScore 0 on all CombatOptions #cli/selected 
+ [ ] Support shield and weapon as combat option #cli/selected 



# Wishlist

This is not prioritised in version 1.0, but low-hanging fruits may still be included.

+ [ ] Calculate Lab Totals    #cli/backlog
+ [ ] Generate Character Sheets at different points in time #cli/backlog 
+ [x] Show season on Markdown sheet
+ [ ] Show age on pre-game characters #cli/backlog 
+ [ ] Handle skill specialisations in combat stats #cli/backlog 
+ [ ] #bug  Inconsistent ordering of virtues and flaws #cli/backlog 
	+ Alphabetical ordering?  Or something cleverer?
+ [ ] Equipment load #cli/backlog 
	+ [ ] load per item
	+ [ ] total load (multiplied by quantity) per line
	+ [ ] aggregate load and encumbrance
+ [ ] Current year and season on one line #cli/backlog 

# Roadmap

+ [ ] [[Covenant]] Support #kanban/backlog 
+ [ ] Advancement log (new output format)  #cli/backlog 

# Considerations

+ [ ] Consider distinction between Grog/Companion and Magus #kanban/backlog 
	+ [ ] handle differently in Haskell?
	+ [x] Different output in Markdown
	+ [x] Or drop headers when a section is empty
+ [ ] Remove items with zero quantity #cli/backlog 
