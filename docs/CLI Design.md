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
	+ [ ] Quantity, Load etc.
	+ [ ] Sort equipment list
	+ [ ] More detailed description
	+ [ ] Review Ontology and use of classes
	+ [ ] Add vis to sample character and have it work
+ [ ] Make a grog example and output  #cli/selected 
	+ [ ] Make archer grog with 20 arrows
+ [ ] #bug  SkillScore 0 on all CombatOptions #cli/selected 
+ [ ] Support shield and weapon as combat option #cli/selected 



# Wishlist

This is not prioritised in version 1.0, but low-hanging fruits may still be included.

+ [ ] Calculate Lab Totals    #cli/backlog
+ [ ] Generate Character Sheets at different points in time #cli/backlog 
	+ [ ] Show season on Markdown sheet
+ [ ] Handle skill specialisations in combat stats #cli/backlog 
+ [ ] #bug  Inconsistent ordering of virtues and flaws #cli/backlog 
	+ Alphabetical ordering?  Or something cleverer?

# Roadmap

+ [ ] [[Covenant]] Support #kanban/backlog 

# Considerations

+ [ ] Consider distinction between Grog/Companion and Magus #kanban/backlog 
	+ [ ] handle differently in Haskell?
	+ [x] Different output in Markdown
	+ [x] Or drop headers when a section is empty
