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
+ [x] Revise equipment list  #cli/selected 
	+ [x] Sort equipment list (postponed)
	+ [x] More detailed description
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
+ [x] #bug add quantities of same type of item
	+ [x] `Types/Trait` need to recalculate `hasQuantity` upon advancement
+ [x] Make a grog example and output  #cli/selected 
	+ [x] Make archer grog with 20 arrows
	+ [x] Test removal of arrows
	+ [x] Make Andrew (grog.ttl) rules compliant
+ [x] Format vis display #cli/selected 
	+ [x] Define and format label
	+ [x] Fix quantity summation (separate task) and validate the vis display
+ [x] #bug  SkillScore 0 on all CombatOptions #cli/selected 
+ [x] Support shield and weapon as combat option #cli/selected 
+ [x] Generate every character from saga resource #cli/backlog 
+ [x] Show season on Markdown sheet
+ [x] Advancement log (new output format)  #cli/backlog 

+ [ ] Covenant markdown output #cli/selected 
+ [ ] Virtues/Flaws taken more than once #cli/selected 
	+ [ ] Test a character with Silent Magic twice
	+ [ ] Test a character with two different Art affinities
+ [ ] Covenant name does not show because the covenant data is not included when inferring character data  #cli/selected #bug 
+ [ ] [[Covenant]] Support #cli/selected 
	+ [x] Suppress size and confidence
	+ [x] Make separate section for Library
	+ [ ] Infer labels for books
	+ [ ] Display all stats for books
	+ [ ] Show virtues and flaws as boons and hooks on covenants
+ [ ] Make resource listings, as reference catalogue #kanban/backlog 

# Usage Notes

+ Equipment
	+ One can use a generic class, e.g. `arm:Equipment` and set `arm:hasLabel`.
	+ Different labels will be treated as different items
	+ Labels must be unique, adding a new item with the same label and a different detail (`arm:hasDetail`) will simply change the detail and quantity of the existing item

# Roadmap

This is not prioritised in version 1.0, but low-hanging fruits may still be included.


+ [ ] Saga printout #cli/backlog 
	+ [ ] Display links to other files
	+ [ ] Display Saga Title
+ [ ] quantity when advancing possessions #cli/backlog 
+ [ ] art on vis in advancement log #cli/backlog 
+ [ ] new TraitProperty hasTrait - do we want to generalise existing properties? #cli/backlog 
	+ [ ]  puissant/affinity (bonusTo)
	+ [ ] Vis (isVisOfArt)
	+ [ ] book (appliesTo)
+ [ ] Advancement by reading #cli/backlog 
	+ [x] Covenant as a character
	+ [x] Covenant hasBook property to define library
	+ [x] Book as RDF resources with stats
	+ [ ] Advancement readsBook property
	+ [ ] Deduce XP gain from book
	+ [ ] Implement and debug relevant virtues
		+ [ ] book reader
		+ [ ] study bonus
		+ [ ] affinity
+ [ ] Lab work advancement type #cli/backlog 
	+ [ ] Calculate Lab Totals    #cli/backlog
	+ [ ] Validate lab total
	+ [ ] infer exposure
+ [ ] Generate Character Sheets at different points in time #cli/backlog 
+ [ ] Show age on pre-game characters #cli/backlog 
+ [ ] Handle skill specialisations in combat stats #cli/backlog 
	+ [ ] make a string property
	+ [ ] hasSpeciality should refer to a class (weapon/ability/art)
+ [ ] Review ordering of items #cli/backlog 
	+ [ ] #bug  Inconsistent ordering of virtues and flaws #cli/backlog 
	+ Alphabetical ordering?  Or something cleverer?
+ [ ] Equipment load #cli/backlog 
	+ [ ] load per item
	+ [ ] total load (multiplied by quantity) per line
	+ [ ] aggregate load and encumbrance
+ [ ] Current year and season on one line on output character sheet   #cli/backlog 
+ [ ] Allow Unique pieces of Equipment (incl. Weapons)  #cli/backlog 
+ [ ] Code review and simplification  #cli/backlog 
+ [ ] Ontology review and simplification  #cli/backlog 
+ [ ] queries account for 91% of the run time; review to see if some calls can be simplified #cli/backlog 
+ [ ] Validate and complete characters #cli/backlog 
	+ [ ] Sylvain
	+ [ ] Marcus
	+ [ ] Grog
- [ ] affinity mechanics #cli/selected 
    + xp-factor - analogous to bonus
    +  applied
+ [ ] elementalist mechanics  #cli/backlog 
    - ad hoc
    - create extra advancement resource



# Considerations

+ [ ] Consider distinction between Grog/Companion and Magus #kanban/backlog 
	+ [ ] handle differently in Haskell?
	+ [x] Different output in Markdown
	+ [x] Or drop headers when a section is empty
+ [ ] Remove items with zero quantity #cli/backlog 
