---
tags:
  - armchar/json
  - combat-stats
---

+ Combat Statistics depend on several objects
	+ `Possession` representing equipment including weapons and other items serving as weapons
	+ `Weapon` representing the weapon statistics of an item or of natural weapons. Standard `Weapon` objects are retrieved from CSV and stored in a `WeaponDB`
	+ `CombatOption` representing an actual application of weapons recorded with stats on the character sheet.
+ Note
	+ one `Possession` may have several `Weapon` objects, representing use with different skills
		+ a throwing knife or axe can be used in melee
		+ a lance can be used single handed on horseback or two-handed on foot
	+ one `CombatOption` may use a shield in addition to the main weapon, and the sheild too is represented as a `Possession` with `Weapon` stats
	+ a `CombatOption` may apply a `Possession` or a natural weapons which are drawn from the `WeaponDB`
	+ a `Possession` may reference combat stats from the `WeaponDB` or have its own unique stats
+ These observations show that all three objects are required.
	+ Weapon stats are stored with unique items (with bonuses) to make sure they are transferred with the item. 
	+ Weapon stats  only needs to be included in the character file when the item is exceptional

+ Combat stats depend on
	+ Ability
	+ Characteristics
	+ Equpment
+ Challenges
	+ Dual use weapons
	+ Natural weapons
	+ Shields
	+ Unique and standard weapons
+ Combat Option
	+ `[ Either Weapon StandardItem ]`
	+ `Ability`
	+ Should it link to possession or to weapon stats

+ Dual use
	+ hatchet / thrown axe
	+ lance / longspear
	+ javelin / shortspear