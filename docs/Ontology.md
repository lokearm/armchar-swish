---
tags:
   - armchar/domain/model
---

+ [[RDF Classes.canvas|RDF Classes]]
+ [[Traits and Possessions]]


## Generic features

### Top-Level Classes

1. Traitlike which includes [[Traits and Possessions]]
2. GeneralCharacter which includes Character and Covenant
3. Advancement
4. CharacterSheet which is created from a GeneralCharacter and Advancements
5. Saga which contains very little information at present

#### Traitlike

+ Traitlike is a very generic concept, which is open to abuse and inconsistent use.
+ Generally subclasses of Traitlike represent traits as defined in the rules.
+ Instances of the class represent the trait possessed by an individual at a given time.
+ Many traitlike objects may appear as other things than character traits
	+ Hermetic arts are Traitlike classes which categorise vis
	+ Books teach an art or an ability, which are Traitlike classes
	+ Spells will have to be linked to lab texts
+ It may seem that only Traitlike classes have to be used as other things than character traits
	+ this traitlike classes then behave like objects
### Class membership

+ Special ArM property to identify non-transitive class membership
	+ `arm:traitClass`
	+ `arm:advancementClass`
+ This facilitates some reasoning, being able to identify the direct membership of a key class, without being caught up in generic classes (like `owl:Object`)

### Label  and Description

+ All ArM classes and objects have label and description
+ The label is a short string, suitable for listing
+ The description is a longer description
+ Label and description can be defined both at class and instance level
+ Class level description (`hasDescription` and `hasLabel`) are generic
	+ inherited by every instance
+ Instance level description (`instaceDescription` and `instanceLabel`) are more specific
