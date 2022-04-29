# Ontology

Use make to generate the actual ontology (.ttl) and inference logic (.rules) files.
Each file is split into multiple parts (.include and .r respectively) which are
concatenated by make.

The resulting files are
1. arm.ttl - schema for the data model
2. resources.ttl - resources, like lists of abilities, spells, etc.
3. cieran.ttl - sample character
4. contested.ttl - sample saga
5. logic.rules - main inference rules
6. basic.rules - a few cheap inference rules handled separately

## Client properties and editable properties

Properties in the ontology have to be classified to decide what
to transfer to and from the client.

1.  Editable properties can be changed by the client.
   Examples: `arm:addXP`
2.  Viewable properties are derived properties that should never 
   be persisted, but they need to be transmitted to the client
   for view purposes.
   Examples: `arm:hasScore` for XP based traits.
3.  Immutable properties are set when the client creates the resource,
   and are persisted.  They cannot be changed.
   Examples: `arm:isCharacter` and `arm:inYear`
4.  Other properties are part of the ontology and should be kept separate
   from character data.
   Examples: type and subclass relations.

Some properties are tricky to manage.  For instance, `arm:hasScore` is
derived for XP based traits, editable for personality traits, and
immutable for virtues and flaws.

## Possession Advancement

+ Alternatives
    1.  Chain modifications explicitely
    2.  Create classes even for unique possessions
    3.  Link to prototype
+ Requirements
    1.  Modify description
    2.  Change quantity
    3.  Add/Remove
+ Variations
    1.  Unique Items
    2.  Classed Items with Unique Description
    3.  Standard Items 
+ A.  Countable versus singleton
+ B.  Expendable versus durable

**NOTE.**  Some items are entities in their own right.
