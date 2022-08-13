# Ontology

Use make to generate the actual ontology (.ttl) files.
Each file is split into multiple parts (.include files) which are
concatenated by make.

The resulting files are
1. arm.ttl - schema for the data model
2. resources.ttl - resources, like lists of abilities, spells, etc.
3. cieran.ttl - sample character
4. contested.ttl - sample saga

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

## Key Classes

1.  Character (Metadata) - read/write
2.  Character Sheet - read only
    - with Trait including data inherited from class
3.  Advancement - read/write
    - with Trait with editable fields

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

# Basic Principles

These principles have been established but refactoring may be
required to ensure full compliance.

1.  Properties that have instances as their domain may have type
    + `arm:PersistentProperty` if they should be stored on file and
      the client should be allowed to change them
    + `arm:CalculatedProperty` if they are derived and should never
      be persisted
    + `arm:ViewProperty` if they should be included in get responses.
      This is a superclass of both persistent and calculated properties.
2.  All standard traits (e.g. as given in the rules) are classes.
    Custom traits may also be classes.  However, the traits of a 
    given character sheets are instances of those classes.
    
