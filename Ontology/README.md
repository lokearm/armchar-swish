# Ontology

Use make to generate the actual ontology (.ttl) files.
Each file is split into multiple parts (.include files) which are
concatenated by make.

The resulting files are
1. arm.ttl - schema for the data model
2. resources.ttl - resources, like lists of abilities, spells, etc.

A sample character is provided in the `/Test` directory.

## Client properties and editable properties

1.  Editable properties can be changed by the client and needs to
   be persisted.
   Examples: `arm:addXP`
   They should have type `arm:PersistentProperty` 
2.  Calculated properties are derived properties that should never 
   be persisted.
   They have type `arm:CalculatedProperty`.
3. Viewable properties should be included in get responses. 
   They have type `arm:ViewProperty` which includes 
   both persistent and calculated properties.
3. Immutable properties are set when the client creates the resource,
   and are persisted.  They cannot be changed.
   Examples: `arm:isCharacter` and `arm:inYear`
   This needs to be revied, and a type defined.
4. Other properties are part of the ontology and should be kept separate
   from character data.
   Examples: type and subclass relations.

Some properties are tricky to manage.  For instance, `arm:hasScore` is
derived for XP based traits, editable for personality traits, and
immutable for virtues and flaws.
Also `arm:hasLabel` is calculated, that is derived from classes, for
most traits, but there are some exceptions which require it to be
of type `arm:PersistentProperty`.

## Key Classes

1.  Character (Metadata) - read/write
    This represents a character with its constant properties.
2.  Character Sheet - read only
    - with Trait including data inherited from class.
    This represents the character at one given time.
3.  Advancement - read/write
    - with Trait with editable fields
    This represents changes to a character at a given time.

## Top Level Entities

+ Core entities
    + `arm:Character`
    + `arm:Advancement`
    + `arm:CharacterSheet` (derived)
+ Other important entities
    + `arm:Trait` Both advancements and character sheets have traits.
    + `arm:Possession` are handled much like traits
+ Less important entities.  
    + `arm:Saga` is only used to hold some shared data which are 
       copied to the character.
    + `arm:Covenant` this is currently similar to `arm:Saga`, but
      will in time be extended to work like a character with advancements
      over time.  `arm:GeneralCharacter` is a superclass comprising
      characters and covenants.
    + `arm:Object` is a superclass of all ArM classes
+ TODO
    + traits and possessions may need a common superclass

## Comment on some properties

+ `arm:hasLabel` and `arm:hasDescription` are used for both
  traits and possessions and both for instances and classes.
  Instances inherit them from classes, but may define their own,
  in which case there is no conflict resolution.
  TODO: they are currently not correctly typed.
    - in the example, `arm:hasLabel` is also used in lieu of
      `arm:hasName` and `arm:hasLabel` for saga and covenant.

# The include files

- arm.include  (reviewed)
- advancement.include
- spell.include
- equipment.include
- trait.include

