---
title: Data Types
---

# Algebraic Data Types

## Character definition

+ `Trait`
+ `CharacterSheet`
+ `Advancement`
+ `KeyPairList = KeyPairList [KeyValuePair]`
+ `Character`

There are two fundamental concepts in the model,
the `CharacterSheet` and the `Advancement`.

The other two data types are used as constituent data types.
The `KeyPairList` type is used for property/object pairs
(directly corresponding to RDF triples).
to store various properties in composite objects.
Traits occur in both Character Sheets and Advancements.

## Other Application Domain Types

+ `Saga`
+ `CharTime`

## Data Management Types

+ `MapState`
+ `CharGen` and `CharStage`

The `MapState` type is defined in `ArM.STM` to hold all the data
stored in Software Transactional Memory.

Finally, `Character` is used for the metadata.  The initial
`CharacterSheet` prior to every `Advancement` is generated from
a `Character` object.

## Internal Processing Types


+ `KVP = KVP { prefixedid :: String }`
    + This is used to distinguish prefixed names from URIs.
    + Used only internally in `ArM.Types.RDF`.
+ `ProtoAdvancement`
    + This reorganises the data from `Advancement` to allow a neat
      JSON representation.  It has three fields, for ID,
      traits, and other contents (`KeyPairList`).
    + Used only internally in `ArM.Types.Advancement`.
+ `CharacterKey` used internally in `ArM.Character.CharGen`

# Conversions

## Initial Loading

1.  `ArM.STM` loads the Saga object.
2.  `ArM.STM` loads other graphs as specified in the Saga object.
3.  All graphs are stored in `TVar` objects (STM). 
    All data are referenced from a single `MapState` object.
4.  RDF Reasoning is used to augment the graphs.
5.  For each character graph, a `CharGen` object is generated
    to hold `CharacterSheet` objects for every time step.
    See below.

## Character Generation

This is handled by the `ArM.Character.CharGen` module.

1.  To create character sheets, the RDF Graph is converted into
    Algebraic datatypes.
2.  For each time step, a `CharStage` object is created, containing
    the Advancement and Character Sheet as Algebraic Data Types as
    well as a RDFGraph representation of the CharacterSheet.
3.  The `CharStage` Objects are created successively by applying
    Advancements in order.
4.  The `CharStage` objects are collected in a `CharGen` object
    together with the raw graph (from file), the augmented graph,
    and the initial character sheet both as RDFGraph and as
    `CharacterSheet`.
5.  The `CharGen` object is stored in a map in the `MapState` object.

This way the data is always represented twice.  Once as an internal
data type and once as RDFGraph.

## Other constituent objects

1.  During the `CharGen` creation, the subgraph defining the character
    metadata is extracted from the full graph by
    `ArM.Types.Character.extractBaseCharacterGraph`.
    + This is used to reconstruct the full graph when advancements are 
      added or updated.

## Retrieval

1.  Character Metada are retrieved using fromRDFGraph on the full character graph,
    and then converting this to JSON implicitly in the Scotty `json` function.
    - The Character could here be replaced by a KeyPairList, but it is not clear
      that this would be advantageous.

## Updates

### Character Metadata

1.  Input is converted to a graph using `makeRDFgraph`.
2.  The reasoner is used to get persistent properties.
3.  The base graph is replaced in the CharGen object.
4.  TODO: regenerate CharStage objects with new metadata

### Advancements

# Questions and Caveats

1.  Can the `Character` object be made redundant or internal to 
    `ArM.Types.Character`?  It seems to be used once elsewhere,
    in `putCharacter`.
2.  Updating metadata is unnecessarily slow because CharStage objects must
    be regenerated.
