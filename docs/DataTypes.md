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
