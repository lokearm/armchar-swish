---
title: Data Types
---

# Application Domain Types

+ `Trait`
+ `CharacterSheet`
+ `Advancement`

There are two fundamental concepts in the model,
the `CharacterSheet` and the `Advancement`.
Traits occur as components in both Character Sheets and
Advancements.

A lot of the constituent data are stored as lists of
`ArM.KeyPair.KeyValuePair`, that is property/object
pairs directly corresponding to the RDFGraph representation.

# Data Processing Types

The following types are defined in `ArM.KeyPair` and used
for intermediate data processing RDF Graphs.

+ `ObjectKeyValue`
+ `KeyValuePairString`
+ `ObjectKeyValueString`

The `MapState` type is defined to support STM.

The following types are defined in `ArM.JSON` to
support serialisation in JSON.

+ `KeyPairList = KeyPairList [KeyValuePair]`
    + The algebraic data type makes it possible to make a class instance
      of the list of `KeyValuePair`
+ `KVP = KVP { prefixedid :: String }`
    + This is used to distinguish prefixed names from URIs.
+ `ProtoAdvancement`
    + This reorganises the data from `Advancement` to allow a neat
      JSON representation.  It has three fields, for ID,
      traits, and other contents (`KeyPairList`).
