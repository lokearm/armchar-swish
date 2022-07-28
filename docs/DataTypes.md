---
title: Data Types
---

# Application Domain Types

+ `Trait`
+ `CharacterSheet`
+ `Advancement`
+ `KeyPairList = KeyPairList [KeyValuePair]`

There are two fundamental concepts in the model,
the `CharacterSheet` and the `Advancement`.

The other two data types are used as constituent data types.
The `KeyPairList` type is used for property/object pairs
(directly corresponding to RDF triples).
to store various properties in composite objects.
Traits occur in both Character Sheets and Advancements.

# Data Processing Types

The following types are defined in `ArM.KeyPair` and used
for intermediate data processing RDF Graphs.

+ `ObjectKeyValue`
+ `KeyValuePairString`
+ `ObjectKeyValueString`

The `MapState` type is defined to support STM.

The following types are defined in `ArM.JSON` to
support serialisation in JSON.

+ `KVP = KVP { prefixedid :: String }`
    + This is used to distinguish prefixed names from URIs.
+ `ProtoAdvancement`
    + This reorganises the data from `Advancement` to allow a neat
      JSON representation.  It has three fields, for ID,
      traits, and other contents (`KeyPairList`).
