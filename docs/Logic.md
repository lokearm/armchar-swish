---
title: Business Logic
---

The data structure organised as an ontology and is divided into three parts.

1.  The schema defines the conceptual relationships between data, and it
    is assumed to be permanent and immutable.
2.  The resource ontology defines concepts that tend to be augmented by
    supplements and sometimes augmented.  The most typical example is 
    lists of abilities, spells, virtues, and flaws, and their descriptions.
    This is assumed to change rarely.
3.  The character data describes individual characters and covenants.
    This is obviously heavily edited by the users.

The character is described by two main concepts.  The character itself 
contains only a few attributes, such as name and player.  The traits are
described by a series of advancements, making it possible to derive a
character sheet at any point in time.

# Overview of reasoning processes

1.  Augmenting by schema.  Adding the schema to the character data makes it
    possible to derive additional type information (super types).  This is 
    important to make subsequent steps work.
2.  Augmenting by resources makes it possible to derive stats and implied
    traits.  It also add descriptions and human-readable labels to the traits.
3.  Advancement.  This is the most important step.  For each advancement, a
    character sheet is derived, accumulating every prior advancement to calculate
    the complete stats of the character at a given point in time.  Among other
    things, XP are accumulated per trait over all advancements.
    + The data structure at this point is crude, with simple code advancing any
      trait without knowing its type.
    + The characters are converted to RDFGraphs and stored individually.
      This makes it possible to use the schema to sort traits into categories.
4.  A reasoner is applied to each character sheet to add convenience properties
    to make it easier to look up specific character data later.
5.  Queries.  The rich ontology, with subclassing of properties and concepts make 
    it possible to tailor queries to get specific pieces of information.
    + The result is converted first to the Trait algebraic data type and then to
      JSON.
    + *Could it be sped up by converting directly from RDF triples to JSON?*
6.  Updates.  The ontology defines what properties should be editable and persistable.
    Queries are used to extract the triples that should be stored from any PUT request.
