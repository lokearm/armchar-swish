

### SAGA

# Inverse properties, not inferred by the Micro OWL reasoner
[ covenantsaga:
  ( ?c arm:hasSaga ?s ) ( ?c rdf:type arm:Covenant ) 
  -> ( ?s arm:hasCovenant ?c ) ]
[ charactersaga:
  ( ?c arm:hasSaga ?s ) ( ?c rdf:type arm:BaseCharacter ) 
  -> ( ?s arm:hasCharacter ?c ) ]

# Infer covenant/saga/sg name 
[ covenantname:
  ( ?c arm:hasCovenant ?cov ) ( ?cov arm:hasName ?n ) 
  -> ( ?c arm:hasCovenantName ?n ) ]
[ addsg: ( ?c arm:hasSaga ?s ) ( ?s arm:hasSG ?sg ) -> ( ?c arm:hasSG ?sg ) ]
[ addsaga: 
  ( ?c rdf:type arm:Character ) ( ?c arm:hasSaga ?s ) ( ?s arm:hasTitle ?t ) 
  -> ( ?c arm:hasSagaTitle ?t ) ]

### CHARACTER

# Size Attribute (simpler than treating it as a trait)
[ addsize: 
  ( ?c rdf:type arm:Character ) ( ?c arm:hasOtherTrait ?s ) 
  ( ?s rdf:type arm:Size ) 
  ( ?s arm:hasScore ?size ) 
  -> ( ?c arm:hasSize ?size ) ]

# Character sheet inherits from the base character
[ charsheet1: ( ?c rdf:type arm:CharacterSheet )
             ( ?c arm:isCharacter ?b ) ( ?b ?p ?o )
	     ( ?p rdfs:domain arm:GeneralCharacter )
	     -> ( ?c ?p ?o ) ]
[ charsheet2: ( ?c rdf:type arm:CharacterSheet )
             ( ?c arm:isCharacter ?b ) ( ?b ?p ?o )
	     ( ?p rdfs:domain arm:Character )
	     -> ( ?c ?p ?o ) ]

### TRAITS


# Abilities and Arts (XP)

[ artscore:
  ( ?s rdf:type arm:AccelleratedTrait )
  -> [ ( ?s arm:hasScore ?score ) <-
    ( ?s arm:hasTotalXP ?xp )
    ( ?e armpyramid:xp ?xp )
    ( ?e armpyramid:artScore ?score )
    ( ?e rdf:type armpyramid:xpTableEntry )
  ]
  [ ( ?s arm:hasXP ?rem ) <-
    ( ?s arm:hasTotalXP ?xp )
    ( ?e armpyramid:xp ?xp )
    ( ?e armpyramid:artRemainder ?rem )
    ( ?e rdf:type armpyramid:xpTableEntry )
 ]
 ]
[ abscore:
  ( ?s rdf:type arm:XPTrait )
  -> [ ( ?s arm:hasScore ?score ) <-
  ( ?s arm:hasTotalXP ?xp )
  ( ?e rdf:type armpyramid:xpTableEntry )
  ( ?e armpyramid:xp ?xp )
  ( ?e armpyramid:abScore ?score )
  ] [ ( ?s arm:hasXP ?rem ) <-
  ( ?s arm:hasTotalXP ?xp )
  ( ?e rdf:type armpyramid:xpTableEntry )
  ( ?e armpyramid:xp ?xp )
  ( ?e armpyramid:abRemainder ?rem )
	]
 ]

# Virtues and Flaws
[ majorvirtuescore:
   ( ?v rdf:type armr:majorVirtue )
   noValue(?v,arm:hasScore)
   -> ( ?v arm:hasScore '+3'^^xsd:int ) ]
[ minorvirtuescore:
   ( ?v rdf:type armr:minorVirtue )
   noValue(?v,arm:hasScore)
   -> ( ?v arm:hasScore '+1'^^xsd:int ) ]
[ freevirtuescore:
   ( ?v rdf:type armr:freeVirtue )
   noValue(?v,arm:hasScore)
   -> ( ?v arm:hasScore '0'^^xsd:int ) ]
[ majorflawscore:
   ( ?v rdf:type armr:majorFlaw )
   noValue(?v,arm:hasScore)
   -> ( ?v arm:hasScore '-3'^^xsd:int ) ]
[ minorflawscore:
   ( ?v rdf:type armr:minorFlaw )
   noValue(?v,arm:hasScore)
   -> ( ?v arm:hasScore '-1'^^xsd:int ) ]

# Spells

[ spellrange:
   ( ?s rdf:type arm:Spell ) ( ?s arm:hasRange ?o ) ( ?o rdfs:label ?st )
   -> ( ?s arm:hasRangeString ?st ) ]
[ spellduration:
   ( ?s rdf:type arm:Spell ) ( ?s arm:hasDuration ?o ) ( ?o rdfs:label ?st )
   -> ( ?s arm:hasDurationString ?st ) ]
[ spelltarget:
   ( ?s rdf:type arm:Spell ) ( ?s arm:hasTarget ?o ) ( ?o rdfs:label ?st )
   -> ( ?s arm:hasTargetString ?st ) ]
[ spelltech:
   ( ?s rdf:type arm:Spell ) ( ?s arm:hasTechnique ?o ) ( ?o arm:hasLabel ?st )
   -> ( ?s arm:hasTechniqueString ?st ) ]
[ spellform:
   ( ?s rdf:type arm:Spell ) ( ?s arm:hasForm ?o ) ( ?o arm:hasLabel ?st )
   -> ( ?s arm:hasFormString ?st ) ]

