[ hasAdvancement:
  ( ?s arm:hasAdvancementList ?l )
  -> listMapAsObject(?s, arm:hasAdvancement, ?l)
 ]
[ ( ?s arm:hasPregameAdvancementList ?l )
  -> listMapAsObject(?s, arm:hasAdvancement, ?l)
 ]
[ advanceTrait:
  ( ?s arm:advanceTraitList ?l )
  -> listMapAsObject(?s, arm:advanceTrait, ?l)
 ]
# Infer Character Sheets
[ infercharsheet1:
  ( ?char rdf:type arm:Character )
  ( ?char arm:hasAdvancement ?adv )
  ( ?adv arm:advanceCharacter ?c )
  ( ?adv arm:atSeasonTime ?t )
  ( ?cs1 arm:atSeasonTime ?t )
  ( ?cs1 arm:isCharacter ?char )
  ->
  ( ?adv arm:advanceFromCharacterSheet ?cs1 )
  ]
[ infercharsheet1:
  ( ?char rdf:type arm:BaseCharacter )
  ( ?char arm:hasAdvancement ?adv )
  ( ?adv arm:advanceFromCharacterSheet ?cs1 )
  ->
  ( ?cs1 arm:isCharacter ?char )
  ]
[ infercharsheet2:
  ( ?char rdf:type arm:BaseCharacter )
  ( ?char arm:hasAdvancement ?adv )
  ( ?adv arm:advanceToCharacterSheet ?cs2 )
  ->
  ( ?cs2 arm:isCharacter ?char )
  ]
[ inferseasontime:
  ( ?adv arm:advanceToCharacterSheet ?cs )
  ( ?adv arm:atSeasonTime ?pretime )
  ( ?pretime arm:isPrecedingSeasonOf ?time )
  ->
  ( ?cs arm:atSeasonTime ?time )
  ]
[ advtype:
  ( ?adv arm:hasAdvancementType ?type )
  ( ?type rdfs:label ?l )
  ->
  ( ?adv arm:hasAdvancementTypeString ?l )
  ]

# Character Sheet points to next season Character Sheet
[ nextcharactersheet:
  ( ?adv rdf:type arm:CharacterAdvancement )
  ( ?adv arm:advanceFromCharacterSheet ?cs1 )
  ( ?adv arm:advanceToCharacterSheet ?cs2 )
  ->
  ( ?cs1 arm:hasNextCharacterSheet ?cs2 )
  ]

# Trait instance points to its predecessor from before advancement
[ advancedtrait:
   ( ?adv rdf:type arm:CharacterAdvancement )
   ( ?adv  arm:advanceFromCharacterSheet ?cs ) 
   ( ?adv  arm:advanceTrait ?trait )
   ( ?trait  rdf:type ?tc )
   ( ?tc  rdf:type arm:LeafTraitClass )
   ( ?cs  arm:hasTrait ?oldtrait )
   ( ?oldtrait  rdf:type ?tc )
   -> ( ?trait arm:advancedFromTrait ?oldtrait ) ]

# Three alternatives.
# 1. Trait copied (noadvancetrait)
# 2. Trait improved
# 3. Trait created

# 1.  Traits without Advancement are carried forward
[ noadvancetrait:
   ( ?cs   rdf:type arm:CharacterSheet )
   ( ?cs   arm:hasTrait ?oldtrait ) 
   ( ?cs   arm:hasNextCharacterSheet ?nc ) 
   ( ?cs   ?p ?oldtrait ) 
   noValue( ?trait arm:advancedFromTrait ?oldtrait ) 
   -> ( ?nc ?p ?oldtrait ) ]

# 2-3. Traits from the CharacterAdvancement are added to the new charactersheet
# This rule is too costly to compute.
[ advancementtrait:
   ( ?adv rdf:type arm:CharacterAdvancement )
   ( ?adv  arm:advanceToCharacterSheet ?ncs ) 
   ( ?adv  arm:advanceTrait ?trait )
   -> 
   ( ?ncs arm:hasTrait ?trait )
]        

# 2. Traits which already existed are updated
[ advancetraitXP:
   ( ?trait arm:advancedFromTrait ?oldtrait  )
   ( ?trait arm:addedXP ?xp2 ) 
   ( ?oldtrait arm:hasTotalXP ?xp1 ) 
   sum(?xp1,?xp2,?xp)
   -> (  ?trait arm:hasTotalXP ?xp ) ]
[ -> table(arm:hasSpeciality) ]
[ -> table(arm:hasTotalXP) ]
[ advancetraitSpec:
   ( ?trait arm:advancedFromTrait ?oldtrait  )
   -> [ ( ?trait arm:hasSpeciality ?oldspec ) 
       <- noValue( ?trait arm:hasSpeciality ?spec ) 
          ( ?oldtrait arm:hasSpeciality ?oldspec ) 
   ]]
[ advancetraitEffect:
   ( ?trait arm:advancedFromTrait ?oldtrait  )
   ( ?oldtrait arm:hasEffect ?effect ) 
   ->
   ( ?trait arm:hasEffect ?effect ) ]

# 3. Convert addedXP to TotalXP when there was no prior trait.
[ newtraitXP:
   ( ?trait arm:addedXP ?xp ) -> [
   (  ?trait arm:hasTotalXP ?xp ) <-
   noValue( ?trait, arm:advancedFromTrait   )
   noValue( ?trait, arm:hasTotalXP  )  ]
]

### [ fixhasAbility: ( ?c arm:hasAbility ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Ability ) ]
### [ fixhasSpell: ( ?c arm:hasSpell ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Spell ) ]
### [ fixhasArt: ( ?c arm:hasArt ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Art ) ]
### [ fixhasPersonalityTrait: ( ?c arm:hasPersonalityTrait ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:PersonalityTrait ) ]
### [ fixhasOtherTrait: ( ?c arm:hasOtherTrait ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:OtherTrait ) ]
### [ fixhasReputation: ( ?c arm:hasReputation ?t )  <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Reputation ) ]
### [ fixhasVirtue: ( ?c arm:hasVirtue ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Virtue ) ]
### [ fixhasFlaw: ( ?c arm:hasFlaw ?t )  <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Flaw ) ]
### [ fixhasCharacteristic: ( ?c arm:hasCharacteristic ?t ) <-
###   ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Characteristic ) ]


[ fixhasAbility:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Ability )
  -> ( ?c arm:hasAbility ?t ) ]
[ fixhasSpell:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Spell )
  -> ( ?c arm:hasSpell ?t ) ]
[ fixhasArt:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Art )
  -> ( ?c arm:hasArt ?t ) ]
[ fixhasPersonalityTrait:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:PersonalityTrait )
  -> ( ?c arm:hasPersonalityTrait ?t ) ]
[ fixhasOtherTrait:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:OtherTrait )
  -> ( ?c arm:hasOtherTrait ?t ) ]
[ fixhasReputation:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Reputation )
  -> ( ?c arm:hasReputation ?t ) ]
[ fixhasVirtue:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Virtue )
  -> ( ?c arm:hasVirtue ?t ) ]
[ fixhasFlaw:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Flaw )
  -> ( ?c arm:hasFlaw ?t ) ]
[ fixhasCharacteristic:
  ( ?c arm:hasTrait ?t ) ( ?t rdf:type arm:Characteristic )
  -> ( ?c arm:hasCharacteristic ?t ) ]


[ ( ?node rdf:first ?trait )
  ( ?trait rdf:type arm:Trait )
  noValue( ?trait arm:addedXP ?xp ) 
  -> ( ?trait  arm:addedXP 0 ) ]
[ ( ?node rdf:first ?trait )
  ( ?trait arm:addedXP ?xp ) 
  ( ?node rdf:rest rdf:nil )
  -> ( ?node  arm:hasListXP ?xp ) ]
[ ( ?node rdf:first ?trait )
  ( ?trait arm:addedXP ?xp1 ) 
  ( ?node rdf:rest ?r )
  ( ?r arm:hasListXP ?xp0 ) 
  sum(?xp0,?xp1,?xp)
  -> ( ?node  arm:hasListXP ?xp ) ]
[ ( ?adv arm:advanceTraitList ?l )
  ( ?l arm:hasListXP ?xp )
  -> ( ?adv arm:hasSpentXP ?xp ) ]
