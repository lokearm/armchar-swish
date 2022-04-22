# Instantiate traits to be granted by virtues.
[ vfp:
  ( ?vc arm:grantsPTraitClass ?pc )
  ( ?v  rdf:type ?vc )
  ( ?v  rdf:type armr:minorFlaw )
  makeTemp( ?p ) 
  ->
  ( ?p rdf:type ?pc )
  ( ?p rdf:type arm:PersonalityTrait )
  ( ?p rdf:type arm:Trait )
  ( ?v arm:grantsPTrait ?p )
  ( ?v arm:grantsTrait ?p )
  ( ?p arm:hasScore '+3'^^xsd:int )
  ]
[ vfpmajor:
  ( ?vc  arm:grantsPTraitClass ?pc )
  ( ?v  rdf:type ?vc )
  ( ?v  rdf:type armr:majorFlaw )
  makeTemp( ?p ) 
  ->
  ( ?p rdf:type ?pc )
  ( ?p rdf:type arm:PersonalityTrait )
  ( ?p rdf:type arm:Trait )
  ( ?v arm:grantsPTrait ?p )
  ( ?v arm:grantsTrait ?p )
  ( ?p arm:hasScore '+6'^^xsd:int )
  ]
[ vfv:
  ( ?vc1  arm:grantsVirtueClass ?vc2 )
  ( ?v1  rdf:type ?vc1 )
  makeTemp( ?v2 ) 
  ->
  ( ?v2 rdf:type ?vc2 )
  ( ?v2 rdf:type arm:Virtue )
  ( ?v2 rdf:type arm:Trait )
  ( ?v1 arm:grantsVirtue ?v2 )
  ( ?v1 arm:grantsTrait ?v2 )
  ( ?v2 arm:hasScore 0 )
  ]
[ vff:
  ( ?vc1  arm:grantsFlawClass ?vc2 )
  ( ?v1  rdf:type ?vc1 )
  makeTemp( ?v2 ) 
  ->
  ( ?v2 rdf:type ?vc2 )
  ( ?v2 rdf:type arm:Flaw )
  ( ?v2 rdf:type arm:Trait )
  ( ?v1 arm:grantsFlaw ?v2 )
  ( ?v1 arm:grantsTrait ?v2 )
  ( ?v2 arm:hasScore 0 )
  ]
[ vfability:
  ( ?vclass  arm:grantsAbilityClass ?abclass )
  ( ?v  rdf:type ?vclass )
  makeTemp( ?ab ) 
  ->
  ( ?ab rdf:type ?abclass )
  ( ?ab rdf:type arm:Ability )
  ( ?ab rdf:type arm:Trait )
  ( ?v arm:grantsAbility ?ab )
  ( ?v arm:grantsTrait ?ab )
  ( ?ab arm:hasTotalXP 5 )
  ]
