
[ retainequipment:
   ( ?adv arm:advanceFromCharacterSheet ?cs1 )
   ( ?adv arm:advanceToCharacterSheet ?cs2 )
   ( ?cs1 arm:hasPossession ?item )
   noValue( ?adv arm:lostPossession ?item )
   ->
   ( ?cs2 arm:hasPossession ?item ) 
]

[ loseequipment:
   ( ?adv arm:advanceFromCharacterSheet ?cs1 )
   ( ?adv arm:advanceToCharacterSheet ?cs2 )
   ( ?cs1 arm:hasPossession ?item )
   ( ?adv arm:lostPossession ?loss )
   ( ?loss arm:hasLoss ?item )
   ( ?loss arm:hasLossQuantity ?lossq )
   ( ?item arm:hasQuantity ?oldq )
   ( ?item rdf:type ?type )
   difference(?oldq,?lossq,?q)
   makeTemp(?new)
   ->
   ( ?cs2 arm:hasPossession ?new ) 
   ( ?new arm:hasQuantity ?q ) 
   ( ?new rdf:type ?type ) 
   ( ?new arm:updateOfPossession ?item ) 
]
[ loseequipment2:
   ( ?new arm:updateOfPossession ?item ) 
   ( ?item ?p ?o ) 
   noValue( ?new, ?p )
   ->
   ( ?new ?p ?o ) 
]

[ fixhasvis: ( ?c arm:hasPossession ?item ) ( ?item rdf:type arm:Vis )
   -> ( ?c arm:hasVis ?item ) ]
[ fixhaseq: ( ?c arm:hasPossession ?item ) ( ?item rdf:type arm:Equipment )
   -> ( ?c arm:hasEquipment ?item ) ]
[ fixhasweapon: ( ?c arm:hasPossession ?item ) ( ?item rdf:type arm:Weapon )
   -> ( ?c arm:hasWeapon ?item ) ]
[ vislabel: ( ?v arm:isVisOfArt ?art ) ( ?art arm:hasLabel ?l )
   -> ( ?v arm:hasPossessionLabel ?l ) ]

[ eqinherit: 
  ( ?t rdf:type owl:Class )
  ( ?p rdf:type arm:PossessionProperty ) 
  ( ?t ?p ?o ) 
  ( ?s rdf:type ?t ) 
  noValue( ?s,?p ) 
  -> ( ?s ?p ?o ) 
      ]
