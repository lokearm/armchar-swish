
# The following two rules should be obsolete, replacing the rdfs:label
# with arm:hasLabel.
[ addlabel: 
  ( ?t rdf:type arm:LeafTraitClass )
  ( ?s rdf:type ?t ) 
  ( ?t rdfs:label ?l ) 
  -> [ ( ?s rdfs:label ?l ) <- noValue( ?s,rdfs:label ) ]
  ]
[ label:
  ( ?s rdf:type arm:Trait )
  ( ?s rdfs:label ?l )
  -> [ ( ?s arm:hasLabel ?l ) <- noValue( ?s,arm:hasLabel ) ]
  ]
