
# Trait instances inherit properties from their class
[ traitinherit: 
  ( ?t rdf:type arm:LeafTraitClass )
  ( ?p rdf:type arm:TraitProperty ) 
  ( ?t ?p ?o ) 
  ( ?s rdf:type ?t ) 
  noValue( ?s,?p ) 
  -> ( ?s ?p ?o ) 
      ]
