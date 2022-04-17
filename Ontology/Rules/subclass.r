# These are OWL rules, but may have to be applied again by the generic
# reasoner.

[ subclass:
  ( ?s rdf:type arm:Trait ) ( ?s rdf:type ?t1 ) ( ?t1 rdfs:subClassOf ?t2 ) 
  -> ( ?s rdf:type ?t2 ) ]

# TODO.  Reconsider.  Is this required?
[ hastrait:
  ( ?p rdfs:subPropertyOf arm:hasTrait ) 
  -> ( ?p rdf:type owl:ObjectProperty ) ]
### [ formclass:
###   ( ?s rdf:type arm:FormClass ) -> ( ?s rdfs:subClassOf arm:Form ) ]
### [ techclass:
###   ( ?s rdf:type arm:TechClass ) -> ( ?s rdfs:subClassOf arm:Tech ) ]
