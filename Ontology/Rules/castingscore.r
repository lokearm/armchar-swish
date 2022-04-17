# TODO.  Multiple characters have the same spell.  
# Thus the casting total is ambiguous.  This cannot work.

[ castingtotal:
  ( ?spell rdf:type arm:Spell )
  ( ?char arm:hasSpell ?spell )
  ( ?spell arm:hasTechnique ?teclass )
  ( ?char arm:hasArt ?te )
  ( ?te rdf:type ?teclass )
  ( ?spell arm:hasForm ?foclass )
  ( ?char arm:hasArt ?fo )
  ( ?fo rdf:type ?foclass )
  ( ?fo arm:hasScore ?foscore )
  ( ?te arm:hasScore ?tescore )
  sum( ?tescore,?foscore,?tefoscore )
  ( ?char arm:hasCharacteristic ?sta )
  ( ?sta rdf:type armr:sta )
  ( ?sta arm:hasScore ?stascore)
  sum( ?tefoscore,?stascore,?castingscore )
  -> ( ?spell arm:hasCastingScore ?castingscore )
  ]
