
# Deduce age from birthyear
[ age:
  ( ?char rdf:type arm:CharacterSheet )
  ( ?char arm:inYear ?current )
  ( ?char arm:hasBirthYear ?birth )
  difference( ?current, ?birth, ?age )
  ->
  ( ?char arm:hasAge ?age )
 ]
