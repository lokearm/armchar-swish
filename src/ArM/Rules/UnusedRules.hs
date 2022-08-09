-- Old rules no longer used

-- These are retained for review.
-- They have been removed from the modules used n the system.

-- | apply grantsTrait to a CharacterSheet
grantRule = makeCRule  "grantRule" 
     [ arc sVar htRes oVar,     -- s hasTrait o
       arc oVar typeRes tVar,   -- o a t
       arc sVar typeRes csRes,  -- s a CharacterSheet
       arc tVar gtRes cVar ]    -- o grantsTrait c
     [ arc sVar htRes cVar ]    -- s

-- | Infer character sheet properties from character properties
csRule = makeCRule "csRule" 
    [ arc csVar isCharacterLabel cVar,
      arc pVar typeRes armCharacterProperty,
      arc cVar pVar oVar ]
    [ arc csVar pVar oVar ]
