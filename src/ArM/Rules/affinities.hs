-- Failed drafts
bonusXP :: RDFRule
bonusXP = makeCRule  "bonusXP" 
     [ arc (Var "char") (armRes "hasTrait") (Var "trait")
     , arc (Var "trait") typeRes tVar
     , arc (Var "char") (armRes "hasTrait") (Var "bonus")
     , arc (Var "bonus") (armRes "bonusTo") tVar
     , arc (Var "bonus") (armRes "hasXPfactor") (Var "score") ]
     [ arc (Var "trait") (armRes "hasXPfactor") (Var "score") ]
bonusXPtest :: RDFRule
bonusXPtest = makeCRule  "bonusXPtest"
     [ arc (Var "char") (armRes "hasTrait") (Var "trait")
     , arc (Var "trait") typeRes tVar
     , arc (Var "char") (armRes "hasTrait") (Var "bonus")
     , arc (Var "bonus") (armRes "bonusTo") tVar
     ]
     [ arc (Var "trait") (armRes "fooBar") (litString "trait") 
     , arc (Var "bonus") (armRes "fooBar") (litString "bonus") ]
