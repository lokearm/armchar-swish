module ArM.Char.Legacy.Trait where

updateMastery :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateMastery a t = t { mastery = f (mastery t) (mastery a) }
    where f Nothing x = x
          f y Nothing = y
          f (Just x) (Just y)  = Just (x ++ y)

updateSpec :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateSpec a = u (spec a)
    where u Nothing t = t
          u (Just x) t = t { spec = Just x }
updateScore :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateScore a = u (score a)
    where u Nothing t = t
          u (Just x) t = t { score = Just x }
updateXP :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateXP a t = t { xp = maybeAdd (xp a) (xp t) }

updateAging :: ProtoTrait -> ProtoTrait -> ProtoTrait
updateAging a t = t { aging = maybeAdd ( aging t ) ( aging a ) }

{-
   advanceTrait a
       | ability a /= Nothing = updateSpec a . updateXP a
       | characteristic a /= Nothing = updateScore a . updateAging a
       | art a /= Nothing = updateXP a 
       | spell a /= Nothing = updateXP a . updateMastery a
       | ptrait a /= Nothing = updateScore a 
       | reputation a /= Nothing = updateScore a 
       | confidence a /= Nothing = updateScore a . updatePts a
       | other a /= Nothing = updatePts a
       | otherwise  = id
     where updatePts ad t = t { points = maybeAdd ( points t ) ( points ad ) }
-}
