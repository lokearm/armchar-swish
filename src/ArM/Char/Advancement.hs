-----------------------------------------------------------------------------
-- |
-- Module      :  ArM.Types.Character
-- Copyright   :  (c) Hans Georg Schaathun <hg+gamer@schaathun.net>
-- License     :  see LICENSE
--
-- Maintainer  :  hg+gamer@schaathun.net
--
-----------------------------------------------------------------------------
module ArM.Char.Advancement where

import ArM.Char.Character
-- import ArM.Char.Trait

{-
advanceCharacter :: Character -> [ ( CharTime, [ ProtoTrait ] ) ]
advanceCharacter c = advanceCharacter' (charAdvancement c)
                   $ advanceCharacter' (pregameAdvancement c) []
advanceCharacter' :: [ Advancement ] -> [ ( CharTime, [ ProtoTrait ] ) ] 
                                    -> [ ( CharTime, [ ProtoTrait ] ) ]
advanceCharacter' [] cs = cs
advanceCharacter' (a:as) [] = advanceCharacter' as [ ( season a, changes a ) ] 
advanceCharacter' (a:as) ((t,xs):cs) = 
    advanceCharacter' as bs
       where bs = ( ( season a, advanceTraits ys xs):(t,xs):cs ) 
             ys = sortTraits $ changes a
-}

advanceCharacterState :: 
    ( CharacterState, [ Advancement ], [ Advancement ] ) ->
    ( CharacterState, [ Advancement ], [ Advancement ] ) 
advanceCharacterState (cs,[],ys) = (cs,[],ys)
advanceCharacterState (cs,(x:xs),ys) = advanceCharacterState (cs',xs,(x:ys))
   where cs' = advanceCS cs x
advanceCS :: CharacterState -> Advancement -> CharacterState 
advanceCS cs _ = cs { traits = cx, protoTraits = nx }
   where cx = computeCS nx
         nx = []
