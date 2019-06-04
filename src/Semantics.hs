module Semantics (
    timeout, enabledEvents,
    reduce, eventStep, timeStep,
    prefixNormsets
) where

import Data.List
import Logic
import Helper

-- Apply the syntactic reduction left to right
reduce :: Contract -> Contract
reduce c
    | changed   = reduce c'
    | otherwise = c
    where
        (changed, c') = reduce0 c

        reduce0 Top = (False, Top)
        reduce0 Bottom = (False, Bottom)
        reduce0 (Wait 0) = (True, Top)
        reduce0 c@(Wait t) = (False, c)
        reduce0 (Cond _ _ 0 (_, c)) = (True, snd $ reduce0 c)
        reduce0 (Cond p a t (c1, c2)) =
            (ch1 || ch2, Cond p a t (c1', c2'))
            where
                (ch1, c1') = reduce0 c1
                (ch2, c2') = reduce0 c2
        reduce0 (Norm O _ _ 0) = (True, Bottom)
        reduce0 (Norm P _ _ 0) = (True, Top)
        reduce0 (Norm F _ _ 0) = (True, Top)
        reduce0 c@(Norm _ _ _ _) = (False, c)

        reduce0 (Or cs)
            | Top `elem` cs' = (True, Top)
            | Bottom `elem` cs' = (True, or' (filter (Bottom /=) cs'))
            | otherwise = (or ds, or' cs')
            where
                (ds, cs') = unzip $ map reduce0 cs
                or' [] = Bottom
                or' [c] = c
                or' cs = Or cs
        reduce0 (And cs)
            | Bottom `elem` cs' = (True, Bottom)
            | Top `elem` cs' = (True, and' (filter (Top /=) cs'))
            | otherwise = (or ds, and' cs')
            where
                (ds, cs') = unzip $ map reduce0 cs
                and' [] = Top
                and' [c] = c
                and' cs = And cs

        reduce0 (Seq Top c) = (True, snd $ reduce0 c)
        reduce0 (Seq Bottom _) = (True, Bottom)
        reduce0 c@(Seq c1 c2)
            | ch1 || ch2 = (True, snd $ reduce0 (Seq c1' c2'))
            | otherwise  = (False, c)
            where
                (ch1, c1') = reduce0 c1
                (ch2, c2') = reduce0 c2

        reduce0 (Reparation Bottom c) = (True, snd $ reduce0 c)
        reduce0 (Reparation Top c) = (True, Top)
        reduce0 c@(Reparation c1 c2)
            | ch1 || ch2 = (True, snd $ reduce0 (Reparation c1' c2'))
            | otherwise  = (False, c)
            where
                (ch1, c1') = reduce0 c1
                (ch2, c2') = reduce0 c2

        reduce0 (Rec v c) = (ch, Rec v c')
            where
                (ch, c') = reduce0 c

        reduce0 c@(Var _) = (False, c)

-- Applies a visible action step along a contract
-- Assumes that the input contract is reduced.
eventStep :: Contract -> Event -> Contract
eventStep Top _ = Top
eventStep Bottom _ = Bottom
eventStep c@(Norm O p a _) e
    | a==actionOf e && p==partyOf e = Top
    | otherwise                 = c
eventStep c@(Norm F p a _) e
    | a==actionOf e && p==partyOf e = Bottom
    | otherwise                 = c
eventStep c@(Norm P p a _) e
    | a==actionOf e && p==partyOf e && isAttempt e = Bottom
    | a==actionOf e && p==partyOf e && isPerform e = Top
    | otherwise                                = c
eventStep (Cond p a d (c1, c2)) e
    | a==actionOf e && p==partyOf e && isPerform e = c1
    | otherwise                 = Cond p a d (c1, c2)
eventStep c@(Wait _) _ = c
eventStep (And cs) e = And [ eventStep c e | c <- cs]
eventStep (Or cs) e = Or [ eventStep c e | c <- cs]
eventStep (Seq c1 c2) e = Seq (eventStep c1 e) c2
eventStep (Reparation c1 c2) e = Reparation (eventStep c1 e) c2
eventStep c@(Rec v c1) e = eventStep (replaceFree (v, c) c1) e

-- Applies a time step along a contract
-- Assumes that:
-- * The input contract is reduced.
-- * The time step is no longer than the time at the head of the contract.
timeStep :: Time -> Contract -> Contract
timeStep _ Top = Top
timeStep _ Bottom = Bottom
timeStep dt@(Finite _) (Norm n p a t)
    | dt > t    = error "<timeStep> called with a time longer than the applicable time of a head term norm."
    | otherwise = Norm n p a (t-dt)
timeStep dt@(Finite _) (Cond p a t cs)
    | dt > t    = error "<timeStep> called with a time longer than the applicable time of a head term conditional."
    | otherwise = Cond p a (t-dt) cs
timeStep dt@(Finite _) (Wait t)
    | dt > t    = error "<timeStep> called with a time longer than the applicable time of a head term wait."
    | otherwise = Wait (t-dt)
timeStep dt c@(Rec v c1) = timeStep dt (replaceFree (v, c) c1)
timeStep dt (And cs) = And $ map (timeStep dt) cs
timeStep dt (Or cs)  = Or  $ map (timeStep dt) cs
timeStep dt (Seq c1 c2) = Seq (timeStep dt c1) c2
timeStep dt (Reparation c1 c2) = Reparation (timeStep dt c1) c2
timeStep Infinity _ = error "<timeStep> called with an infinite time."

-- Time until the structure of the formula changes
-- Assumes that the input contract is reduced.
timeout :: Contract -> Time
timeout Top = inf
timeout Bottom = inf
timeout (Norm _ _ _ t) = t
timeout (Cond _ _ t _) = t
timeout (Wait t) = t
timeout (Rec _ c) = timeout c
timeout (And cs) = minimum $ map timeout cs
timeout (Or cs) = minimum $ map timeout cs
timeout (Seq c _) = timeout c
timeout (Reparation c _) = timeout c
timeout (Var _) = error "timeout should never be applied to a variable"
-- Extract which events will change the structure of a contract formula
-- Assumes that the input contract is reduced.
enabledEvents :: Contract -> [Event]
enabledEvents = nub . enabledEvents'
    where
        enabledEvents' (Norm _ p a _) =
            [ attempt (p,a)
            , perform (p,a)
            ]
        enabledEvents' (Cond p a _ _) =
            [ attempt (p,a)
            , perform (p,a)
            , attempt (party "~", action "~")
            , perform (party "~", action "~")
            ]
        enabledEvents' (Or cs) = concat $ map enabledEvents' cs
        enabledEvents' (And cs) = concat $ map enabledEvents' cs
        enabledEvents' (Seq c1 _) = enabledEvents' c1
        enabledEvents' (Reparation c1 _) = enabledEvents' c1
        enabledEvents' (Rec _ c1) = enabledEvents' c1
        enabledEvents' _ = []

-- Extract possible norm sets in prefix of a contract, corresponding to the
-- possible sets of concurrent norms than can be possibly in force at the start
-- of the input contract.
-- e.g. <(oo p a 10 ||| oo p b 15) &&& oo p c 5> would extract
--      [[oo p a 10, oo p c 5], [oo p b 15, oo p c 5]]
-- Assumes that the input contract is reduced.
prefixNormsets :: Contract -> [[Contract]]
prefixNormsets c@(Norm _ _ _ _) = [[c]]
-- fix this
prefixNormsets (And cs) = map (foldl1 union) $ oneOfEach $ map prefixNormsets cs
prefixNormsets (Or cs) = foldl1 union $ map prefixNormsets cs
prefixNormsets (Seq c1 c2) = prefixNormsets c1
prefixNormsets (Rec _ c) = prefixNormsets c
prefixNormsets (Var _) = error "<prefixNormsets> applied to recursion variable which should never happen"
prefixNormsets _ = [[]]
-- prefixNormsets Top = [[]]
-- prefixNormsets Bottom = [[]]
-- prefixNormsets (Wait _) = [[]]
-- prefixNormsets (Cond _ _ _ _) = [[]]
-- prefixNormsets (Reparation c1 c2) = [[]]
