module Logic (
    Party (..),
        party,

    Action,
        action,

    Time (..),
        inf,

    Modality (..),

    RecursionVariable,

    Contract (..),
        oo, pp, ff, wait, rec, var, cond, (>->), (|>), (&&&), (|||),

    Event (..),
        isAttempt, isPerform,
        perform, attempt,
        partyOf, actionOf,

    replaceFree, normaliseContract
) where

import Data.List

import Helper

-- Parties
newtype Party = Party { fromParty :: String } deriving (Eq, Ord)

instance Show Party where
    show = fromParty

instance LaTeX Party where
    toLaTeX p = "\\mathtt{"++fromParty p++"}"

party name = Party { fromParty = name }

-- Action
newtype Action = Action { fromAction :: String } deriving (Eq, Ord)

action = Action

instance Show Action where
    show = fromAction

instance LaTeX Action where
    toLaTeX a = "\\mathtt{"++fromAction a++"}"

-- Time
data Time = Infinity | Finite Integer deriving Eq

instance Show Time where
    show (Finite n) = show n
    show _ = "infty" -- "∞"

instance LaTeX Time where
    toLaTeX (Finite n) = show n
    toLaTeX _ = "\\infty"

instance Num Time where
    Infinity + _ = Infinity
    _ + Infinity = Infinity
    Finite n + Finite m = Finite (n+m)

    Infinity * _ = Infinity
    _ * Infinity = Infinity
    Finite n * Finite m = Finite (n*m)

    abs (Finite n) = Finite (abs n)
    abs x = x

    signum (Finite n) = Finite (signum n)
    signum Infinity = Finite 1

    negate (Finite n) = Finite (negate n)

    fromInteger = Finite

instance Ord Time where
    compare (Finite n) (Finite m) = compare n m
    compare Infinity Infinity = EQ
    compare Infinity _ = GT
    compare _ Infinity = LT

inf = Infinity

-- Modalities
data Modality = P | F | O deriving (Eq, Ord, Show)

instance LaTeX Modality where
    toLaTeX P = "\\pp"
    toLaTeX F = "\\ff"
    toLaTeX O = "\\oo"

    -- Recursion variables
type RecursionVariable = String

-- Contracts
data Contract
    = Top
    | Bottom
    | Norm Modality Party Action Time
    | Wait Time
    | Cond Party Action Time (Contract, Contract)
    | Seq Contract Contract
    | And [Contract]
    | Or [Contract]
    | Reparation Contract Contract
    | Rec RecursionVariable Contract
    | Var RecursionVariable
    deriving (Eq, Ord)

instance Show Contract where
    show = show'
        where
            show' Top = "Top" -- "⊤"
            show' Bottom = "Bottom" -- ⊥"
            show' (Norm m p a t) =
                concat [show m, "_", show p, "(", show a, ")[", show t, "]"]
            show' (Wait t) = concat ["wait(", show t, ")"]
            show' (Cond p a t (c1, c2)) =
                concat ["cond_", show p, "(", show a, ")[", show t, "](", show' c1, ", ", show' c2, ")"]
            show' (Var v) = v
            show' (Rec v c) =
                concat ["rec ", v, ". ", show' c]
            show' c@(Seq c1 c2) =
                concat [bshow' p c1, "; ", bshow' p c2]
                where
                    p = precedence c
            show' c@(And cs) =
                concat $ intersperse " ^ " $ map (bshow' p) cs
                where
                    p = precedence c
            show' c@(Or cs) =
                concat $ intersperse " V " $ map (bshow' p) cs
                where
                    p = precedence c
            show' c@(Reparation c1 c2) =
                concat [bshow' p c1, " > ", bshow' p c2]
                where
                    p = precedence c

            precedence (Rec _ _) = 0
            precedence (Or _) = 0
            precedence (And _) = 0
            precedence (Reparation _ _) = 0
            precedence (Seq _ _) = 0
            precedence _ = 1

            bshow' p c
                | precedence c > p = sc
                | otherwise        = concat ["(", sc, ")"]
                where
                    sc = show' c

instance LaTeX Contract where
    toLaTeX = toLaTeX'
        where
            toLaTeX' Top = "\\top"
            toLaTeX' Bottom = "\\bot"
            toLaTeX' (Norm m p a t) =
                concat [toLaTeX m, "{", toLaTeX p, "}{", toLaTeX a, "}{", toLaTeX t, "}"]
            toLaTeX' (Wait t) = concat ["\\wait(", toLaTeX t, ")"]
            toLaTeX' (Cond p a t (c1, c2)) =
                concat ["\\timedcondtn{", toLaTeX a, "}{", toLaTeX p, "}{", toLaTeX t, "}{", toLaTeX' c1, "}{", toLaTeX' c2, "}"]
            toLaTeX' (Var v) = v
            toLaTeX' (Rec v c) =
                concat ["\\rec ", v, ". ", toLaTeX' c]
            toLaTeX' c@(Seq c1 c2) =
                concat [toLaTeX'' p c1, ";", toLaTeX'' p c2]
                where
                    p = precedence c
            toLaTeX' c@(And cs) =
                concat $ intersperse " \\land " $ map (toLaTeX'' p) cs
                where
                    p = precedence c
            toLaTeX' c@(Or cs) =
                concat $ intersperse " \\lor " $ map (toLaTeX'' p) cs
                where
                    p = precedence c
            toLaTeX' c@(Reparation c1 c2) =
                concat [toLaTeX'' p c1, " \\violationrec ", toLaTeX'' p c2]
                where
                    p = precedence c

            precedence (Rec _ _) = 0
            precedence (Or _) = 0
            precedence (And _) = 0
            precedence (Reparation _ _) = 0
            precedence (Seq _ _) = 0
            precedence _ = 1

            toLaTeX'' p c
                | precedence c > p = sc
                | otherwise        = concat ["(", sc, ")"]
                where
                    sc = toLaTeX' c

-- Events
data Event
    = Perform Party Action
    | Attempt Party Action
    deriving Eq

instance Show Event where
    show (Perform p a) = concat ["(", show p, ", ", show a, ")"]
    show (Attempt p a) = concat ["!(", show p, ", ", show a, ")"]

instance LaTeX Event where
    toLaTeX (Perform p a) = "\\langle "++toLaTeX p++", "++toLaTeX a++"\\rangle"
    toLaTeX (Attempt p a) = "\\langle \\overline{"++toLaTeX p++", "++toLaTeX a++"}\\rangle"

isPerform (Perform _ _) = True
isPerform _ = False

isAttempt (Attempt _ _) = True
isAttempt _ = False

perform (p, a) = Perform p a
attempt (p, a) = Attempt p a

partyOf (Perform p _) = p
partyOf (Attempt p _) = p

actionOf (Perform _ a) = a
actionOf (Attempt _ a) = a

-- Syntactic sugar
oo = Norm O
ff = Norm F
pp = Norm P

wait = Wait
rec = Rec
cond = Cond
var = Var

c >-> c' = Seq c c'
c |> c' = Reparation c c'
c &&& c' = And [c, c']
c ||| c' = Or [c, c']

replaceFree :: (RecursionVariable, Contract) -> Contract -> Contract
replaceFree (v, e) c = replaceIn c
    where
        replaceIn (Cond p a t (c1, c2)) = Cond p a t (replaceIn c1, replaceIn c2)
        replaceIn (Seq c1 c2) = Seq (replaceIn c1) (replaceIn c2)
        replaceIn (And cs) = And $ map replaceIn cs
        replaceIn (Or  cs) = Or $ map replaceIn cs
        replaceIn (Reparation c1 c2) = Reparation (replaceIn c1) (replaceIn c2)
        replaceIn c@(Rec v1 c1)
            | v == v1   = c
            | otherwise = Rec v1 (replaceIn c1)
        replaceIn c@(Var v1)
            | v == v1   = e
            | otherwise = c
        replaceIn c = c


-- Order con/disjunctions and remove idempotent con/disjuncts
normaliseContract :: Contract -> Contract
normaliseContract (Cond p a t (c1, c2)) = Cond p a t (normaliseContract c1, normaliseContract c2)
normaliseContract (Seq c1 c2) = Seq (normaliseContract c1) (normaliseContract c2)
normaliseContract (Reparation c1 c2) = Reparation (normaliseContract c1) (normaliseContract c2)
normaliseContract (Rec v c) = Rec v (normaliseContract c)
normaliseContract c@(And cs) = and' $ sort $ nub $ map normaliseContract $ getConjuncts c
    where
        and' [] = Top
        and' [c] = c
        and' cs = And cs

        getConjuncts (And cs) = concat $ map getConjuncts cs
        getConjuncts c = [c]
normaliseContract c@(Or cs) = or' $ sort $ nub $ map normaliseContract $ getDisjuncts c
    where
        or' [] = Bottom
        or' [c] = c
        or' cs = Or cs

        getDisjuncts (Or cs) = concat $ map getDisjuncts cs
        getDisjuncts c = [c]
normaliseContract c = c
