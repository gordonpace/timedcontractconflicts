module ContractStateSpace (
    ContractAutomaton (..), Label (..), show',
    constructAutomaton, automatonToDot
) where

import Data.List
import Data.Maybe

import Helper
import Logic
import Semantics

-- Transition labels in conflict automaton
data Label
    = Observation Event
    | Waiting Time
    deriving Eq

instance Show Label where
    show (Waiting dt) = concat ["~~[", show dt,"]~~>"] 
    show (Observation e) = concat ["--[", show e, "-->"]

-- Conflict analysis automaton
data ContractAutomaton =
    Automaton {
        initial :: Contract,
        states :: [Contract],
        transitions :: [(Int, Label, Int)]
    } deriving Eq

instance Show ContractAutomaton where
    show automaton =
        unlines $
            "Intial state: ":
            ("   "++show (head qs)):
            "Transitions:":
            [ concat ["   ",show (qs !! q_index)," ",show l," ",show (qs !! q_index')] 
            | (q_index, l, q_index') <- transitions automaton 
            ]
        where
            qs = states automaton
            ts = transitions automaton

show' automaton =
    unlines $
        "States:":
        [ concat ["   ",show n,": ",show c] | (n,c) <- zip [0..] (states automaton) ] ++
        "Intial state: 0":
        "Transitions:":
        [ concat ["   ",show q," ",show l," ",show q'] | (q, l, q') <- transitions automaton ]

automatonToDot automaton =
    unlines $
        [ "digraph contract {"
        , "   graph [ outputorder=\"edgesfirst\" ];" 
        , concat [ "   0 [ shape=circle, style=filled, color=black, label=\"\" ];" ]
        ] ++
        [ concat [ "   ", show n, " [ shape=circle, label=\"\" ];" ] | n <- [1..(length (states automaton) - 1)] ]++
        [ concat [ "   ", show q, " -> ", show q', ";"] | (q, l, q') <- transitions automaton ] ++
        [ "}" ]

-- Constructing the conflict automaton from a starting automaton
constructAutomaton :: Contract -> ContractAutomaton
constructAutomaton contract = construct [contract0] [] [contract0]
    where
        contract0 = normaliseContract $ reduce contract

        construct :: [Contract] -> [(Int, Label, Int)] -> [Contract] -> ContractAutomaton
        construct qs ts [] = 
            Automaton {
                initial = contract0,
                states = qs,
                transitions = ts
            }
        construct qs0 ts0 (q:qs) =
            let new_qs_time = temporal \\\ qs0
                new_ts_time = 
                    [ (q_index, Waiting q_timeout, q_index') 
                    | q' <- temporal
                    , let q_index' = index q' qs0'
                    ]

                new_qs_event = visible \\\ qs0
                new_ts_event = 
                    [ (q_index, Observation e, q_index')
                    | (e, q') <- zip q_events visible 
                    , let q_index' = index q' qs0' 
                    ]

                new_qs = nub $ new_qs_time ++ new_qs_event
                qs0' = qs0 ++ new_qs
                ts0' = new_ts_time ++ new_ts_event

            in construct qs0' (ts0'++ts0) (qs++new_qs)
            where
                index q qs = fromJust $ elemIndex q qs

                q_index = index q qs0
                q_timeout = timeout q
                q_events = enabledEvents q
                
                temporal     = [ normaliseContract $ reduce $ timeStep q_timeout q ]
                visible      = map (normaliseContract . reduce . eventStep q) q_events

