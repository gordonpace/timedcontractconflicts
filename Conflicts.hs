module Conflicts (
    (><), inConflict, reachableConflict,

    shortestConflicts, allConflicts,
    
    conflictsToDot,
    showShortestConflicts, toLaTeXShortestConflicts, showShortestConflictTraces, toLaTeXShortestConflictTraces,
    showAllConflicts, toLaTeXAllConflicts, showAllConflictTraces, toLaTeXAllConflictTraces,
) where

import Data.List
import qualified Data.Set as S

import Helper
import Logic
import Semantics
import ContractStateSpace

-- Checks whether two contract norms are in conflict
(><) :: Contract -> Contract -> Bool
Norm F p a t >< Norm n p' a' t' = p==p' && a==a' && n `elem` [P, O]
Norm n p a t >< Norm F p' a' t' = p==p' && a==a' && n `elem` [P, O]
_ >< _ = False

-- Checks whether a contract contain an immediate conflict
-- Assumes that the input contract is reduced.
inConflict' :: [Contract] -> Bool
inConflict' l = or [ c1 >< c2 | (c1:cs) <- tails l, c2 <- cs ]

inConflict :: Contract -> Bool
inConflict c = 
  or [ n1 >< n2 | normset <- prefixNormsets c, (n1:normset') <- tails normset, n2 <- normset' ]

-- Checks whether two contracts contain a conflict
reachableConflict :: Contract -> Bool
reachableConflict c = any inConflict $ states $ constructAutomaton c

-- Visualise the automaton in dot format with conflicts
conflictsToDot automaton =
    unlines $
        [ "digraph contract {" 
        , "   graph [ outputorder=\"edgesfirst\" ];"
        , concat [ "   0 [ shape=circle, style=filled, color=blue, label=\"\" ];" ]
        ] ++
        [ concat [ "   ", show n, " [ shape=circle, style=filled, label=\"\" color=", color," ];" ] 
        | (n, c') <- zip [1..] (tail $ states automaton)
        , let color = if inConflict c' then "red" else "black"
        ] ++
        [ concat [ "   ", show q, " -> ", show q', ";"] | (q, l, q') <- transitions automaton ] ++
        [ "}" ]


-- Get all reachable conflicts from a contract
allConflicts' :: ContractAutomaton -> [(Int, Contract)]
allConflicts' automaton =
    [ (n, q) 
    | (n, q) <- zip [0..] (states automaton)
    , inConflict q 
    ]

allConflicts :: Contract -> [Contract]
allConflicts = map snd . allConflicts' . constructAutomaton

-- Given a contract, get all reachable conflicts, each paired with a shortest path 
-- leading to it. Conflicts are ordered by length of trace to reach them.
type Trace = [(Label, Contract)]

allConflictTraces' :: ContractAutomaton -> [Trace]
allConflictTraces' automaton = 
    allConflictTracesAuxiliary [] (S.singleton 0) [(0, [])]
    where
        state n = states automaton !! n 
        outgoingTransitions n = [ (l, n_dst) | (n_src,l,n_dst) <- transitions automaton, n_src == n ]

        allConflictTracesAuxiliary :: [Trace] -> S.Set Int -> [(Int, Trace)] -> [Trace]
        allConflictTracesAuxiliary foundConflicts visitedNodes [] = foundConflicts
        allConflictTracesAuxiliary foundConflicts visitedNodes nodesToCheck = 
            allConflictTracesAuxiliary foundConflicts' visitedNodes' nodesToCheck'
            where
                foundConflicts' = 
                    [ reverse trace | (n, trace) <- nodesToCheck, inConflict (state n) ] ++ foundConflicts
                nodesToCheck' =
                    nubBy (\(n1,_) (n2,_) -> n1 == n2)
                    [ (n', (l, state n'):trace) 
                    | (n, trace) <- nodesToCheck
                    , (l,n') <- outgoingTransitions n
                    , S.notMember n' visitedNodes 
                    ]
                visitedNodes' = 
                    visitedNodes `S.union` S.fromList (map fst nodesToCheck')

allConflictTraces :: Contract -> [Trace]
allConflictTraces = allConflictTraces' . constructAutomaton

-- Given a contract, get any shortest-paths which lead to a conflict 
shortestConflictTraces :: Contract -> [Trace]
shortestConflictTraces contract =
    case allConflictTraces contract of
        [] -> []
        cs -> takeWhile ((length (head cs) ==) . length) cs

-- Given a contract, get any reachable conflict with shortest-path 
shortestConflicts :: Contract -> [Contract]
shortestConflicts contract =
    map (last . (contract:) . map snd) (shortestConflictTraces contract)

-- Explaining conflicts arising from a contract in text
showAllConflicts, showShortestConflicts, showAllConflictTraces, showShortestConflictTraces :: Contract -> String
showAllConflicts contract = showConflicts contract $ allConflicts contract
showShortestConflicts contract = showConflicts contract $ shortestConflicts contract
showAllConflictTraces contract = showConflictTraces contract $ allConflictTraces contract
showShortestConflictTraces contract = showConflictTraces contract $ shortestConflictTraces contract

-- Explaining a list of conflict traces in LaTeX
toLaTeXAllConflicts, toLaTeXShortestConflicts, toLaTeXAllConflictTraces, toLaTeXShortestConflictTraces :: Contract -> String
toLaTeXAllConflicts contract = toLaTeXConflicts contract $ allConflicts contract
toLaTeXShortestConflicts contract = toLaTeXConflicts contract $ shortestConflicts contract
toLaTeXAllConflictTraces contract = toLaTeXConflictTraces contract $ allConflictTraces contract
toLaTeXShortestConflictTraces contract = toLaTeXConflictTraces contract $ shortestConflictTraces contract

-- List conflicts arising from a given contract in text
showConflicts :: Contract -> [Contract] -> String
showConflicts contract conflicts
    | null conflicts = "No conflicts found"
    | otherwise      =
        unlines $
            [ "<"++show contract++"> found to have "++show (length conflicts)++
                " conflict"++(if length conflicts == 1 then "" else "s")++":"
            , ""
            ]++
            map (\contract -> "* <"++show contract++">") conflicts

-- Explain list of conflicts arising from a given contract in text
showConflictTraces :: Contract -> [Trace] -> String
showConflictTraces contract conflicts
    | null conflicts = "No conflicts found"
    | otherwise      =
        unlines $
            ["<"++show contract++"> found to have "++show (length conflicts)++
                " conflict"++(if length conflicts == 1 then "" else "s")++"."]++
            (if any null conflicts then ["", "* <" ++ show contract ++ "> itself is in conflict."] else [])++
            concat
                [ "":
                  ("* Conflicting contract <"++show c'++"> can be reached as follows:"):
                  [ "    "++show n++". "++show' l++" (resulting in <"++show c++">)"
                  | (n, (l, c)) <- zip [1..] conflict
                  ]
                | conflict <- conflicts
                , not (null conflict)
                , let c' = snd (last conflict)
                ]
    where
        show' (Waiting dt) = show dt ++ " time unit(s) pass"
        show' (Observation (Perform p a)) = "Action <"++show a++"> performed by <"++show p++">"
        show' (Observation (Attempt p a)) = "Action <"++show a++"> attempted by <"++show p++"> but no other party accepted"


-- List conflicts arising from a given contract in text
toLaTeXConflicts :: Contract -> [Contract] -> String
toLaTeXConflicts contract conflicts
    | null conflicts = "No conflicts found"
    | otherwise      =
        unlines $
            [ "Contract $\\langle "++toLaTeX contract++"\\rangle$ found to have "++show (length conflicts)++
                " conflict"++(if length conflicts == 1 then "" else "s")++":"
            , "\\begin{itemize}"
            ]++
            map (\contract -> "\\item $\\langle\\rangle$ "++toLaTeX contract++"$\\rangle$") conflicts ++
            [ "\\end{itemize}" ]

-- Explain list of conflicts arising from a given contract in LaTeX
toLaTeXConflictTraces :: Contract -> [Trace] -> String
toLaTeXConflictTraces contract conflicts
    | null conflicts = "No conflicts found."
    | otherwise      =
        unlines $
            [ "Contract $\\langle "++toLaTeX contract++"\\rangle$ found to have "++show (length conflicts)++
                 " conflict"++(if length conflicts == 1 then "" else "s")++"."
            , "\\begin{itemize}"
            ] ++
            (if any null conflicts then ["", "\\item $\\langle " ++ toLaTeX contract ++ "\\rangle$ itself is in conflict."] else [])++
            concat
                [ ("\\item Conflicting contract $\\langle\\rangle$ "++toLaTeX c'++"\\rangle$ can be reached as follows:"):
                  "\\begin{enumerate}":
                  [ "\\item "++show' l++" (resulting in $\\langle "++toLaTeX c++"\\rangle$)"
                  | (l, c) <- conflict
                  ]++
                  ["\\end{enumerate}"]
                | conflict <- conflicts
                , not (null conflict)
                , let c' = snd (last conflict)
                ]++
            ["\\end{itemize}"]
    where
        show' (Waiting dt) = show dt ++ " time unit"++(if dt==1 then "" else "s")++" pass"
        show' (Observation e) = "Event $\\langle"++toLaTeX e++"\\rangle$"
