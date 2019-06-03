# TheCon: THEmulus CONflict Discovery
A tool to discover conflicts in the timed deontic contract calculus Themulus [1]. The tool embeds Themulus in Haskell to be able to write timed contracts and analyse them for conflicts.

## Themulus-in-Haskell Syntax

*Support types*
Parties can be created using the 'party' function and a string describing it. Similarly actions using the 'action' function:
```
party :: String -> Party
action :: String -> Action
```
Time deadlines can be either any floating point number, or infinite:
```
time :: Float -> Time
inf :: Time
```
*Contract syntax:*

Obligations
```
oo :: Party -> Action -> Time -> Contract
```
Prohibitions
```
ff :: Party -> Action -> Time -> Contract
```
Permissions
```
pp :: Party -> Action -> Time -> Contract
```
Wait
```
wait :: Time -> Contract
```
Conditional
```
cond :: Party -> Action -> Time -> (Contract, Contract) -> Contract
```
Sequential composition 
```
(>->) :: Contract -> Contract -> Contract
```
Reparation 
```
(|>) :: Contract -> Contract -> Contract
```
Conjunction
```
(&&&) :: Contract -> Contract -> Contract
```
Disjunction
```
(|||) :: Contract -> Contract -> Contract
```
Recursion
```
rec :: String -> Contract
```
Recursion variable
```
var :: String -> Contract
```

*Helper functions:*

Produces a LaTeX version of the formula:
```
toLaTeX :: Contract -> String
```

Check a contract for conflicts:
```
hasConflict :: Contract -> Bool
showShortestConflicts :: Contract -> String
showConflicts :: Contract -> String
toLaTeXShortestConflicts :: Contract -> String
toLaTeXAllConflicts :: Contract -> String
```
Visualisation of the automaton produced by full-transitions (see paper)  and conflicts
```
constructAutomaton :: Contract -> ContractAutomaton
conflictsToDot :: ContractAutomaton -> String
```
(The latter produces a dot file which can be visualised using graphviz or similar tool)

