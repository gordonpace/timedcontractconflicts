# TheCon: THEmulus CONflict Discovery
A tool to discover conflicts in the timed deontic contract calculus Themulus [1]. The tool embeds Themulus in Haskell to be able to write timed contracts and analyse them for conflicts.

## Themulus-in-Haskell Syntax

*Support types*
Parties can be created using the `party` function and a string describing it. Similarly actions using the `action` function:
```
party :: String -> Party
action :: String -> Action
```
Time deadlines can be defined using `time` on any `Double` value (number constants do not need the use of `time` and are automatically converted), or `inf` for an infinite deadline:
```
time :: Double -> Time
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

Produce a LaTeX version of the formula:
```
toLaTeX :: Contract -> String
```
The folder `latex` contains a minimal LaTeX source with the definitions to allow for typesetting the tool output. 

Check a contract for conflicts:
```
hasConflict :: Contract -> Bool
showShortestConflicts :: Contract -> String
showConflicts :: Contract -> String
toLaTeXShortestConflicts :: Contract -> String
toLaTeXAllConflicts :: Contract -> String
```
Visualisation of the automaton produced by full-transitions (see paper)  and conflicts:
```
constructAutomaton :: Contract -> ContractAutomaton
conflictsToDot :: ContractAutomaton -> String
```
The latter produces a dot file which can be visualised using graphviz or similar tool.

##Example ISP end-user agreement##
```
isp = party "ISP"
user = party "user"

pay = action "pay"
disconnect = action "disconnect"
lost_connection = action "lost connection"
compensate = action "compensate"

-- The user is obliged to pay within 60 time units, but if not
-- the ISP is allowed to disconnect them within 20 time units.
paymentClause = oo user pay 60 |> pp isp disconnect 20

-- If the user gets a lost connection within 10 time units, the 
-- the ISP is obliged to compensate them within 60 time units and
-- continue with the agreement. If no connection is lost, the 
-- payment clause kicks in.
contract = 
  rec "x" $ 
    cond user lost_connection 60 (
      oo isp compensate 60 >-> var "x", 
      paymentClause
    )
```
