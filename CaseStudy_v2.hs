{-# LANGUAGE OverloadedStrings #-}
module Main where

import Helper
import Logic
import Semantics
import ContractStateSpace
import Conflicts

-- time unit 1 hour
-- one_hour = 1
-- limit = 28 * 12 * 100 -- almost 10 years
day = 24
limit = Infinity

client = party "client"
provider = party "provider"
terminate = action "terminate" -- "Termitate agreement"
pis = action "pis"  -- "Provider IP as stipulated"
mitr = action "mitr"  -- "Measure Internet Traffic"
sfi = action "sfi"  -- "Supply False Information"
ith = action "ith" -- "Internet traffic is High"
itn = action "itn" -- "Internet traffic is Normal"
pay_price = action "pay_price" -- "Pay price"
pay_price_double = action "pay_price_double"  -- "Pay price"
pay_price_triple = action "pay_price_triple" -- "Pay price"
notify_pay_later = action "notify_pay_later"  -- Notify later
ioper = action "ioper" -- Internet operative
spdf = action "spdf" -- Send Personal Data Form
pay = action "pay"
choose_28 = action "choose_28"
choose_84 = action "choose_84"
choose_168 = action "choose_168"
mail = action "mail" -- Have an email and a user account
send_data = action "send_data" -- send personal data
send_report = action "send_report" -- Send report of intnernet usage
request = action "request" -- "make a request"
response = action "response" -- "be responsive"
return_data = action "return_data" -- return data to the original status
supply_internet = action "supply_internet" -- "supply Internet"
notify_termination = action "notify_termination" -- "notify termination"
seize = action "seize" -- seize activity upon request
personal_data = action "personal_data" -- alter, delte use personal data


t1 = cond client terminate limit (Top, Top)
t2 = cond provider terminate limit (Top, Top)


c3 = oo provider pis limit
c51 = pp client mitr limit &&& pp provider mitr limit
c71 = ff client sfi limit
c73 = (oo client notify_pay_later day >->
       (oo client itn day |> c74)) &&&
       oo client pay_price_double (3*day)
c74 = oo client pay_price_triple (6*day)
c72 = rec "X" (cond client ith limit (oo client pay_price (2*day) |||
                                      c73, Top) >-> Var "X")
c75 = cond provider ioper limit (oo client spdf (7*day), Top)
c8a = oo client choose_28 (1*day) >->
  rec "X" ((c133 &&& wait (28*day)) >-> Var "X")
c8b = oo client choose_84 (1*day) >->
  rec "X" ((c133 &&& wait (84*day)) >-> Var "X")
c8c = oo client choose_168 (1*day) >->
  rec "X" ((c133 &&& wait (168*day)) >-> Var "X")
c8 = c8a ||| c8b ||| c8c
c92 = pp client mail limit &&&
      cond client send_data limit(oo provider mail (7*day), Top) -- 9.3
c94 = rec "X" (cond client pay limit  (
                  (oo provider send_report (10*day) >-> Var "X"),
                  Top))
c101 = oo provider return_data (1*day)
c102 = rec "X" ( pp client request limit >-> oo provider response (1*day) >-> Var "X")
c111 = pp provider personal_data limit
c112 = c71 |> pp provider terminate limit
c131 = rec "X" (cond provider supply_internet (7*day)(
             Var "X",
             pp client terminate limit))
c132 = ff provider terminate limit |||
  cond provider notify_termination limit(
  wait (2*day) >-> pp provider terminate limit, Top)
c133 = oo client pay (14*day) |> pp provider terminate limit
c161 = oo provider seize limit

c = (t1 ||| t2 ||| (c3 &&& c51 &&&
                   c72 &&& c75 &&& c8 &&& c92 &&& c94 &&&
                   c102 &&& c111 &&& c112 &&& c132 &&& c161)) >-> c101

c_a = (t1 ||| t2 ||| (c3 &&& c51 &&&
                   c72 &&& c75 &&& c8a &&& c92 &&& c94 &&&
                   c102 &&& c111 &&& c112 &&& c132 &&& c161)) >-> c101

short = (t1 ||| t2 ||| c132 &&& c112 &&& c8) >-> c101

short_a = (t1 ||| t2 ||| c132 &&& c112 &&& (c8a|||c8b)) >-> c101

aut = constructAutomaton short_a

contracts =
  do
    putStr $ "t1 &"
    putStr $ toLaTeX t1
    putStrLn $ "\\\\"
    putStr $ "t2 &"
    putStr $ toLaTeX t2
    putStrLn $ "\\\\"
    putStr $ "c2 &"
    putStr $ toLaTeX c3
    putStrLn $ "\\\\"
    putStr $ "c{51} &"
    putStr $ toLaTeX c51
    putStrLn $ "\\\\"
    putStr $ "c{71} &"
    putStr $ toLaTeX c71
    putStrLn $ "\\\\"
    putStr $ "c{73} &"
    putStr $ toLaTeX c73
    putStrLn $ "\\\\"
    putStr $ "c{74} &"
    putStr $ toLaTeX c74
    putStrLn $ "\\\\"
    putStr $ "c{72} &"
    putStr $ toLaTeX c72
    putStrLn $ "\\\\"
    putStr $ "c{75} &"
    putStr $ toLaTeX c75
    putStrLn $ "\\\\"
    putStr $ "c{8}a &"
    putStr $ toLaTeX c8a
    putStrLn $ "\\\\"
    putStr $ "c{8}b &"
    putStr $ toLaTeX c8b
    putStrLn $ "\\\\"
    putStr $ "c{8}c &"
    putStr $ toLaTeX c8c
    putStrLn $ "\\\\"
    putStr $ "c{8} &"
    putStr $ toLaTeX c8
    putStrLn $ "\\\\"
    putStr $ "c{92} &"
    putStr $ toLaTeX c92
    putStrLn $ "\\\\"
    putStr $ "c{94} &"
    putStr $ toLaTeX c94
    putStrLn $ "\\\\"
    putStr $ "c{101}&"
    putStr $ toLaTeX c101
    putStrLn $ "\\\\"
    putStr $ "c{102}&"
    putStr $ toLaTeX c102
    putStrLn $ "\\\\"
    putStr $ "c{111}&"
    putStr $ toLaTeX c111
    putStrLn $ "\\\\"
    putStr $ "c{112}&"
    putStr $ toLaTeX c112
    putStrLn $ "\\\\"
    putStr $ "c{131}&"
    putStr $ toLaTeX c131
    putStrLn $ "\\\\"
    putStr $ "c{132}&"
    putStr $ toLaTeX c132
    putStrLn $ "\\\\"
    putStr $ "c{133}&"
    putStr $ toLaTeX c133
    putStrLn $ "\\\\"
    putStr $ "c{161}&"
    putStr $ toLaTeX c161
    putStrLn $ "\\\\"

main =
  do
    -- putStrLn $ toLaTeX c1
    -- putStrLn $ toLaTeX c2
    -- putStrLn $ show $ constructAutomaton c
    -- putStrLn $ showShortestConflicts c
    -- putStrLn $ showAllConflicts (c24 &&& c26 &&& c27 &&& c29)
    -- putStrLn $ toLaTeXShortestConflicts shorta
    -- putStrLn $ "------------------------"
    -- putStrLn $ toLaTeXAllConflicts shorta
    -- putStrLn $ "------------------------"
    -- putStrLn $ "------------------------"
    -- putStrLn $ toLaTeXShortestConflicts ca
    -- putStrLn $ "------------------------"
    -- putStrLn $ toLaTeXAllConflicts ca
    -- putStrLn $ "------------------------"
    -- putStrLn $ "------------------------"
    -- putStrLn $ show $ length (states (constructAutomaton a))
    -- putStrLn $ showAllConflicts shorta
    -- putStrLn $ "------------------------"

    putStrLn $ showShortestConflictTraces short_a
