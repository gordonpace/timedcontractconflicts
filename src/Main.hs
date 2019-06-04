module Main where

import TheCon

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

main =
  do
    putStrLn "------------------"
    putStrLn $ show $ constructAutomaton contract
    putStrLn "------------------"
    putStrLn $ show $ inConflict contract
    putStrLn "------------------"
    putStrLn $ showShortestConflicts contract
    putStrLn "------------------"
    putStrLn $ toLaTeXShortestConflicts contract
    putStrLn "------------------"
    putStrLn $ showAllConflicts contract
    putStrLn "------------------"
    putStrLn $ toLaTeXAllConflicts contract
    putStrLn "------------------"
    putStrLn $ show $ constructAutomaton contract
