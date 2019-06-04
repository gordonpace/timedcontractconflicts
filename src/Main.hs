module Main where

import TheCon

gordon = party "gordon"
emilia = party "emilia"
luis = party "luis"

pay = action "pay"
jump = action "jump"
enter = action "enter"

example1 = oo gordon pay 5
example2 = oo gordon pay 5 >-> ff emilia jump 3
example3 = ((ff luis pay 10 ||| oo gordon pay 5) >-> ff emilia jump 3) |> (wait 5 >-> pp luis jump 5)
example4 = (oo gordon pay 5 >-> ff emilia jump 3) &&& (wait 3 >-> ff gordon pay 10)
example5 = example4 |> oo luis pay 10

example6 = rec "X" (oo gordon pay 5 >-> Var "X")
example7 = rec "X" (oo gordon pay 5 |> Var "X")
example8 = rec "X" (oo gordon pay 5 |> (oo gordon jump 10 &&& Var "X"))
example8' = cond gordon enter 100 (example8, example7)

-- Need idempotency of disjunction and conjunction to terminate
example9 = rec "X" ((oo gordon pay 5 >-> Var "X") &&& oo luis jump 6)
example10 = rec "X" ((oo gordon pay 5 >-> Var "X") ||| oo luis jump 6)

main =
  do
         putStrLn "------------------"
         putStrLn $ show $ constructAutomaton example2
         putStrLn "------------------"
         putStrLn $ show $ constructAutomaton example3
         putStrLn "------------------"
         putStrLn $ show $ inConflict example4
         putStrLn "------------------"
         putStrLn $ showShortestConflicts example4
         putStrLn "------------------"
         putStrLn $ toLaTeXShortestConflicts example4
         putStrLn "------------------"
         putStrLn $ showAllConflicts example4
         putStrLn "------------------"
         putStrLn $ toLaTeXAllConflicts example4
         putStrLn "------------------"
         putStrLn $ show' $ constructAutomaton example8'
         putStrLn "------------------"
