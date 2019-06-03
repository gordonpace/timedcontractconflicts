{-# LANGUAGE OverloadedStrings #-}

module Main where

import Helper
import Logic
import Semantics
import ContractStateSpace
import Conflicts

client = party "Client"
provider = party "Provider"
plaw = party "Law"
it = party "InternetTraffic"

pis = action "pis" -- "Provider IP as stipulates"
mitr = action "mitr" -- "Measure Internet Traffic"
sfi = action "sfi" -- "Supply False Information"
sisc = action "sisc" -- "Suspend Internet Services to the client"
pay = action "pay" -- "Pay for the Internet Service"
ih = action "ih" -- "Is High"
npl = action "npl" -- "Notify the Provider payment later"
lowert = action "lowert" -- "Lower Internet Traffic to Normal"
pay2 = action "pay2" -- "Pay 2*price"
pay3 = action "pay3" -- "Pay 3*price"
oper = action "oper" -- "Operative"
spdf = action "spdf" -- "Submit PDF"
uaccount = action "uaccount" -- "Have an email and user account"
offerpe = action "offerpe" -- "Offer Password and Equipment"
ri = action "ri" -- "Receive of all necessary data about the client"
ruse = action "ruse" -- "Send a Report of Internet Usage" --
rdata = action "rdata" -- "Returns Client Personal Date to Original Status"
deletedata = action "deletedata" -- "Delete and not Use Data"
request = action "request" -- "Guarantee CRD responsive to request"
statistics = action "statistics" -- "Alter, delete and use PD only for statistics"
terminatea = action "terminatea" -- "Teminate the Agreement"
provis = action "provis" -- "Provide Internet Services"
notifyt = action "notifyt" -- "Notify Agreement Termination"
sa = action "sa" -- "Seize any activity"
alaw = action "alaw" -- "Law stipulates"

c1 = oo provider pis 365
c2 = ff client sfi 0 |> c5
c3 = pp provider mitr 365
c4 = pp client mitr 365
c5 = pp provider sisc 365
c6 = cond it ih 365 ((oo client pay 1) ||| (c7 >-> (c8 &&& c9)), Top)
c7 = oo client npl 1
c8 = oo client lowert 1 |> c10
c9 = oo client pay2 3
c10 = oo client pay3 6
c11 =  cond it oper 365 (oo client spdf 7, Top)
c12 = pp client pay 28 ||| pp client pay 84 ||| pp client pay 168
c13 = pp client uaccount 365
c14 = cond provider ri 365 (oo provider offerpe 7, Top)
c15 = cond  client pay 365 (c16, Top)
c16 = oo provider ruse 10
c17 = oo provider rdata 366
c18 = oo provider deletedata 366
c19 = oo provider request 1
c20 = pp provider statistics 365
c21 =  cond client sfi 365 (c5, Top)
c22 = pp client terminatea 365
c23 = pp provider terminatea 365
c24 = cond provider provis 7 ( Top, c22)
c25 = ff provider terminatea 365
c26 = (c25  ||| cond provider notifyt 365 (wait(2)>->c23, Top))
c27 = cond client pay 14 (Top, c23)
c28 = ff provider sa 365
c29 = cond plaw alaw 365 (c28, Top)




c = c2 >-> (c1 &&& c3 &&& c4 &&& c5 &&& c6 &&& c11 &&& c12 &&& c13 &&& c14 &&& c15 &&& c19 &&& c20 &&& c21 &&& c22 &&& c24 &&& c26 &&& c27 &&& c29 ) >-> (c17 &&& c18)

d = c2 >-> (c1 &&& c5 &&& c6 &&& c12 &&& c13 &&& c14 &&& c15 &&& c19 &&& c20 &&& c21 &&& c22 &&& c24 &&& c26 &&& c27 &&& c29 ) >-> (c17 &&& c18)


main =
  do
    -- putStrLn $ toLaTeX c1
    -- putStrLn $ toLaTeX c2
    putStrLn $ show $ hasConflict d
    putStrLn $ showShortestConflicts d
    -- putStrLn $ showAllConflicts (c24 &&& c26 &&& c27 &&& c29)
