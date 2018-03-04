module FsLogic.Experiments.Lists

open FsLogic.Goal

let rec membero x l = 
       let (h, t, t2) = fresh()
       matche l
            [ cons x t2 ->> []
              cons h t   ->> [ recurse (fun () -> membero x t) ]
            ]
