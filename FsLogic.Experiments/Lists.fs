module FsLogic.Experiments.Lists

open FsLogic.Goal

let rec membero x l = 
       let (h, t, t2) = fresh()
       matche l
            [ cons x t2 ->> []
              cons h t   ->> [ recurse (fun () -> membero x t) ]
            ]

let rec listNotLonger l1 l2 = 
    let anyList1, h1, h2, t1, t2 = fresh()
    matche (l1, l2) 
        [ (nil, anyList1) ->> []
          (cons h1 t1, cons h2 t2) ->> [ recurse (fun () -> listNotLonger t1 t2) ]]
