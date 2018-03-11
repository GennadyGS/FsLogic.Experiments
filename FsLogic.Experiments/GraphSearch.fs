module FsLogic.Experiments.GraphSearch

open FsLogic.Goal

let rec graphPath graphLink source target path = 
    let medium, subPath = fresh()
    conde [
        [source *=* target; path *=* ~~[target]]
        [graphLink source medium; recurse (fun () -> graphPath graphLink medium target subPath); path *=* cons source subPath]
    ]
