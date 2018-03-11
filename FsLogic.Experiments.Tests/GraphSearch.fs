module GraphSearch

open Xunit
open Swensen.Unquote
open FsLogic.Goal
open FsLogic.Substitution
open FsLogic.Experiments.Predicates

let private graphLink = 
    binaryPredicate [
        ("a", "b")
        ("a", "c")
        ("b", "d")
        ("c", "d")
        ("c", "f")
        ("d", "e")
        ("d", "f")
        ("f", "a")
    ]

let rec private graphPath source target path = 
    let medium, subPath = fresh()
    conde [
        [source *=* target; path *=* ~~[target]]
        [graphLink source medium; recurse (fun () -> graphPath medium target subPath); path *=* cons source subPath]
    ]

[<Fact>]
let ``graphPath should find first path`` () =
    let res = run 1 (fun q -> graphPath ~~"a" ~~"c" q)
    res =! [ Det ["a"; "c"] ]
