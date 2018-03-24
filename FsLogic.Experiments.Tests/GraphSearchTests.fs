module GraphSearch

open Xunit
open Swensen.Unquote
open FsLogic.Goal
open FsLogic.Substitution
open FsLogic.Experiments.Predicates
open FsLogic.Experiments.GraphSearch

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

[<Fact>]
let ``graphPath should find first path`` () =
    let res = run 1 (fun q -> graphPath graphLink ~~"a" ~~"c" q)
    res =! [ Det ["a"; "c"] ]

[<Fact>]
let ``graphPath should find non-cyclic path`` () =
    let res = run 1 (fun q -> graphPath graphLink ~~"a" ~~"f" q)
    res =! [ Det ["a"; "c"; "f"] ]
