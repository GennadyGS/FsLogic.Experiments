module Predicates

open Xunit
open Swensen.Unquote
open FsLogic
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Predicates
open FsLogic.Experiments.Tests

let private fatherList = [
        ("Abraham", "Ismael")
        ("Abraham", "Isaac")
        ("Isaac", "Jacob")
        ("Jacob", "Benjamin")
    ]

let private fathero =
    binaryPredicate fatherList

let inline private fathero2 arg =
    fatherList 
    |> List.map (fun (father, son) -> [arg *=* (~~father, ~~son)])
    |> conde

let internal grandFathero gf gc = 
    let f = fresh()
    fathero gf f &&& fathero f gc

let internal brother x y = 
    let f = fresh()
    fathero f x &&& fathero f y &&& x*<>*y

let rec internal ancestor x y = 
    let z = fresh()
    fathero x y ||| (fathero x z &&& recurse (fun () -> ancestor z y))

[<Fact>]
let ``should infer result from simple facts`` () =
    let res = run -1 (fun q -> fathero q ~~"Isaac")
    res =! [ Det "Abraham" ]

[<Fact>]
let ``should infer result from rule``() = 
    let res = run -1 (fun q -> grandFathero q ~~"Benjamin")
    res =! [ Det "Isaac" ]

[<Fact>]
let ``should infer result from recursive rule`` () =
    let res = run -1 (fun q -> ancestor q ~~"Benjamin")
    res |> Utils.getSortedValues =! ([ "Jacob"; "Abraham"; "Isaac" ] |> List.sort)

[<Fact>]
let ``brothers should return distinct`` () =
    let res = run -1 (fun q -> 
        let (x, y) = fresh()
        brother x y &&& q *=* ~~(x, y))
    res |> Utils.getSortedValues<string*string> =! ([ ("Ismael", "Isaac"); ("Isaac", "Ismael") ] |> List.sort)

[<Fact>]
let ``Implication rule`` () =
    let pred a b = (a *<>* 1Z) ||| (b *=* 2Z)
    let res = run -1 (fun q -> pred 1Z q)
    res =! [ Det 2 ]

[<Fact>]
let ``Unify tuples`` () =
    let res = run -1 (fun q -> ~~(1Z, 2Z) *=* ~~(q, 2Z))
    res =! [ Det 1 ]

[<Fact>]
let ``Predicate`` () =
    let res = run -1 (fun q -> fathero2 (q, ~~"Isaac"))
    res =! [ Det "Abraham" ]
