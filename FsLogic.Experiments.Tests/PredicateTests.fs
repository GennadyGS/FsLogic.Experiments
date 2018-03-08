module FsLogic.Experiments.Tests.PredicateTests

open Xunit
open FsLogic.Substitution
open FsLogic.Goal
open Swensen.Unquote
open FsLogic.Experiments.Predicates
open FsLogic.Experiments.Lists

let getValues<'T> = 
    List.map ReifiedTerm.GetDeterminedValue
    >> List.map (fun obj -> obj :?> 'T)

let getSortedValues<'T when 'T : comparison> = 
    getValues >> List.sort

let internal fathero =
    binaryPredicate [
        ("Abraham", "Ismael")
        ("Abraham", "Isaac")
        ("Isaac", "Jacob")
        ("Jacob", "Benjamin")
    ]

let internal grandFathero gf gc = 
    let f = fresh()
    fathero gf f &&& fathero f gc

let rec internal ancestor x y = 
    let z = fresh()
    fathero x y ||| (fathero x z &&& recurse (fun () -> ancestor z y))

let f x y =
    x *=* ~~"Abraham" &&& y *=* ~~"Isaac"

[<Fact>]
let ``should infer result from simple facts`` () =
    let res = run -1 (fun q -> f q ~~"Isaac")
    res =! [ Det "Abraham" ]

[<Fact>]
let ``should infer result from simple facts 2`` () =
    let res = run -1 (fun q -> fathero q ~~"Isaac")
    res =! [ Det "Abraham" ]

[<Fact>]
let ``should infer result from rule``() = 
    let res = run -1 (fun q -> grandFathero q ~~"Benjamin")
    res =! [ Det "Isaac" ]

[<Fact>]
let ``should infer result from recursive rule`` () =
    let res = run -1 (fun q -> ancestor q ~~"Benjamin")
    res |> getSortedValues =! ([ "Jacob"; "Abraham"; "Isaac" ] |> List.sort)

[<Fact>]
let ``membero should return singleton``() =
    let res = run -1 (fun q -> membero q ~~[1])
    res =! [ Det 1 ]

[<Fact>]
let ``membero should return empty for empty list``() = 
    let res = run -1 (fun q -> membero q nil)
    res =! []

[<Fact>]
let ``membero should return all items of list``() = 
    let res = run -1 (fun q -> membero q ~~[1; 2; 3])
    res =! [Det 1; Det 2; Det 3]
