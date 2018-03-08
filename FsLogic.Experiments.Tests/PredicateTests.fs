module Predicates

open Xunit
open Swensen.Unquote
open FsLogic
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Predicates

let getValues<'T> =
    List.map ReifiedTerm.GetDeterminedValue
    >> List.map (fun obj -> obj :?> 'T)

let getSortedValues<'T when 'T : comparison> = 
    getValues<'T> >> List.sort

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
    res |> getSortedValues =! ([ "Jacob"; "Abraham"; "Isaac" ] |> List.sort)

[<Fact>]
let ``brothers should return distinct`` () =
    let res = run -1 (fun q -> 
        let (x, y) = fresh()
        brother x y &&& q *=* ~~(x, y))
    res |> getSortedValues<string*string> =! ([ ("Ismael", "Isaac"); ("Isaac", "Ismael") ] |> List.sort)
