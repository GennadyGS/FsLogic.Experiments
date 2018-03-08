﻿module FsLogic.Experiments.Tests.PredicateTests

open Expecto
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Predicates

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

[<Tests>]
let tests =
    testList "Facts" [
        testCase "should infer result from simple facts" (fun _ ->
            let res = run -1 (fun q -> fathero q ~~"Isaac")
            res =! [ Det "Abraham" ]
        )

        testCase "should infer result from rule" (fun _ ->
            let res = run -1 (fun q -> grandFathero q ~~"Benjamin")
            res =! [ Det "Isaac" ]
        )

        testCase "should infer result from recursive rule" (fun _ ->
            let res = run -1 (fun q -> ancestor q ~~"Benjamin")
            res |> getSortedValues =! ([ "Jacob"; "Abraham"; "Isaac" ] |> List.sort)
        )
    ]
