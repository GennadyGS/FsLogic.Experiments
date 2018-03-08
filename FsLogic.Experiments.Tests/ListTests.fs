module FsLogic.Experiments.Tests.ListTests

open Expecto
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Lists

[<Tests>]
let listTests = 
    testList "Lists" [
        testCase "membero should return singleton" (fun _ ->
            let res = run -1 (fun q -> membero q ~~[1])
            res =! [ Det 1 ]
        )

        testCase "membero should return empty for empty list" (fun _ ->
            let res = run -1 (fun q -> membero q nil)
            res =! []
        )

        testCase "membero should return all items of list" (fun _ ->
            let res = run -1 (fun q -> membero q ~~[1; 2; 3])
            res =! [Det 1; Det 2; Det 3]
        )
    ]
