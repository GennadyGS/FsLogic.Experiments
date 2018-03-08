module FsLogic.Experiments.Tests.ListTests

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Lists

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
