module Lists

open Xunit
open Swensen.Unquote
open FsLogic
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

[<Fact>]
let ``notMembero of empty list is true``() = 
    let res = run -1 (fun q -> notMembero 1Z nil)
    res =! [ Free 0 ]

[<Fact>]
let ``notMembero of empty list is true for any value``() =
    let res = run -1 (fun q -> notMembero q nil)
    res =! [ Free 0 ]

[<Fact>]
let ``notMembero should return false when item is member and singleton``() = 
    let res = run -1 (fun q -> notMembero 1Z ~~[1])
    res =! []

[<Fact>]
let ``notMembero should return false when item is member``() = 
    let res = run -1 (fun q -> notMembero 1Z ~~[2; 3; 1; 4])
    res =! []

[<Fact>]
let ``notMembero should return true when item is not member``() = 
    let res = run -1 (fun q -> notMembero 1Z ~~[2; 3; 4])
    res =! [ Free 0 ]

[<Fact>]
let ``listNotLonger shoud return any value on true`` () =
    let res = run -1 (fun q -> listNotLonger nil ~~[1; 2])
    res =! [ Free 0 ]

[<Fact>]
let ``listNotLonger shoud return any value on true 2`` () =
    let res = run -1 (fun q -> listNotLonger ~~[1] ~~[1; 2])
    res =! [ Free 0 ]

[<Fact>]
let ``listNotLonger shoud return any value on true 3`` () =
    let res = run -1 (fun q -> listNotLonger ~~[1; 4; 6] ~~[1; 2; 4])
    res =! [ Free 0 ]

[<Fact>]
let ``listNotLonger shoud return empty on false`` () =
    let res = run -1 (fun q -> listNotLonger ~~[2; 4; 5] ~~[1; 2])
    res =! []

[<Fact>]
let ``listNotLonger shoud return nil not longer any list`` () =
    let res = run -1 (fun q -> listNotLonger nil q)
    res =! [ Free 0 ]

[<Fact>]
let ``listNotLonger shoud return only nil not longer nil`` () =
    let res = run -1 (fun q -> listNotLonger q nil)
    res =! [Det []]

