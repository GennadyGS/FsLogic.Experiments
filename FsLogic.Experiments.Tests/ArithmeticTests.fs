module Arythmetic

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Arithmetic
open FsLogic.Experiments.Tests
open FsLogic.Experiments.Arithmetic

[<Fact>]
let ``pluso``() = 
    let res = run -1 (fun q -> pluso q (ofNat 5) (ofNat 7))
    res |> Utils.getNatValues =! [2]

[<Fact>]
let ``greaterThan should return free variable greater``() = 
    let res = run -1 (fun q -> ofNat 7 *>=* ofNat 2)
    res =! [Free 0]

[<Fact>]
let ``greaterThan should return free variable equals``() = 
    let res = run -1 (fun q -> ofNat 7 *>=* ofNat 7)
    res =! [Free 0]

[<Fact>]
let ``greaterThan should return empty variable less than``() = 
    let res = run -1 (fun q -> ofNat 7 *>=* ofNat 8)
    res =! []

[<Fact>]
let ``greaterThan should return list of options``() = 
    let res = run -1 (fun q -> ofNat 4 *>=* q)
    res |> Utils.getSortedNatValues =! [0; 1; 2; 3; 4]
