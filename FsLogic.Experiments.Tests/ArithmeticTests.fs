module Arythmetic

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FsLogic.Experiments.Lists
open FsLogic.Arithmetic
open FsLogic.Experiments.Tests

[<Fact>]
let ``pluso``() = 
    let res = run -1 (fun q -> pluso q (ofNat 5) (ofNat 7))
    res |> Utils.getNatValues =! [2]

