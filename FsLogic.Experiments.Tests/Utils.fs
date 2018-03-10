module FsLogic.Experiments.Tests.Utils

open FsLogic.Substitution
open FsLogic.Arithmetic

let getValues<'T> =
    List.map ReifiedTerm.GetDeterminedValue
    >> List.map (fun obj -> obj :?> 'T)

let getSortedValues<'T when 'T : comparison> = 
    getValues<'T> >> List.sort

let getNatValues = 
    getValues >> List.map toNat

let getSortedNatValues = 
    getNatValues >> List.sort

