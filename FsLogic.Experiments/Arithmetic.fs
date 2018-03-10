module FsLogic.Experiments.Arithmetic

open FsLogic.Goal
open FsLogic.Arithmetic

let inline ( *>=* ) a b =
    let x = fresh()
    pluso b x a