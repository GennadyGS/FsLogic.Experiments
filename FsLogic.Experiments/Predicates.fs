module FsLogic.Experiments.Predicates

open FsLogic.Goal

let inline unaryPredicate values =  
    fun arg -> 
        [for value in values -> [arg *=* ~~value]] 
        |> conde

let inline binaryPredicate values =  
    fun arg1 arg2 -> 
        [for (value1, value2) in values -> [arg1 *=* ~~value1; arg2 *=* ~~value2]] 
        |> conde

