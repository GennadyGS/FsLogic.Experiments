﻿module Parsing

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal
open FSharp.Quotations.Evaluator
open Microsoft.FSharp.Quotations

type Step = Up | Down

let isStep step list rest =
    conde [
        [list *=* cons ~~"up" rest; step *=* prim Up]
        [list *=* cons ~~"down" rest; step *=* prim Down]
    ]

let rec isMove move list rest =
    let list1, step1, steps1 = fresh ()
    conde [
        [isStep step1 list rest; move *=* cons step1 nil]
        [isStep step1 list list1; recurse (fun () -> isMove steps1 list1 rest); move *=* cons step1 steps1]
    ]

[<ReflectedDefinition>]
let freshVars () = Unchecked.defaultof<'r>

[<ReflectedDefinition>]
let next (x : 'a list) = true

[<ReflectedDefinition>]
let isStep2 step =
    next ["up"] && step = Up ||
    next ["down"] && step = Down

let rec isMove2 move =
    let step', move' = freshVars ()
    isStep2 step' && move = [step'] ||
    isStep2 step' && isMove2 move' && move = step' :: move'

[<ReflectedDefinition>]
let rec isList isItem list = 
    let item', list' = freshVars ()
    isItem item' && list = [item'] ||
    isItem item' && isList isItem list' && list = item' :: list'

[<ReflectedDefinition>]
let isMove3 move = isList isStep2 move

[<Fact>]
let ``step should be parsed``() =
    let res = run -1 (fun q -> isStep q ~~["up"] nil)
    res =! [ Det Up ]

[<Fact>]
let ``one-step move should be parsed``() =
    let res = run -1 (fun q -> isMove q ~~["down"] nil)
    res =! [ Det [Down] ]

[<Fact>]
let ``move should be parsed``() =
    let res = run -1 (fun q -> isMove q ~~["up"; "down"; "up"] nil)
    res =! [ Det [Up; Down; Up] ]

[<Fact>]
let ``move should be predicted``() =
    let res = run 2 (fun q -> 
        let list, rest = fresh ()
        isMove list (cons ~~"up" (cons ~~"down" (cons q rest))) rest)
    res =! [ Det "up"; Det "down" ]

[<Fact>]
let ``move should be parsed 2``() =
    let mkBody moves targetMoves = isMove moves ~~targetMoves nil
    let res = run -1 (fun q -> mkBody q ["up"; "down"; "up"])
    res =! [ Det [Up; Down; Up] ]

[<Fact>]
let ``move should be parsed 3``() =
    let res = run -1 (fun q -> 
        let rec isList isItem resultList list rest = 
            let list1, item1, list2 = fresh ()
            conde [
                [isItem item1 list rest; resultList *=* cons item1 nil]
                [isItem item1 list list1; recurse (fun () -> isList isItem list2 list1 rest); resultList *=* cons item1 list2]
            ]
        let isMove move list rest = isList isStep move list rest
        isMove q ~~["up"; "down"; "up"] nil
    )
    res =! [ Det [Up; Down; Up] ]

type sentence = {
    Noun: string
    Verb: string
}

[<Fact>]
let ``sentence should be parsed``() =
    let isNoun noun list rest =
        conde [
            [list *=* cons ~~"cat" rest; noun *=* ~~"cat"]
            [list *=* cons ~~"mouse" rest; noun *=* ~~"mouse"]
        ]
    let isArticle article list rest = 
        conde [
            [list *=* cons ~~"a" rest; article *=* ~~"a"]
            [list *=* cons ~~"the" rest; article *=* ~~"the"]
        ]
    let isNounPhrase noun list rest = 
        let article, list1 = fresh()
        conde [
            [isArticle article list list1; isNoun noun list1 rest]
            [isNoun noun list rest]
        ]
    let isVerbPhrase verb list rest = 
        conde [
            [list *=* cons ~~"catch" rest; verb *=* ~~"catch"]
            [list *=* cons ~~"hide" rest; verb *=* ~~"hide"]
        ]
    let isSentence sentence list rest =
        let nounPhrase, verbPhrase, list1 = fresh()
        isNounPhrase nounPhrase list list1 &&& 
        isVerbPhrase verbPhrase list1 rest &&&
        sentence *=* ~~(nounPhrase, verbPhrase)
    let res = run -1 (fun q -> 
        isSentence q ~~["a"; "cat"; "catch"] nil
    )
    res =! [Det ("cat", "catch")]

let buildGoalExpr predicateExpr sourceListExpr = 
    <@ 
    let isStep step list rest =
        conde [
            [unify (list, cons (prim "up") rest); unify (step, prim Up)]
            [unify (list, cons (prim "down") rest); unify (step, prim Down)]
        ]
    fun arg -> isStep arg %sourceListExpr nil 
    @>

let buildGoal predicateExpr sourceList = 
    let sourceListTerm = sourceList |> List.map prim |> ofList
    let goalExpr = buildGoalExpr predicateExpr <@ sourceListTerm @>
    goalExpr.Evaluate()

[<Fact>]
let ``step should be parsed with quotations``() =
    let sourceList = ["up"]
    let predicateExpr = <@ 
        fun step ->
            next ["up"] && step = Up ||
            next ["down"] && step = Down
    @>
    let goal = buildGoal predicateExpr sourceList
    let res = run -1 goal
    res =! [ Det Up ]
