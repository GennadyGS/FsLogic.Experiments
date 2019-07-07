module Parsing

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal

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
