module Parsing

open Xunit
open Swensen.Unquote
open FsLogic.Substitution
open FsLogic.Goal

let step list rest =
    conde [
        [list *=* cons ~~"up" rest]
        [list *=* cons ~~"down" rest]
    ]

let rec move list rest =
    let list1 = fresh ()
    conde [
        [step list rest]
        [step list list1; recurse (fun () -> move list1 rest)]
    ]

[<Fact>]
let ``sentence should be parsed``() =
    let res = run -1 (fun q -> move ~~["up"; "down"; "up"] nil)
    res =! [ Free 0 ]
