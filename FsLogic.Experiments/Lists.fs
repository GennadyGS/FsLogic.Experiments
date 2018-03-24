﻿module FsLogic.Experiments.Lists

open FsLogic.Goal

let rec membero item list = 
       let (head, tail, tail2) = fresh()
       matche list
            [ cons item tail2 ->> []
              cons head tail   ->> [ recurse (fun () -> membero item tail) ]]

let rec listNotLonger list1 list2 = 
    let head1, tail1, tail2 = fresh()
    matche (list1, list2) 
        [ (nil, __) ->> []
          (cons head1 tail1, cons __ tail2) ->> [ recurse (fun () -> listNotLonger tail1 tail2) ]]

let rec notMembero item list = 
    let (head, tail) = fresh()
    matche list
        [ nil ->> []
          cons head tail ->> [ item *<>* head; recurse (fun () -> notMembero item tail) ]]

