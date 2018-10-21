<Query Kind="FSharpExpression">
  <Reference Relative="..\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll">D:\Source\fslogic.experiments\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll</Reference>
  <Namespace>FsLogic</Namespace>
  <Namespace>FsLogic.Goal</Namespace>
  <Namespace>FsLogic.Substitution</Namespace>
</Query>

let dump x = x.Dump() |> ignore

run -1 (fun q -> (q *=* 1Z) ||| (q *=* 2Z)) |> dump

run -1 (fun q -> (q *=* 1Z) &&& (q *=* 2Z)) |> dump

run -1 (fun q -> (q *=* 1Z) &&& (q *<>* 2Z)) |> dump

run -1 (fun q -> q *<>* 1Z) |> dump

run -1 (fun q -> (q *<>* 1Z) &&& (q *<>* 2Z)) |> dump

let a = fresh()
let b = fresh()
run -1 (fun q -> (a *<>* 1Z) &&& (b *<>* 2Z) &&& (q *=* ~~[a; b])) |> dump
