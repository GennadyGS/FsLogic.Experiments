<Query Kind="FSharpExpression">
  <Reference Relative="..\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll">..\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll</Reference>
  <Namespace>FsLogic</Namespace>
  <Namespace>FsLogic.Goal</Namespace>
  <Namespace>FsLogic.Substitution</Namespace>
</Query>

let pred a b = (a *<>* 1Z) ||| (b *=* 2Z)
run -1 (fun q -> pred 1Z q)
