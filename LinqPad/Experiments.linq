<Query Kind="FSharpProgram">
  <Reference Relative="..\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll">C:\Source\Private\FsLogic.Experiments\Lib\FsLogic\FsLogic\bin\Debug\netstandard2.0\FsLogic.dll</Reference>
  <Reference>C:\Users\Gennadii_Saltyshchak\.nuget\packages\netstandard.library\2.0.3\build\netstandard2.0\ref\netstandard.dll</Reference>
  <Namespace>FsLogic</Namespace>
  <Namespace>FsLogic.Goal</Namespace>
  <Namespace>FsLogic.Substitution</Namespace>
  <Namespace>System</Namespace>
  <AppConfig>
    <Content>
      <configuration>
        <compilation debug="true" targetFramework="4.7.1">
          <assemblies>
            <add assembly="netstandard, Version=2.0.0.0, Culture=neutral, PublicKeyToken=cc7b13ffcd2ddd51" />
          </assemblies>
        </compilation>
      </configuration>
    </Content>
  </AppConfig>
</Query>

let dump x = x.Dump() |> ignore

//run -1 (fun q -> (q *=* 1Z) ||| (q *=* 2Z)) |> dump
//
//run -1 (fun q -> (q *=* 1Z) &&& (q *=* 2Z)) |> dump
//
//run -1 (fun q -> (q *=* 1Z) &&& (q *<>* 2Z)) |> dump
//
//run -1 (fun q -> q *<>* 1Z) |> dump
//
//run -1 (fun q -> (q *<>* 1Z) &&& (q *<>* 2Z)) |> dump
//
let a = fresh()
let b = fresh()
run -1 (fun q -> ((a *=* 1Z) ||| (b *=* 2Z)) &&& (a *<>* 1Z) &&& (b *<>* 2Z) &&& (q *=* ~~[a; b])) |> dump