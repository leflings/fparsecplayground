// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FParsec
open Ast
open Parser
open PrettyPrint

[<EntryPoint>]
let main argv = 
    let prog =
        runParserOnFile program () "completeTest.java" (System.Text.Encoding.UTF8)
        |> function
        | Success(result,_,_) -> result
        | Failure(_,_,_) -> []

    let doc = pprogram prog
    let str = printDocument doc
    printf "%s" str
    System.Console.ReadLine() |> ignore
    0
   
