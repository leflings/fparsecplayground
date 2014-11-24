System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\TNX.NamesAndPassword.dll"
#r "bin\Debug\Zlib.Portable.dll"
#load "Solver.fs"

open Solver

#time "on";;

let boardStr = """
heoc
sonb
urma
btrp
"""
let words = [8;5;3]
words |> forOne boardStr |> Seq.iter (printfn "%A")
words |> forAll boardStr

let sol = words |> solutions boardStr |> Seq.map id
sol |> printSolutions
sol
|> Seq.filter (List.exists (fst >> (=) "wing"))
|> Seq.filter (List.exists (fst >> (=) "path"))
|> printSolutions
(boardStr,sol) ||> printSolutionsAndPaths

let ps = [(3, 0); (2, 1); (2, 2); (1, 2); (0, 3); (0, 2)]
let printPath b ps =
    b
    |> List.mapi (fun x -> List.mapi (fun y e -> if List.exists ((=) (x,y)) ps then sprintf " %c " e else "   "))
    |> ListBoard.printAny
    |> ignore

printPath (ListBoard.fromString boardStr) ps



