System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\TNX.NamesAndPassword.dll"
#r "bin\Debug\Zlib.Portable.dll"
#load "Solver.fs"
open Solver

#time "on";;

let boardStr = """
etrkk
ksiei
utbsk
rreuu
vkadm
"""

Words.english.Count
Words.danish.Count

ListBoard.fromString boardStr
|> ListBoard.print

let words = [5;4;5;6;5]
words |> forOne boardStr |> Seq.iter (printfn "%A")
words |> forAll boardStr

let sol = words |> solutions true boardStr |> Seq.map id

words |> solutions true boardStr |> printSolutions
words |> solutions false boardStr |> printSolutions

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



