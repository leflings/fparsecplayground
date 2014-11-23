System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\TNX.NamesAndPassword.dll"
#r "bin\Debug\Zlib.Portable.dll"
#load "Solver.fs"

open Solver

let boardStr = """
BNGY
AKIS
GLVE
GOER
"""
solutions boardStr [6;3;7] |> Seq.iter (printfn "%A")

 //((<|||) (printfn "%s - %s - %s"))

 |> Seq.distinct |> Seq.filter (fun (a,b,c) -> a = "meat")

let b = ListBoard.fromString boardStr
//let ps = [0,0 ; 1,0 ; 1,1 ; 2,1 ; 2,2]
let ps =
    [2,2 ; 2,1 ; 1,1 ; 1,0 ; 0,0]
    |> List.sortBy fst
ListBoard.removeChars b ps
|> ListBoard.availableFields 
|> List.sortBy fst


let printtuple = (<||) (printfn "%s - %s")


//let b = ListBoard.fromString boardStr
//ListBoard.print b
//let b' = ListBoard.removeChars b [2,0 ; 2,1; 1,1 ; 2,2]
//ListBoard.print b'
//
//b'
//|> List.mapi (fun x -> List.mapi (fun y -> function | ' ' -> None | _ -> Some(x,y))) 
//|> List.collect id
//|> List.choose id
//
//let boardChars = ListBoard.fromString boardStr
//    
//
//let wordsAndPathsFrom b bA n p =
//
//    Logic.treeFrom b n p
//    |> Logic.allBranches
//    |> Seq.filter (Seq.length >> (=) n)
//    |> Seq.map (fun x ->
//        x |> (Seq.map (ArrayBoard.get bA) >> Array.ofSeq >> fun e -> new System.String(e) |> toLower), x)
//
//let test b n p =
//    let board : Board<bool> = b |> List.map (List.map (function | ' ' -> false | _ -> true)) 
//    let boardArr = ArrayBoard.fromList b
//    let wtMap =
//        wordsAndPathsFrom board boardArr n p
//        |> Seq.groupBy fst
//        |> Seq.map (fun (k, vs) -> (k, Seq.map snd vs))
//        |> Map.ofSeq
//    let wordsAndTrails = Map.toSeq wtMap
//    wordsAndTrails
//    |> Seq.map fst
//    |> Set.ofSeq
//    |> Set.intersect Words.english
//    |> Seq.map (fun k -> (k, Map.find k wtMap))
//
//let testAll b n =
//    let n2 =
//        let s = (ListBoard.size b)
//        s * s - n
//    let run b n ps =
//        ps
//        |> Seq.collect (fun p -> test b n p)
//        |> Seq.distinct
//        
//    let s = ListBoard.size b - 1
//    let allFields = [for x in 0..s -> [for y in 0..s -> (x,y)]] |> Seq.collect id
//    let longWords = run b n allFields
//    let combos =
//        seq {
//            for w,pps in longWords do
//                for ps in pps do
//                    let newBoard = ListBoard.removeChars b ps
//                    let fields =
//                        newBoard
//                        |> List.mapi (fun x -> List.mapi (fun y -> function | ' ' -> None | _ -> Some(x,y))) 
//                        |> List.collect id |> List.choose id
//                    for w2 in run newBoard n2 fields do
//                        yield w,fst w2
//        }
//    combos
//
//testAll boardChars 4
//
