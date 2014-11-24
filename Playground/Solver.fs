module Solver

type Move =
    | N | E | W | S
    | NW | NE | SW | SE
type Position = int * int
type Board<'a> = 'a list list

let sum2 (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let moves = [N; E; W; S; NW; NE; SW; SE]
let rec dir = function
    | N -> (-1,0)
    | E -> (0,1)
    | W -> (0,-1)
    | S -> (1,0)
    | NW -> (dir N, dir W) ||> sum2
    | NE -> (dir N, dir E) ||> sum2
    | SW -> (dir S, dir W) ||> sum2
    | SE -> (dir S, dir E) ||> sum2
let move p m = sum2 p (dir m)

type Tree<'a> = | Node of 'a * Tree<'a> list
let rec mapTree f tree =
    match tree with
    | Node(e, ns) -> Node(f e, List.map (mapTree f) ns)

let toLower (s:string) = s.ToLower()

module Words =
    let english =
        let enWords = TNX.NamesAndPassword.WorldDictionary.GetWordsForCulture("en") |> Seq.map toLower
        enWords |> Set.ofSeq

module ArrayBoard =
    let get (arr : 'a[,]) (x,y) = arr.[x,y]
    let fromList b =
        let size = List.length b
        Array2D.init size size (fun x y -> List.nth (List.nth b x) y)

module ListBoard = 
    let printAny b = b |> List.map (String.concat "" >> printfn "%s")
    let print b = b |> List.map (Array.ofList >> fun e -> new System.String(e)) |> List.iter (printfn "%s")
    let fromString (str:string) : Board<char> =
        let rows = str.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
        rows |> List.map (fun (s : string) -> s.ToCharArray() |> List.ofArray)
    let size b = List.length b
    let getVal b (x,y) = List.nth (List.nth b x) y
    let setVal (x,y) n b =
        let mapY = List.mapi (fun i e -> if i = y then n else e)
        b |> List.mapi (fun i e -> if i = x then mapY e else e)
    let swap b p1 p2 =
        let x1, x2 = getVal b p1, getVal b p2
        b |> setVal p1 x2 |> setVal p2 x1
    let setAndTrickleUp v p b =
        let rec trickleUp p b =
            match move p N with
            | -1, _ -> b
            | p' -> trickleUp p' (swap b p p')
        let b' = setVal p v b
        trickleUp p b'

    let removeChar = setAndTrickleUp (' ')
    let removeChars ps b = (ps |> List.sortBy fst |> List.rev, b) ||> List.foldBack removeChar
    let availableFields b =
        b
        |> List.mapi (fun x -> List.mapi (fun y -> function | ' ' -> None | _ -> Some(x,y))) 
        |> List.collect id |> List.choose id


module Logic =
    let canMove b p m =
        let n = ListBoard.size b
        let x,y = move p m
        0 <= x && x < n
        &&
        0 <= y && y < n
        &&
        ListBoard.getVal b (x,y)

    let movesFrom b p = moves |> List.filter (canMove b p)
    let rec treeFrom b n p =
        match n with
        | 0 | 1 -> Node(p, [])
        | _ ->
            let moves = movesFrom b p
            let b' = ListBoard.setVal p false b
            let k = fun m -> treeFrom b' (n-1) (move p m) 
            Node(p, moves |> List.map k)

    let allBranches tree =
        let rec f word words (Node(c, es)) =
            let word' = c :: word
            match es with
            | [] -> [List.rev word']
            | _ -> es |> List.collect (f word' words)
        f [] [] tree

type Result =
    | Row of seq<string * Position list>
    | Result of seq<Result>

module Aux =
    let rec insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

    let rec flatResult = function
        | Row s -> s |> Seq.singleton
        | Result rs -> Seq.collect (flatResult) rs

    let result board lengths =
        let wordsAndPathsFrom b bA n p =
            Logic.treeFrom b n p
            |> Logic.allBranches
            |> Seq.filter (Seq.length >> (=) n)
            |> Seq.map (fun x ->
                x |> (Seq.map (ArrayBoard.get bA) >> Array.ofSeq >> fun e -> new System.String(e) |> toLower), x)

        let walkFrom b n p =
            let board : Board<bool> = b |> List.map (List.map (function | ' ' -> false | _ -> true)) 
            let boardArr = ArrayBoard.fromList b
            wordsAndPathsFrom board boardArr n p
            |> Seq.filter (fun x -> Words.english.Contains(fst x))

        let walkFromAll b ns =
            let run n b =
                ListBoard.availableFields b
                |> Seq.collect (walkFrom b n)
                |> Seq.distinct

            let rec recur nstack acc b =
                match nstack with
                | [] -> acc |> List.rev |> Seq.ofList |> Row
                | n :: ns ->
                    run n b
                    |> Seq.map (fun (w,ps) ->
                            let b' = ListBoard.removeChars ps b
                            let acc' = (w,ps)::acc
                            recur ns acc' b')
                    |> Result
            recur ns [] b

        walkFromAll board lengths |> flatResult


let forOne str ns = Aux.result (ListBoard.fromString str) ns
let forAll str ns =
    let nns = ns |> Aux.permutations |> Seq.distinct |> Seq.toList
    seq { for ns in nns do
            yield! forOne str ns }

let solutions str ns =
    forAll str ns
    |> Seq.map (List.ofSeq)
    |> Seq.distinctBy (List.map fst >> List.sort)
    |> Seq.sortBy (List.map fst)

let printSolutions sols = sols |> Seq.iter (List.map fst >> printfn "%A")

let printSolutionsAndPaths str sols =
    let printPath b ps =
        let b' = b |> List.mapi (fun x -> List.mapi (fun y e -> if List.exists ((=) (x,y)) ps then sprintf " %c " e else "   "))
        b' |> ListBoard.printAny |> ignore
        b'
//    let sols = solutions str ns
    let board = ListBoard.fromString str
    sols
    |> Seq.iter (fun sol ->
        (board,sol)
        ||> List.fold (fun b (w,p) ->
            printfn "%A" w
            printPath b p|> ignore
            ListBoard.removeChars p b)
            |> ignore
        printfn "-----------------------"
        )

    
