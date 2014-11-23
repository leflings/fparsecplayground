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
    let setAndTrickleUp v b p =
        let b' = setVal p v b
        let rec trickleUp b p =
            match move p N with
            | -1, _ -> b
            | p' -> trickleUp (swap b p p') p'
        trickleUp b' p
    let removeChar = setAndTrickleUp (' ')
    let removeChars b ps =
        (b,List.sortBy fst ps) ||> List.fold removeChar
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


    

let solutions boardStr lengths =
    let wordsAndPathsFrom b bA n p =

        Logic.treeFrom b n p
        |> Logic.allBranches
        |> Seq.filter (Seq.length >> (=) n)
        |> Seq.map (fun x ->
            x |> (Seq.map (ArrayBoard.get bA) >> Array.ofSeq >> fun e -> new System.String(e) |> toLower), x)

    let test b n p =
        let board : Board<bool> = b |> List.map (List.map (function | ' ' -> false | _ -> true)) 
        let boardArr = ArrayBoard.fromList b
        wordsAndPathsFrom board boardArr n p
        |> Seq.filter (fun x -> Words.english.Contains(fst x))

    let testAll b n n2 n3 =
        let ns = [n;n2;n3]
        let run b n ps =
            let newBoard = ListBoard.removeChars b ps
            ListBoard.availableFields newBoard
            |> Seq.collect (fun p -> test newBoard n p)
            |> Seq.distinct

        let kkk() =
            let x1 = run b n []
            let x2 = seq { for w,ps in x1 do yield! run b n2 ps }
            let x3 = seq { for w,ps in x2 do yield! run b n3 ps }
            (x1,x2,x3)

        let haha() =
            seq {
                for n in ns do ()
                }
                

        let combos() =
            seq {
                for w,ps in run b n [] do
                    for w2,ps2 in run b n2 ps do
                        let b2 = ListBoard.removeChars b ps
                        for w3,_ in run b n3 ps2 do
                            yield w,w2,w3
            }
//        combos
        combos()
    let (|SeqEmpty|SeqNotEmpty|) s =
        match Seq.isEmpty s with
        | true -> SeqEmpty
        | false -> SeqNotEmpty
    let board = ListBoard.fromString boardStr
    let n1, n2, n3 =
        match lengths with
        | [] -> 0,0,0
        | x :: [] -> x,0,0
        | x :: y :: [] -> x,y,0
        | x :: y :: z :: _ -> x,y,z
    
//    let n1 = longestWordLength
//    let n2 =
//        let s = (ListBoard.size board)
//        s * s - longestWordLength
//    testAll board n1 n2 n3
    testAll board n1 n2 n3
    |> Seq.append (testAll board n2 n1 n3)
    |> Seq.append (testAll board n3 n2 n1)
    |> Seq.append (testAll board n3 n1 n2)

