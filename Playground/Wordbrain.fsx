let board =
    [ [ 'R'; 'P'; 'H' ]
      [ 'O'; 'O'; 'T' ]
      [ 'E'; 'T'; 'O' ] ]

type Move =
    | N | E | W | S
    | NW | NE | SW | SE
type Position = int * int
type Board = (bool * char)[,]
type ListBoard = bool list list

type Tree =
    | Leaf of Position
    | Node of Position * Tree list

let sum2 (x1,y1) (x2,y2) = (x1+x2,y1+y2)

let rec dir = function
    | N -> (-1,0)
    | E -> (0,1)
    | W -> (0,-1)
    | S -> (1,0)
    | NW -> (dir N, dir W) ||> sum2
    | NE -> (dir N, dir E) ||> sum2
    | SW -> (dir S, dir W) ||> sum2
    | SE -> (dir S, dir E) ||> sum2

let moves = [N; E; W; S; NW; NE; SW; SE]

let boardDim (b : Board) = (Array2D.length1 b, Array2D.length2 b)

let boardArr : Board = Array2D.init 3 3 (fun x y -> false, List.nth board x |> List.nth <| y)

let listBoardDim (b : ListBoard) = List.length b

let word1 = [Some('T'); None; None; None; None]
let word2 = [Some('R'); None; None; None]

let canMove (b : ListBoard) (p : Position) (m : Move) =
    let n = listBoardDim b
    let _ = List.nth 
    let x,y : Position =  sum2 p (dir m) 
    0 <= x && x < n
    &&
    0 <= y && y < n
    && x,b ||> List.nth //x |> List.nth y |> not 

let move (p: Position) (m : Move) : Position = sum2 p (dir m)

let movesFrom (b : Board) (p : Position) =
    moves |> List.filter (canMove b p)

let rec moveTrees (b: Board) (p: Position) n : Tree =
    match n with
    | 0 -> Leaf p
    | _ ->
        let moves = movesFrom b p
        Node(p, moves |> List.map (fun m -> moveTrees b (move p m) (n-1)))
        
moveTrees boardArr (0,0) 5
