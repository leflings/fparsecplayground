module TreeDesign

type Tree<'a> = | Node of 'a * Tree<'a> list

// Extent is an envelope for a tree
// (leftmost, rightmost)
// values are absolute
type Extent = (float * float) list

// Moves a single tree horizontally
// It's position is relative to it's parent
let movetree (tree, x':float) =
    match tree with
    | Node((label,x), subtrees) -> Node((label, x + x'), subtrees)

// Moves an extent
let moveextent (e : Extent, x) : Extent = List.map (fun (p,q) -> (p+x,q+x)) e

// Merges two extents, by contionously picking the leftmost value of the first extent
// and the rightmost value of the second extent. Handles extents of varying depth
let rec merge (ps : Extent, qs : Extent) =
    match (ps,qs) with
    | ([],_) -> qs
    | (_,[]) -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge (ps,qs)

// Merges a list of extents
let mergelist (es : Extent list) = List.foldBack (fun e acc -> merge (e, acc)) es []

// Wrapper for max function to accept tuples as parameters
let fmax (p,q) = max p q

let rec fit (ps : Extent) (qs : Extent) =
    match (ps,qs) with
    | ((_,p)::ps , (q,_)::qs) -> fmax(fit ps qs, p - q + 1.2)
    | (_,_) -> 0.0

let fitlistl (es : Extent list) =
    let rec f acc = function
    | [] -> []
    | e::es -> let x = fit acc e
               x :: (f (merge (acc, moveextent (e,x))) es)
    f [] es

let fitlistr (es : Extent list) =
    let rec f acc = function
    | [] -> []
    | e::es -> let x = -(fit e acc)
               x :: (f (merge(moveextent (e,x), acc))) es
    f [] (List.rev es) |> List.rev

let mean (x,y) = (x+y)/2.0

let fitlist es = List.zip (fitlistl es) (fitlistr es) |> List.map mean

let design tree =
    let rec f = function
    | Node(label, subtrees) ->
        let (trees, extents) = List.map f subtrees |> List.unzip
        let positions = fitlist extents
        let ptrees = List.zip trees positions |> List.map movetree
        let pextents = List.zip extents positions |> List.map moveextent
        let resultextent = (0.0,0.0) :: (mergelist pextents)
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    f tree