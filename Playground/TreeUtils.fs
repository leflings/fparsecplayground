module TreeUtils

open TreeDesign

// Gets the absolute position of the leftmost extent
let rec leftmost = function
    | Node((_, x), []) -> x
    | Node((_, x), ts) -> x + (List.fold (fun acc t -> min acc (leftmost t)) 0 ts)

// Gets the absolute position of the rightmost extent
let rec rightmost = function
    | Node((_, x), []) -> x
    | Node((_, x), ts) -> x + (List.fold (fun acc t -> max acc (rightmost t)) 0 ts)

// Calculates the depth of the tree
let rec depthOfTree = function
    | Node((_, x), []) -> 1
    | Node(_, ts) -> 1 + (List.fold (fun acc e -> max acc (depthOfTree e)) 0 ts)

// Scales the tree horizontally and convert floats to ints in the process
let rec scaleTree (s:float) = function
    | Node((l,x), []) -> Node((l, int (x * s)), [])
    | Node((l,x), ts) -> Node((l, int (x * s)), (List.map (scaleTree s) ts))

// Counts the amount of nodes in the design
let rec countNodes = function
    | Node(_, []) -> 1
    | Node(_, ts) -> 1 + List.fold (fun acc e -> acc + (countNodes e)) 0 ts

