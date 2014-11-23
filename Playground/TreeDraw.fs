module TreeDraw

open TreeUtils
open TreeDesign

let common t =
    let hscale = 40.0
    let vscale = 10.0
    let tree = scaleTree hscale t
    let (leftmost,rightmost,treeDepth) = (leftmost tree, rightmost tree, depthOfTree tree)
    let (rowHeight, beforeText, afterText, stringHeight) = let (bT,aT,sH) = (1.5, 1.0, 1.0)
                                                           let rH = bT + aT + sH
                                                           (int (rH * vscale), int(bT * vscale), int(aT * vscale), int(sH * vscale))
    let cd = ( * ) (rowHeight * -1)
    let trimString (s:string) = if String.length s < 10 then s else s.Remove(9) 

    let beginning = let padding = 50
                    let width = abs(leftmost) + abs(rightmost) + padding
                    let height = treeDepth * rowHeight * 2
                    let (translateX, translateY) =
                        ((abs leftmost) + padding/2, height-1)
                    "%!" +
                    (sprintf
                        "\n<</PageSize[%d %d]/ImagingBBox null>> setpagedevice\n1 1 scale\n%d %d translate\nnewpath\n /Times-Roman findfont 10 scalefont setfont\n"
                        width height translateX translateY)
    (cd,rowHeight,beforeText,afterText,stringHeight,tree,beginning,trimString)
    

let drawTree t =
    let (cd,rowHeight,beforeText,afterText,stringHeight,tree,beginning,trimString) = common t
    let moveto a b = sprintf "%d %d moveto\n" a b
    let lineto a b = sprintf "%d %d lineto\n" a b
    let label a = sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (trimString a)

    // This is where the actual magic happens
    let rec f center depth tree = 
        match tree with
        | Node((str, pos), []) ->
            moveto (center) ((cd depth) - afterText)
            + label str
        | Node((str, pos), subtrees) ->
            let (mini,maxi) = let offsets = List.map (fun  (Node((_,p),_)) -> p) subtrees
                              (List.min offsets, List.max offsets)
            moveto (center) ((cd depth) - stringHeight)
                + label str
                + moveto (center) ((cd depth) - (afterText + stringHeight))
                + lineto (center) (cd (depth+1))
                + moveto (center + mini) (cd (depth+1))
                + lineto (center + maxi) (cd (depth+1))
                + (String.concat "" (List.map (fun (Node((_,o),_)) -> moveto (center + o) (cd (depth+1)) + lineto (center + o) (cd (depth+2)) ) subtrees) )
                + "\nstroke\n"
                + (List.fold (fun acc (Node((_,c),_) as t) -> acc + (f (center + c) (depth + 2) t)) "" subtrees)
    beginning + (f 0 0 tree) + "showpage"

let drawTreeSb t =
    let sb = new System.Text.StringBuilder()
    let drawer =
        let (cd,rowHeight,beforeText,afterText,stringHeight,tree,beginning,trimString) = common t
        let moveto a b = sb.Append(sprintf "%d %d moveto\n" a b) |> ignore
        let lineto a b = sb.Append(sprintf "%d %d lineto\n" a b) |> ignore
        let label a = sb.Append(sprintf " (%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (trimString a)) |> ignore

        // This is where the actual magic happens
        let rec f center depth tree = 
            match tree with
            | Node((str, pos), []) ->
                moveto (center) ((cd depth) - afterText)
                label str
            | Node((str, pos), subtrees) ->
                let (mini,maxi) = let offsets = List.map (fun  (Node((_,p),_)) -> p) subtrees
                                  (List.min offsets, List.max offsets)
                moveto (center) ((cd depth) - stringHeight)
                label str
                moveto (center) ((cd depth) - (afterText + stringHeight))
                lineto (center) (cd (depth+1))
                moveto (center + mini) (cd (depth+1))
                lineto (center + maxi) (cd (depth+1))
                List.iter (fun (Node((_,o),_)) -> moveto (center + o) (cd (depth+1)); (lineto (center + o) (cd (depth+2)))) subtrees
                sb.Append("\nstroke\n") |> ignore
                List.iter (fun (Node((_,c),_) as t) -> f (center + c) (depth + 2) t) subtrees
        sb.Append(beginning) |> ignore
        f 0 0 tree
        sb.Append("showpage") |> ignore
    sb.ToString();

let drawTreeSbFormat t =
    let sb = new System.Text.StringBuilder()
    let drawer =
        let (cd,rowHeight,beforeText,afterText,stringHeight,tree,beginning,trimString) = common t
        let moveto a b = sb.AppendFormat("{0} {1} moveto\n", [|a;b|]) |> ignore
        let lineto a b = sb.AppendFormat("{0} {1} lineto\n", [|a;b|]) |> ignore
        let label a = sb.AppendFormat(" ({0}) dup stringwidth pop 2 div neg 0 rmoveto show\n", (trimString a)) |> ignore

        // This is where the actual magic happens
        let rec f center depth tree = 
            match tree with
            | Node((str, pos), []) ->
                moveto (center) ((cd depth) - afterText)
                label str
            | Node((str, pos), subtrees) ->
                let (mini,maxi) = let offsets = List.map (fun  (Node((_,p),_)) -> p) subtrees
                                  (List.min offsets, List.max offsets)
                moveto (center) ((cd depth) - stringHeight)
                label str
                moveto (center) ((cd depth) - (afterText + stringHeight))
                lineto (center) (cd (depth+1))
                moveto (center + mini) (cd (depth+1))
                lineto (center + maxi) (cd (depth+1))
                List.iter (fun (Node((_,o),_)) -> moveto (center + o) (cd (depth+1)); (lineto (center + o) (cd (depth+2)))) subtrees
                sb.Append("\nstroke\n") |> ignore
                List.iter (fun (Node((_,c),_) as t) -> f (center + c) (depth + 2) t) subtrees
        sb.Append(beginning) |> ignore
        f 0 0 tree
        sb.Append("showpage") |> ignore
    sb.ToString();

