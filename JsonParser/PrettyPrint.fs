module PrettyPrint

open Ast
open System.Text

type PrettyPrinter() =
    let sb = new StringBuilder()
    let mutable indent = 0
    member x.Print(str : string) = sb.Append(str)
    member x.Printn(str : string) = sb.AppendLine(str).Append(String.replicate (indent*2) " ")
    member x.Indent() = indent <- indent + 1
    member x.Undent() = if indent > 0 then indent <- indent - 1
    override x.ToString() = sb.ToString()

let pp = PrettyPrinter()

//type Token = Token of string
//and Line = Line of Token list
//and Block = Block of Line list
//and Section = Section of int * Block list

type Section
    = Line of string
    | EmptyLine
    | Section of Section list
    | Indent of Section
and Document = Document of Section list

let spaces i = String.replicate (i*2) " "

let rec printDocument  (Document s) =
    let sb = StringBuilder()
    (sb,s) ||> List.fold (fun acc e -> acc.AppendLine(printSection 0 e))
    |> fun e -> e.ToString()
and printSection i = function
    | Line str -> spaces i + str + "\n"
    | EmptyLine -> "\n"
    | Section  xs ->
        xs |> List.map (fun e -> (printSection (i) e)) |> String.concat ""
    | Indent b -> b |> printSection (i+1)

//        xs |> List.map (fun e -> (printSection (i+1) e)) |> String.concat ""



let rec pprogram (program : Program) =
    program
    |> List.map pclass
    |> Document
and pclass (cl : Class) =
    let openLine =
        [
            (sprintf "class %s" cl.ClassName)
            (match cl.SuperClass with Some sc -> "extends " + sc | None -> "")
            "{"
        ] |> String.concat " " |> Line
    let closeLine = "}" |> Line
    let vars =
        cl.Variables
        |> List.map (fun e -> (pvariable e) + ";" |> Line)
        |> Section
        |> Indent

    let methods =
        cl.Methods
        |> List.map pmethod
        |> Section
        |> Indent
    Section [openLine; vars; methods; closeLine]

// Method Declaration
and pmethod (decl : MethodDecl) =
    let decLine =
        seq {
            if decl.Public then yield "public"
            if decl.Static then yield "static"
            yield pproctype decl.ProcType
            yield (sprintf "%s(%s) {"
                    decl.MethodName
                    (String.concat ", " (List.map pvariable decl.Parameters)))
        } |> String.concat " " |> Line
    let body = List.map pstmt decl.Body |> Section |> Indent
    let closeLine = Line "}"
    Section [decLine; body; closeLine;  EmptyLine]

and pexpr = function
    | BinaryOp(e1, op, e2) -> String.concat " " [pexpr e1; op; pexpr e2]
    | UnaryOp(op, e) -> op + (pexpr e)
    | MethodCall(e, es) -> sprintf "%s(%s)" (pexpr e) (es |> List.map pexpr |> String.concat ", ")
    | Identifier i -> pidentifier i
    | New n -> pnew n
    | Value v -> pvalue v

and pnew = function
    | Object mt -> sprintf "new %s()" (pmtype mt)
    | New.Array(mt, e) -> sprintf "new %s[%s]" (pmtype mt) (pexpr e)

and pvalue = function
    | Int i -> string i
    | Boolean b -> string b
    | String s -> s

and pidentifier = function
    | Ident str -> str
    | Selector(e1,e2) -> sprintf "%s.%s" (pexpr e1) (pexpr e2)
    | Array(e1,e2) -> sprintf "%s[%s]" (pexpr e1) (pexpr e2)

//Stmt
and pstmt (s : Stmt) =
    let wrap s = pstmt s
//        match pstmt s with
//        | Block _ as b -> b
//        | n -> n

    match s with
    | Stmt.Block ss ->
        let ss' = match ss with 
                  | [] -> EmptyLine
                  | s :: [] -> pstmt s
                  | _ -> List.map pstmt ss |> Section |> Indent

        Section [Line "{"; ss'; Line "}"]
    | Stmt.Assign (e1,e2) -> Line(sprintf "%s = %s" (pexpr e1) (pexpr e2))
    | Stmt.Decl var -> Line(pvariable var + ";")
    | Stmt.Return e ->
        match e with
        | None -> Line("return;")
        | Some e -> Line("return " + pexpr e + ";")
    | Stmt.If(e, s) ->
        let line = sprintf "if(%s)" (pexpr e) |> Line
        Section [line; wrap s]

    | Stmt.IfElse(e, s1, s2) ->
        let line = sprintf "if(%s)" (pexpr e) |> Line
        let el = Line("else")
        Section [line; wrap s1; el; wrap s2]
    | Stmt.While(e, s) ->
        let line = sprintf "while(%s)" (pexpr e) |> Line
        Section [line; wrap s]
    | Stmt.MethodCall(e) -> Line(pexpr e + ";")
    
// Proc type
and pproctype = function
    | Void -> "void"
    | ProcType t -> pmtype t
// Variable
and pvariable ((t,n) : Variable) =
    sprintf "%s %s" (pmtype t) n
// MType
and pmtype (mt : MType) =
    match mt with
    | MType.Int -> "int"
    | MType.Boolean -> "boolean"
    | MType.String -> "String"
    | MType.Class(s) -> s
    | MType.ArrayType(at) -> sprintf "%s[]" (pmtype at)


    
//    pp {
//        yield (sprintf "class %s" (cl.ClassName))
//    }
    