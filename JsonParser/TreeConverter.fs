module TreeConverter

//open AST
open Ast
open TreeDesign

let rec cProgram (p : Program) : Tree<string> =
    Node("Program", List.map cClass p)
and cClass (c : Class) : Tree<string> =
    let decl = 
        seq {
            yield Node("Name:" , [Node(c.ClassName, [])])
            match c.SuperClass with Some s -> yield Node("extends", [Node(s,[])]) | None -> ()
          } |> List.ofSeq
    let vars = Node("Fields", [for v in c.Variables -> cVar v])
    let methods = Node("Methods", [for m in c.Methods -> cMethod m])
    Node("Class",  List.concat [decl; [vars]; [methods]]) 
and cVar (t, n) : Tree<string> =
    Node("var", [cMType t; Node(n,[])])
and cMType t : Tree<string> = 
    match t with
    | MType.Int -> Node("int",[])
    | MType.Boolean -> Node("boolean",[])
    | MType.String -> Node("String",[])
    | MType.Class(c) -> Node("Class", [Node(c,[])])
    | MType.ArrayType(t) -> Node("Array", [cMType t])
    | MType.Void -> Node("Void", [])
and cMethod m : Tree<string> =
    Node("Method", seq {
        yield Node("Signature", seq {
        if m.Public then yield Node("public", [])
        if m.Static then yield Node("static", [])
        yield cMType m.ProcType
        yield Node(m.MethodName, [])
        yield Node("params", [for v in m.Parameters -> cVar v])
        } |> List.ofSeq)
        yield Node("body", [for s in m.Body -> cStmt s])
        } |> List.ofSeq)
and cStmt s : Tree<string> =
    match s with
    | Stmt.Decl v -> cVar v
    | Stmt.Block(ss) -> Node("Block", [for s in ss -> cStmt s])
    | Stmt.Single(s) -> cStmt s
    | Stmt.Return(e) ->
        Node("return", seq { match e with Some x -> yield cExpr x | None -> ()} |> List.ofSeq)
    | Stmt.Assign(lhs, rhs) ->
        Node("assign", [cExpr lhs; cExpr rhs])
    | Stmt.If(e, s) ->
        Node("if", [cExpr e; cStmt s])
    | Stmt.IfElse(e, s1, s2) ->
        Node("if else", [cExpr e; cStmt s1; cStmt s2])
    | Stmt.While(e, s) ->
        Node("while", [cExpr e; cStmt s])
    | Stmt.MethodCall(e) -> cExpr e
and cExpr e : Tree<string> =
    match e with
    | Expr.Value v -> cValue v
    | Expr.BinaryOp(e1, op, e2) -> Node(op, [cExpr e1; cExpr e2])
    | Expr.UnaryOp(op, e) -> Node(op, [cExpr e])
    | Expr.MethodCall(e, es) -> Node("Invoke", [cExpr e; Node("args", [for e in es -> cExpr e])])
    | Expr.Identifier(i) -> cIdent i
    | Expr.New(n) -> cNew n
and cNew n : Tree<string> =
    match n with
    | New.Array(t, e) -> Node("new array", [cMType t; cExpr e])
    | New.Object(t) -> Node("new object", [cMType t])
and cIdent i : Tree<string> =
    match i with
    | Identifier.Ident s -> Node(sprintf "@%s" s, [])
    | Identifier.Selector(obj, field) ->
        Node("Selector", [cExpr obj; cExpr field])
    | Identifier.Array(a, i) ->
        Node("Array", [cExpr a; cExpr i])
and cValue v : Tree<string> =
    match v with
    | Value.Boolean b -> Node(string b,[])
    | Value.Int i -> Node(string i, [])
    | Value.String s -> Node(sprintf "\"%s\"" s, [])
