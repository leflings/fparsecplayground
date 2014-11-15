module TreeConverter

//open AST
open Ast
open TreeDesign

let rec cProgram (p : Program) : Tree<string> =
    Node("Program", List.map cClass p)
and cClass (c : Class) : Tree<string> =
    let decl = 
        seq {
            yield Node(c.ClassName, [])
            match c.SuperClass with Some s -> yield Node(s,[]) | None -> ()
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
    | MType.Class(c) -> Node("Class: " + c, [])
    | MType.ArrayType(t) -> Node("Array: ", [cMType t])
    | MType.Void -> Node("Void", [])
and cMethod m : Tree<string> =
    Node("Method", seq {
        if m.Public then yield Node("public", [])
        if m.Static then yield Node("static", [])
        yield cProcType m.ProcType
        yield Node(m.MethodName, [])
        yield Node("Parameters", [for v in m.Parameters -> cVar v])
        yield Node("Statements", [for s in m.Body -> cStmt s])
        } |> List.ofSeq)
and cProcType pt : Tree<string> =
    match pt with
    | Void -> Node("void", [])
    | ProcType(t) -> cMType t
and cStmt s : Tree<string> =
    match s with
    | Stmt.Decl v -> cVar v
    | Stmt.Block(ss) -> Node("Block", [for s in ss -> cStmt s])
    | Stmt.Single(s) -> Node("Single", [cStmt s])
    | Stmt.Return(e) ->
        Node("return", seq { match e with Some x -> yield cExpr x | None -> ()} |> List.ofSeq)
    | Stmt.Assign(lhs, rhs) ->
        Node("assign", [Node("lhs", [cExpr lhs]); Node("rhs", [cExpr rhs])])
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
    | Expr.MethodCall(e, es) -> Node("Invoke", [Node("method", [cExpr e]); Node("args", [for e in es -> cExpr e])])
    | Expr.Identifier(i) -> cIdent i
    | Expr.New(n) -> cNew n
and cNew n : Tree<string> =
    match n with
    | New.Array(t, e) -> Node("new array", [cMType t; cExpr e])
    | New.Object(t) -> Node("new object", [cMType t])
and cIdent i : Tree<string> =
    match i with
    | Identifier.Ident s -> Node("ident", [Node(s, [])])
    | Identifier.Selector(obj, field) ->
        Node("Selector", [Node("obj", [cExpr obj]); Node("field", [cExpr field])])
    | Identifier.Array(a, i) ->
        Node("Array", [Node("ref", [cExpr a]); Node("index", [cExpr i])])
and cValue v : Tree<string> =
    match v with
    | Value.Boolean b -> Node(string b,[])
    | Value.Int i -> Node(string i, [])
    | Value.String s -> Node(sprintf "\"%s\"" s, [])

    
        


//let rec cExp (e:Exp) : Tree<string> = 
//    match e with
//    | Int(i) -> Node(string i, [])
//    | Bool(b) -> Node(string b, [])
//    | String(str) -> Node(sprintf "%s" str, [])
//    | Var(str) -> Node(sprintf "@%s" str, [])
//    | ContOf(e) -> Node("ContOf", [cExp e])
//    | Apply(e,es) -> Node("Apply", cExp e :: (List.map cExp es))
//    | ArrayElm(e1,e2) -> Node("ArrayElm", [cExp e1; cExp e2])
//    | ArrayFun(e,str) -> Node("ArrayFun", [cExp e; Node(str, [])])
//and cStm (s:Stm) : Tree<string> =
//    match s with
//    | Asg(e1,e2) -> Node("Asg", [cExp e1; cExp e2])
//    | PrintLn(e) -> Node("PrintLn", [cExp e])
//    | Seq(x) -> Node("Seq", (List.map cStm x))
//    | While(e,s) -> Node("While", [cExp e; cStm s])
//    | Block(ds, s) -> Node("Block", (List.map cDec ds) @ [cStm s])
//    | ProcCall(e) -> Node("ProcCall", [cExp e])
//    | IT(e, st) -> Node("IT", [cExp e; cStm st])
//    | ITE(e, st1, st2) -> Node("ITE", [cExp e; cStm st1; cStm st2])
//    | Return(e) -> Node("Return", [cExp e])
//    | Do(st) -> Node("Do", [cStm st])
//    | TC(str, st1 , st2) -> Node("TC", [Node(str, []); cStm st1; cStm st2])
//    | TF(st1, st2) -> Node("TF", [cStm st1; cStm st2])
//    | TCF(str, st1, st2, st3) -> Node("TCF", [Node(str, []); cStm st1; cStm st2; cStm st3])
//and cDec (d:Dec) : Tree<string> =
//    match d with
//    | VarDec(str, e) -> Node("VarDec", [Node(str, []); cExp e])
//    | ProcDec(str, ss, st) -> Node("ProcDec", Node(str,[]) :: (List.map (fun s -> Node(s,[])) ss) @ [cStm st])
//    | ArrayDec(str, e1, e2) -> Node("ArrayDec", [Node(str,[]); cExp e1; cExp e2])
