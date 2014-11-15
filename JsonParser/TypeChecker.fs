module TypeChecker

open Ast

let tryFindMethod (klass : Class) name (types : MType list) =
    klass.Methods
    |> List.tryFind (fun m -> name = m.MethodName && types = (m.Parameters |> List.map fst))

type ClassEnv = { Fields: Map<string, MType>; Methods: Map<string * MType list, MethodDecl> }
and ProgramEnv = Map<string, ClassEnv>
and Env = Map<string, MType>

let methodReturnType (pe : ProgramEnv) cn msig =
    let ce = Map.find cn pe
    let mdecl = Map.find msig ce.Methods
    mdecl.ProcType

let buildEnv (program:Program) : ProgramEnv =
    let tcClass (c:Class) =
        let vars =
            (Map.empty, c.Variables)
            ||> List.fold (fun map ((t,n) as v) ->
                            match Map.tryFind n map with
                            | Some _ -> failwithf "Field %s already declared in %s" n c.ClassName
                            | None -> Map.add n t map)
        let methods =
            let namesAndTypes = c.Methods |> List.map (fun m -> m.MethodName, m.Parameters |> List.map fst)
            (Map.empty, c.Methods, namesAndTypes)
            |||> List.fold2 (fun map m nt ->
                             match Map.tryFind nt map with 
                             | Some _ ->
                                failwithf "Method %s(%s) already declared" (fst nt) (snd nt |> List.map (sprintf "%A") |> String.concat ", ")
                             | None -> Map.add nt { m with Body = [] } map)
        { Fields = vars; Methods = methods }   
    let classes =
        (Map.empty, program)
        ||> List.fold (fun acc c ->
                        match Map.tryFind c.ClassName acc with
                        | Some _ -> failwithf "Class %s already defined" c.ClassName
                        | None -> Map.add c.ClassName (tcClass c) acc)
    classes

module TypeHelper =
    let (|IsInt|IsBool|IsString|IsClass|IsArray|) t =
        match t with
        | MType.Int -> IsInt
        | MType.Boolean -> IsBool
        | MType.String -> IsString
        | MType.Class(_) -> IsClass
        | MType.ArrayType(_) -> IsArray
    let isSameType t1 t2 =
        match t1,t2 with
        | IsInt, IsInt
        | IsBool, IsBool
        | IsString, IsString
        | IsClass, IsClass
        | IsArray, IsArray -> true
        | _,_ -> false
//    let isInt = function
//                | MType.Int -> true
//                | _ -> false
//    let isBool = function
//                 | MType.Boolean -> true
//                 | _ -> false

open TypeHelper

type IdentReturn = Field of MType | Method of MType * string
        
let rec tcProgram (program : Program) =
    let env = buildEnv program
    program |> List.map (fun c -> tcClass env c)
and tcClass (pe : ProgramEnv) (c : Class) =
    c.Methods
    |> List.map (fun m -> tcMethod pe c.ClassName m)
and tcMethod (pe : ProgramEnv) cn (m : MethodDecl) =
    let msig = m.MethodName, m.Parameters |> List.map fst
//    let fields = Map.find cn pe |> fun e -> e.Fields
    let args = m.Parameters |> List.map (fun (t, n) -> (n, t)) |> Map.ofList
//    let env = args |> Map.fold (fun acc k v -> Map.add k v acc) fields
//    let methodEnv : Env = Map.empty
    m.Body |> List.fold (fun acc e -> tcStmt pe cn msig acc e) args
and tcStmt (pe : ProgramEnv) cn msig (env : Env) (s : Stmt) =
    let self = tcStmt pe cn msig
    let exp = tcExpr pe cn msig env
    match s with
    | Stmt.Decl(t, n) ->
        match Map.tryFind n env with
        | Some _ -> failwithf "Variable with name %s already declared" n
        | None -> Map.add n t env
    | Stmt.Block(ss) -> List.fold (fun acc e -> self acc e) env ss
    | Stmt.Single(s) -> self env s
    | Stmt.Assign(e1, e2) ->
        let t1 = exp e1
        let t2 = exp e2
        if isSameType t1 t2
            then env
            else failwithf "LHS [%A] cannot be assigned value of RHS [%A]" t1 t2
    | Stmt.Return(x) ->
        let procType = methodReturnType pe cn msig
        match x, procType with
        | None, ProcType(t) -> failwith "Must return type %A" t
        | None, Void -> env
        | Some _, Void -> failwith "Method return type is void, yet something is returned"
        | Some e, ProcType(p) ->
            let eType = exp e
            if isSameType eType p then env
            else failwithf "Return type %A does not match method signature return type %A" eType p

    | Stmt.If(e, s) ->
        match exp e with
        | IsBool -> self env s
        | t -> failwithf "IF condition must resolve to boolean, not %A" t
    | Stmt.IfElse(e, s1, s2) ->
        match exp e with
        | IsBool ->
            let env' = self env s1
            self env' s2
        | t -> failwithf "If else condition must resolve to boolean, not %A" t
    | Stmt.While(e, s) ->
        match exp e with
        | IsBool -> self env s
        | t -> failwithf "While condition must resolve to boolean, not %A" t
    | Stmt.MethodCall(e) ->
        exp e |> ignore
        env
and tcValue = function
    | Value.Int i -> MType.Int
    | Value.Boolean b -> MType.Boolean
    | Value.String s -> MType.String
and tcIdentField (pe : ProgramEnv) cn msig (env : Env) = function
    | Ident s ->
        let ce = Map.find cn pe
        match Map.tryFind s env with
        | Some t -> t
        | None ->   match Map.tryFind s ce.Fields with
                    | Some t -> t
                    | None -> failwithf "Identfier %s not declared" s
    | Selector(e1,e2) ->
        match tcExpr pe cn msig env e1 with
        | MType.Class s ->  tcExpr pe s msig env e2
        | _ -> failwithf "LHS of selector always expected to be of class type (%A)" e1
    | Identifier.Array(arrayExp, indexExp) ->
        match tcExpr pe cn msig env indexExp with
        | IsInt ->  match tcExpr pe cn msig env arrayExp with
                    | MType.ArrayType(mtype) -> mtype
                    | _ -> failwith "Index into a non array type"
        | _ -> failwith "Index argument into array type must be an int type"
and tcNew (pe : ProgramEnv) cn msig env = function
    | New.Object t ->
        match t with
        | MType.Class s ->
            match Map.tryFind s pe with
            | None -> failwithf "Trying to instantiate %s failed. Class not found" s
            | Some _ -> MType.Class s
        | _ -> failwithf "Trying to instantiate something that isnt a class: %A" t
    | New.Array(t,e) ->
        match tcExpr pe cn msig env e with
        | IsInt -> MType.ArrayType(t)
        | et -> failwithf "new %A[] with %A argument" t et
and tcExpr (pe : ProgramEnv) cn msig (env : Env) (expr : Expr) : MType =
    let spark f = f pe cn msig env
    let self = spark tcExpr
    match expr with
    | Expr.Value v -> tcValue v
    | Expr.New n -> spark tcNew n
    | Expr.Identifier i -> spark tcIdentField i
    | Expr.UnaryOp(op, e) ->
        let t = self e
        match op,t with
        | "-", IsInt -> MType.Int
        | "!", IsBool -> MType.Boolean
        | _ -> failwithf "Operator '%s' cant be used with %A" op t
    | Expr.BinaryOp(e1, op, e2) ->
        let t1, t2 = (self e1, self e2)
        let fail() = failwithf "BinaryOp '%s' ineligible for use with types %A and %A" op t1 t2
        match isSameType t1 t2 with
        | false -> fail()
        | true -> match t1 with
                     | IsInt -> match List.exists ((=) op) ["*";"+";"-";"<";">";"<=";">="] with
                                | true -> MType.Int
                                | false -> fail()
                     | IsBool -> match List.exists ((=) op) ["==";"!=";"&&";"||"] with
                                 | true -> MType.Boolean
                                 | false -> fail()
                     | _ -> fail()
    | Expr.MethodCall(e, es) ->
        let types = es |> List.map self
        tcIdentMethod pe cn types e
        
        
    | _ -> failwith "Expr not implemented"

    
//and rec tcStmt (pe : ProgramEnv) c (stmt : Stmt) =
//    match stmt with
//    | Stmt.Assign