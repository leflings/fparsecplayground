module TypeChecker

open Ast

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
                            if n = "this" then failwith "'this' is not a legal variable name"
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
    let (|IsInt|IsBool|IsString|IsClass|IsArray|IsVoid|) t =
        match t with
        | MType.Int -> IsInt
        | MType.Boolean -> IsBool
        | MType.String -> IsString
        | MType.Class(_) -> IsClass
        | MType.ArrayType(_) -> IsArray
        | MType.Void -> IsVoid
    let isSameType t1 t2 =
        match t1,t2 with
        | IsInt, IsInt
        | IsBool, IsBool
        | IsString, IsString
        | IsClass, IsClass
        | IsVoid, IsVoid
        | IsArray, IsArray -> true
        | _,_ -> false

open TypeHelper

let rec tcProgram (program : Program) =
    let env = buildEnv program
    program |> List.map (fun c -> tcClass env c)
and tcClass (pe : ProgramEnv) (c : Class) =
    c.Methods
    |> List.map (fun m -> tcMethod pe c.ClassName m)
and tcMethod (pe : ProgramEnv) cn (m : MethodDecl) =
    let msig = m.MethodName, m.Parameters |> List.map fst
    let args = m.Parameters |> List.map (fun (t, n) -> (n, t)) |> Map.ofList
    let env = m.Body |> List.fold (fun acc e -> tcStmt pe cn msig acc e) args
    let returnsProper =
        if m.ProcType = MType.Void then true
        else m.Body |> List.rev |> List.tryPick (function | Stmt.Return(e) -> e | _ -> None)
             |> function
             | Some t -> tcExpr pe cn env t = m.ProcType
             | None -> false
    if returnsProper
    then env
    else failwithf "Method body must return something of type %A" m.ProcType
and tcStmt (pe : ProgramEnv) cn msig (env : Env) (s : Stmt) =
    let self = tcStmt pe cn msig
    let exp = tcExpr pe cn env
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
        | None, Void -> env
        | None, _ -> failwith "Must return type %A" procType
        | Some _, Void -> failwith "Method return type is void, yet something is returned"
        | Some e, _ ->
            let eType = exp e
            if isSameType eType procType then env
            else failwithf "Return type %A does not match method signature return type %A" eType procType

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
and tcIdentField (pe : ProgramEnv) (cn:string) (env : Env) (types : MType list option) i =
    match i with
    | Ident s ->
        let thisLookup, className = // HACK this how we represent this.X atm
            match cn.IndexOf(".") with
            | -1 -> false, cn
            | n -> true, cn.Substring(n+1)
        let ce = Map.find className pe
        if s = "this" then MType.Class className else
        match types with
        | None ->
            let fstLookup, sndLookup =
                match thisLookup with
                | true -> ce.Fields, Map.empty
                | false -> env, ce.Fields
            match Map.tryFind s fstLookup with
            | Some t -> t
            | None ->   match Map.tryFind s sndLookup with
                        | Some t -> t
                        | None -> failwithf "Identfier [%s] not declared" s
        | Some ts ->    match Map.tryFind (s,ts) ce.Methods with
                        | None -> failwithf "No method [%s(%A)] found on class %s" s ts className
                        | Some mdecl -> mdecl.ProcType
    | Selector(Identifier(Ident "this"), e2) -> tcExpr pe ("this."+cn) env e2
    | Selector(_,Identifier(Ident "this")) -> failwith "'this' keyword prohibited on rhs of .dot"
    | Selector(e1,e2) ->
        match tcExpr pe cn env e1 with
        | MType.Class s ->  tcExpr pe s env e2
        | _ -> failwithf "LHS of selector always expected to be of class type (%A)" e1
    | Identifier.Array(arrayExp, indexExp) ->
        match tcExpr pe cn env indexExp with
        | IsInt ->  match tcExpr pe cn env arrayExp with
                    | MType.ArrayType(mtype) -> mtype
                    | _ -> failwith "Index into a non array type"
        | _ -> failwith "Index argument into array type must be an int type"
and tcNew (pe : ProgramEnv) cn env = function
    | New.Object t ->
        match t with
        | MType.Class s ->
            match Map.tryFind s pe with
            | None -> failwithf "Trying to instantiate %s failed. Class not found" s
            | Some _ -> MType.Class s
        | _ -> failwithf "Trying to instantiate something that isnt a class: %A" t
    | New.Array(t,e) ->
        match tcExpr pe cn env e with
        | IsInt -> MType.ArrayType(t)
        | et -> failwithf "new %A[] with %A argument" t et
and tcExpr (pe : ProgramEnv) cn (env : Env) (expr : Expr) : MType =
    let spark f = f pe cn env
    let self = spark tcExpr
    match expr with
    | Expr.Value v -> tcValue v
    | Expr.New n -> spark tcNew n
    | Expr.Identifier i -> tcIdentField pe cn env None i
    | Expr.UnaryOp(op, e) ->
        let t = self e
        match op,t with
        | "-", IsInt -> MType.Int
        | "!", IsBool -> MType.Boolean
        | _ -> failwithf "Operator '%s' cant be used with %A" op t
    | Expr.BinaryOp(e1, op, e2) ->
        let t1, t2 = (self e1, self e2)
        let fail() = failwithf "BinaryOp '%s' ineligible for use with types %A and %A" op t1 t2
        let opIn ops = List.exists ((=) op) ops
        let intIntIntOps = ["*";"+";"-"]
        let intIntBoolOps = ["<";">";"<=";">="]
        let boolBoolBoolOps = ["==";"!=";"&&";"||"] 
        match isSameType t1 t2 with
        | false -> fail()
        | true -> match t1 with
                     | IsInt -> if opIn intIntIntOps then MType.Int
                                elif opIn intIntBoolOps then MType.Boolean
                                else fail()
                     | IsBool -> if opIn boolBoolBoolOps then MType.Boolean else fail()
                     | _ -> fail()
    | Expr.MethodCall(e, es) ->
        let types = es |> List.map self
        match e with
        | Expr.Identifier i -> tcIdentField pe cn env (Some types) i
        | _ -> failwith "method invoked on something that is not an identifier"