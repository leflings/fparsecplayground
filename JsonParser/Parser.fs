module Parser

open FParsec
open Ast

module PartialParsers =
    let ws = spaces
    let ws1 = spaces1
    let str = pstring
    let str_ws s = pstring s .>> ws
    let str_ws1 s = pstring s .>> ws1
    let betweenStrings a b p = between (str a) (str b) p

    let opts =
        let isAsciiIdStart c =
            isAsciiLetter c || c = '_'

        let isAsciiIdContinue c =
            isAsciiLetter c || isDigit c || c = '_'

        IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                          isAsciiIdContinue = isAsciiIdContinue)

    let reserved = set ["new";"if";"while";"else";"return"]
    let ident =
        let a = identifier opts
        a >>= fun s -> if reserved |> Set.contains s then fail "keyword" else preturn s
    let ident_ws = ident .>> ws


    let stringLiteral =
        let escape =
            anyOf "\"\\/bfnrt"
            |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c
        let unicodeEscape =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)

    let mbool =
        let mtrue = stringReturn "true" true
        let mfalse = stringReturn "false" false
        (mtrue <|> mfalse) |>> Boolean

    let mint = pint32 |>> Int
    let mstring = stringLiteral |>> String
    let mvalue = choice [ mbool ; mint ; mstring ]

    let mTint = stringReturn "int" MType.Int
    let mTbool = stringReturn "boolean" MType.Boolean
    let mTstring = stringReturn "String" MType.String
    let mTclass = (notFollowedBy (choice [mTint;mTbool;mTstring])) >>. ident |>> MType.Class
    let mTypeChoice = choice[mTint; mTbool; mTstring; mTclass]
    let mType = pipe2
                    mTypeChoice
                    (many(pstring "[]"))
                    (List.fold (fun x _ -> MType.ArrayType x))

    let procType = ((stringReturn "void" MType.Void) <|> mType) .>> str_ws " "

    let mvariable : Parser<Variable, unit> = (mType .>> ws1) .>>. ident

    // Expressions
    let expr, exprRef = createParserForwardedToRef<Expr, unit>()

    let opp = OperatorPrecedenceParser<Expr,unit,unit>()
    let infixOps = ["+";"-";"*";"&&";"==";".";"<";">"]
    let infixOperators =
        [
            100, ["*"]
            90, ["+";"-"]
            80, ["<";">";"<=";">="]
            70, ["=="; "!="]
            60, ["&&"]
            50, ["||"]
        ]
        
    let prefixOps = ["-";"!"]

//    for op in infixOps do
//        opp.AddOperator(InfixOperator(op,ws,1,Associativity.Left, fun x y -> Expr.BinaryOp(x, op, y)))
    for (prec, ops) in infixOperators do
        ops |> List.iter (fun op -> opp.AddOperator(InfixOperator(op, ws, prec, Associativity.Left, fun x y -> Expr.BinaryOp(x, op, y))))
    do
        opp.AddOperator(InfixOperator(".", ws, 5, Associativity.Left, fun x y -> Selector(x,y) |> Expr.Identifier))
    for op in prefixOps do
        opp.AddOperator(PrefixOperator(op, ws, 1, true, fun x -> Expr.UnaryOp(op, x)))

    // New object or array
    let exprNew =
        let obj = attempt (str_ws1 "new" >>. mTclass .>> str_ws "()") |>> New.Object
        let arr =
            pipe2
                (str_ws1 "new" >>. mType)
                (between (str_ws "[") (str_ws "]") expr)
                (fun x y -> New.Array(x,y))
        choice (List.map attempt [obj; arr])
        |>> Expr.New

    let exprValue = mvalue |>> Expr.Value
    let exprIdentifier =
        ident |>> (Ident >> Expr.Identifier)
    let exprMethodCall =
        pipe2
            (exprIdentifier .>> ws)
            (many1(between (str_ws "(") (str_ws ")") (sepBy expr (str_ws ","))))
            (List.fold (fun acc e -> Expr.MethodCall(acc, e)))

    let pvalue = (exprValue <|> attempt exprNew <|> attempt exprIdentifier)

    type IdentTrail = Index of Expr | Invocation of Expr list
    let term =
        let indexing = betweenStrings "[" "]" expr |>> IdentTrail.Index
        let invoking = betweenStrings "(" ")" (sepBy expr (str_ws ",")) |>> IdentTrail.Invocation
        pipe2
            (pvalue .>> ws <|> between (str_ws "(") (str_ws")") expr)
            (opt(many1(
                    (attempt indexing)
                    <|>
                    (attempt invoking)
                )))
            (fun a b ->
                match b with
                | None -> a
                | Some bs ->
                    (a,bs) ||> List.fold (fun acc e -> match e with
                                                       | Index i -> Expr.Identifier(Identifier.Array(acc, i))
                                                       | Invocation args -> Expr.MethodCall(acc, args)
                                                       )
                )

    do
        exprRef := opp.ExpressionParser
        opp.TermParser <- term

    let exprInParen = between (str_ws "(") (str_ws ")") expr

    // ====================
    // STATEMENTS
    // ====================

    let stmt, stmtRef = createParserForwardedToRef<Stmt, unit>()

    type BStmt
        = Single of Stmt
        | Many of Stmt list

    let stmtBlock =
        let singleStmt = stmt |>> BStmt.Single
        let manyStmt = between (str_ws "{") (str_ws "}") (many stmt) |>> BStmt.Many
        singleStmt <|> manyStmt |>> function
                                    | Single s -> Stmt.Single s
                                    | Many ss -> Stmt.Block ss

    let stmtVardecl = mvariable .>> str_ws ";" |>> Stmt.Decl
    let stmtAssign = (expr .>> ws .>> str_ws "=") .>>. expr .>> str_ws ";" |>> Stmt.Assign
    let stmtReturn = str_ws "return" >>. opt expr .>> str_ws ";" |>> Stmt.Return
    let stmtIf = (str_ws "if" >>. exprInParen) .>>.  (stmtBlock) |>> Stmt.If
    let stmtIfElse =
        tuple3 (str_ws "if" >>. exprInParen) (stmtBlock) (str_ws "else" >>. stmtBlock)
        |>> Stmt.IfElse
    let stmtWhile = (str_ws "while" >>. exprInParen) .>>. stmtBlock |>> Stmt.While
    let stmtMethodCall =
//        expr .>> str_ws ";" |>> Stmt.MethodCall
        expr .>> str_ws ";" >>= fun e -> 
                                match e with
                                | MethodCall(_,_) -> preturn (Stmt.MethodCall(e))
                                | Identifier(Selector(_,MethodCall(_,_))) -> preturn (Stmt.MethodCall(e))
                                | _ -> fail "not method call"

    do stmtRef :=
        attempt stmtReturn <|>
        attempt stmtVardecl <|>
        attempt stmtAssign <|> 
        attempt stmtMethodCall <|>
        attempt stmtIfElse <|> attempt stmtIf <|>
        attempt stmtWhile

    let methodDecl : Parser<MethodDecl,unit> =
        let signature =
            tuple4
                (opt (str_ws "public"))
                (opt (str_ws "static"))
                (procType)
                (ident .>> ws)
        let args = between (str_ws "(") (str_ws ")") (sepBy mvariable (str_ws ","))
        let body = between (str_ws "{") (str_ws "}") (many stmt)
        pipe3 signature args body
            (fun (isPublic, isStatic, proc, name) args body ->
                { MethodName = name;
                    Public = (Option.isSome isPublic);
                    Static = (Option.isSome isStatic);
                    ProcType = proc;
                    Parameters = args;
                    Body = body; } )


    type ClassBodyItem = Field of Variable | Method of MethodDecl
    let classDecl =
        let className = str_ws "class" >>. ident_ws
        let superClass = str_ws "extends" >>. ident_ws
        let field = mvariable .>> str_ws ";" |>> Field
        let methodDeclaration = methodDecl |>> Method
        pipe3
            className
            (opt superClass)
            (between (str_ws "{") (str_ws "}") (many (attempt methodDeclaration <|> field)))
            (fun className superClass bodyItems -> 
                let fields =
                    List.fold (fun acc ->
                                    function
                                    | Field v -> v :: acc
                                    | Method _ -> acc) [] bodyItems
                    |> List.rev
                let methods =
                    List.fold (fun acc ->
                                    function
                                    | Field _ -> acc
                                    | Method m -> m :: acc) [] bodyItems
                    |> List.rev
                { ClassName = className;
                    SuperClass = superClass;
                    Variables = fields;
                    Methods = methods; }
                )

    let program : Parser<Program,unit> = ws >>. (many (classDecl .>> ws)) .>> eof
let program = PartialParsers.program
let classDecl = PartialParsers.classDecl
let methodDecl = PartialParsers.methodDecl
let stmt = PartialParsers.stmt
let expr = PartialParsers.expr

module Helpers =
    let getResult = function
        | Success(result,_,_) -> result
        | Failure(s,_,_) -> failwithf "Error: %s" s
    
let parseStr str =
    runParserOnString program () "" str
    |> Helpers.getResult

let parse file = 
    runParserOnFile program () file (System.Text.Encoding.UTF8)
    |> Helpers.getResult