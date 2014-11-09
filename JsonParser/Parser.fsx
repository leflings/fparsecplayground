﻿//module Parser
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\FParsecCS.dll"
#r "bin\Debug\FParsec.dll"
#load "Ast.fs"

open FParsec
open Ast

let ws = spaces
let ws1 = spaces1
let str = pstring
let str_ws s = pstring s .>> ws
let str_ws1 s = pstring s .>> ws1

let sc = str ";"

run sc ";"

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

run ident "a"

let betweenStrings a b p = between (str a) (str b) p

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
run stringLiteral "\"hej\""

let mbool =
    let mtrue = stringReturn "true" true
    let mfalse = stringReturn "false" false
    (mtrue <|> mfalse) |>> Boolean

let mint = pint32 |>> Int
let mstring = stringLiteral |>> String
let mvalue = choice [ mbool ; mint ; mstring ]

run mvalue "2"

let mTint = stringReturn "int" MType.Int
let mTbool = stringReturn "boolean" MType.Boolean
let mTstring = stringReturn "String" MType.String
let mTclass = (notFollowedBy (choice [mTint;mTbool;mTstring])) >>. ident |>> MType.Class
let mTypeChoice = choice[mTint; mTbool; mTstring; mTclass]
let mType = pipe2
                mTypeChoice
                (many(str "[]"))
                (List.fold (fun x _ -> MType.ArrayType x))
run mType "int[][]"
run mType "p"

let procType = ((stringReturn "void" Void) <|> (mType |>> ProcType)) .>> many1 (str " ")

run procType "int"
run procType "int "
run (procType .>>. ident) "int[] a"

let mvariable : Parser<Variable, unit> = (mType .>> ws1) .>>. ident

run mvariable "a[] a"

//let vardecl = mvariable .>>? sc |>> Stmt.Decl

//run ((vardecl .>> ws) .>>. mvariable) "int[] a; int a"
//run ((mvariable) .>>. (ws >>. vardecl)) "int ai i p;"


// Expressions
let expr, exprRef = createParserForwardedToRef<Expr, unit>()

let opp = OperatorPrecedenceParser<Expr,unit,unit>()
let infixOps = ["+";"-";"*";"&&";"==";".";"<";">"]
let prefixOps = ["-";"!"]

for op in infixOps do
    opp.AddOperator(InfixOperator(op,ws,1,Associativity.Left, fun x y -> Expr.BinaryOp(x, op, y)))
for op in prefixOps do
    opp.AddOperator(PrefixOperator(op, ws, 1, true, fun x -> Expr.UnaryOp(op, x)))

// New object or array
let exprNew =
    let obj = attempt (str_ws1 "new" >>. mTclass .>> str_ws "()") |>> New.Object
    let arr =
        pipe2
            (str_ws1 "new" >>. mType)
            (between (str "[") (str "]") expr)
            (fun x y -> New.Array(x,y))
    choice (List.map attempt [obj; arr])
    |>> Expr.New

let exprValue = mvalue |>> Expr.Value
let exprIdentifier =
    ident |>> (Ident >> Expr.Identfier)
let exprMethodCall =
    pipe2
        (exprIdentifier .>> ws)
        (many1(between (str_ws "(") (str_ws ")") (sepBy expr (str_ws ","))))
        (List.fold (fun acc e -> Expr.MethodCall(acc, e)))

type IdentTrail = Index of Expr | Invocation of Expr list

let pvalue = (exprValue <|> attempt exprNew <|> attempt exprIdentifier)

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
                                                   | Index i -> Expr.Identfier(Identifier.Array(acc, i))
                                                   | Invocation args -> Expr.MethodCall(acc, args)
                                                   )
            )

do
    exprRef := opp.ExpressionParser
    opp.TermParser <- term

run expr "hej"
run expr "!false"
run expr "true ==   false"
run expr "2 * 2"
run expr "new int[-1]"
run expr "a.b[2].c(1,2,3)"
run expr "a(1)[2].b(3)"
run expr "new int[q(new A(), new int[2])]"
run expr "a[b[1](2)][3](4)"
run expr "a()()"
run expr "Parser[2](1,2)"
run expr "a(1)[2]"
run expr "a[2](1)"
run expr "a[2][2]"
run expr "(new int[2])[1]()"

let exprInParen = between (str_ws "(") (str_ws ")") expr


// ====================
// STATEMENTS
// ====================

let stmt, stmtRef = createParserForwardedToRef<Stmt, unit>()

let stmtBlock =
    let singleStmt = stmt |>> fun a -> [a]
    (singleStmt <|> between (str_ws "{") (str_ws "}") (many stmt))
        |>> Stmt.Block

let stmtVardecl = mvariable .>> str_ws ";" |>> Stmt.Decl
let stmtAssign = (expr .>> str_ws "=") .>>. expr .>> str_ws ";" |>> Stmt.Assign
let stmtReturn = str_ws "return" >>. opt expr .>> str_ws ";" |>> Stmt.Return
let stmtIf = (str_ws "if" >>. exprInParen) .>>.  (stmtBlock) |>> Stmt.If
let stmtIfElse =
    tuple3 (str_ws "if" >>. exprInParen) (stmtBlock) (str_ws "else" >>. stmtBlock)
    |>> Stmt.IfElse
let stmtWhile = (str_ws "while" >>. exprInParen) .>>. stmtBlock |>> Stmt.While
let stmtMethodCall =
    expr .>> str_ws ";" >>= function
                            | MethodCall(e,es) -> preturn (Stmt.MethodCall(e,es))
                            | _ -> fail "not method call"

do stmtRef :=
    attempt stmtReturn <|>
    attempt stmtVardecl <|>
    attempt stmtAssign <|> 
    attempt stmtMethodCall <|>
    attempt stmtIfElse <|> attempt stmtIf <|>
    attempt stmtWhile

run expr "3"

run stmt "int a;"
run stmt "a = 3;"
run (many stmt) "int a; if(a < 3) { a = 3; } else if ( a == 4) { a = 5; } else { a = 6; }"

let methodDecl =
    pipe2
        (tuple4
            (opt(str "public" .>> ws))
            (opt(str "static" .>> ws))
            (procType .>> ws)
            (ident .>> ws))
        (tuple2
            (between (str "(") (str ")") (sepBy mvariable (ws >>. str "," .>> ws)))
            (ws >>. sBlock))
        (fun (isPublic,isStatic,proc,name) (varList,body) ->
            { MethodName = name;
                Public = (Option.isSome isPublic);
                Static = (Option.isSome isStatic);
                ProcType = proc;
                Parameters = varList;
                Body = body })
"""public static void anders() {
int a;
a = 3;
return;
}"""
|> run methodDecl

//let varMethodChoice =
//    

let classDecl =
    pipe4
        (str "class" >>. ws >>. ident .>> ws)
        (opt( str "extends" >>. ws >>. ident .>> ws) .>> (str "{" .>> ws))
        (many (ws >>. mvariable .>>? sc))
//        (sepEndBy (ws >>. mvariable .>> ws) sc)
        ((many (ws >>. methodDecl)) .>> str "}")
        (fun name superClass variables methods ->
            { ClassName = name;
                SuperClass = superClass;
                Variables = variables;
                Methods = methods; })
"""class Hej extends Arne { int a; int a() { } }
"""
|> run classDecl

let program : Parser<Program,unit> = ws >>. (many (classDecl .>> ws)) .>> eof

let testProgram = """class testmain {
  int main(String[] args) {
    System.out.println("Hello World");
  }
}"""

run program testProgram
