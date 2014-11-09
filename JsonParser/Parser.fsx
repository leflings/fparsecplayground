//module Parser
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

let vardecl = mvariable .>>? sc |>> Stmt.Decl

run ((vardecl .>> ws) .>>. mvariable) "int[] a; int a"
run ((mvariable) .>>. (ws >>. vardecl)) "int ai i p;"

let expr, exprRef = createParserForwardedToRef<Expr, unit>()




// Expressions

let opp = OperatorPrecedenceParser<Expr,unit,unit>()
let inops = ["+";"-";"*";"&&";"==";"."]
let preOps = ["-";"!"]

for op in inops do
    opp.AddOperator(InfixOperator(op,ws,1,Associativity.Left, fun x y -> Expr.BinaryOp(x, op, y)))
for op in preOps do
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
    pipe2 ident (many (betweenStrings "[" "]" expr))
        (fun x y -> List.fold (fun (acc) e -> Expr.Identfier(Identifier.Array(acc, e)) ) (Expr.Identfier(Ident(x))) y)
let exprMethodCall =
    pipe2
        (exprIdentifier .>> ws)
        (many1(between (str_ws "(") (str_ws ")") (sepBy expr (str_ws ","))))
        (List.fold (fun acc e -> Expr.MethodCall(acc, e)))

let pvalue = exprValue <|> attempt exprMethodCall <|> attempt exprNew <|> attempt exprIdentifier
let term = pvalue .>> ws <|> between (str_ws "(") (str_ws")") expr

do
    exprRef := opp.ExpressionParser
    opp.TermParser <- term

run expr "hej"
run expr "!false"
run expr "true ==   false"
run expr "2 * 2"
run expr "new int[-1]"
run expr "a.b[2].c(1,2,3)"
run expr "a(2)[2].b(2)"
run expr "new int[q(new A(), new int[2])]"
run expr "a[c[2]()][3]()"
run expr "a()()"
run expr "Parser[2](1,2)"

let stmt, stmtRef = createParserForwardedToRef<Stmt, unit>()

//let block : Parser<Block, unit> = // str "{" >>. ws >>. many stmt .>> ws .>> str "}"
//    between (str "{") (str "}") (ws >>. many stmt .>> ws)

let sBlock = (str "{") >>. (many stmt) .>> (str "}") |>> Stmt.Block
let sVardecl = vardecl
let sIf =
    pipe2
        (between (str "if(" >>. ws) (ws .>> str ")") expr)
        stmt
        (fun x y -> Stmt.If(x, y))
let sIfElse =
    pipe2
        sIf 
        (str "else" >>. stmt)
        (fun x y -> match x with | If(e,s) -> Stmt.IfElse(e,s,y) | _ -> failwith "impossible")
let sWhile =
    pipe2
        (str "while(" >>. ws >>. expr .>> ws .>> str ")")
        stmt
        (fun x y -> Stmt.While(x,y))
let sAssign =
    pipe2
        (mIdentifier .>> ws .>> str "=")
        (ws >>. expr .>> ws .>> sc)
        (fun x y -> Stmt.Assign(x, y))
//    pipe2
//        expr
//        (ws >>. str "=" >>. ws >>. expr .>> ws .>> sc)
//        (fun x y -> Stmt.Assign(x, y))
let sMethodCall =
    exprMethodCall .>> ws .>> sc
        |>>
        function
        | MethodCall(e, es) -> Stmt.MethodCall(e, es)
        | _ -> failwith "nuh uh"
let sReturn =
    str "return" >>. opt expr .>> sc |>> Stmt.Return

do stmtRef :=
    ws >>. (
        [
            sBlock
            sVardecl
            sIf
            sIfElse
            sWhile
            sAssign
            sMethodCall
            sReturn
        ]
        |> List.map attempt
        |> choice
        ) .>> ws

run expr "3"

run stmt "int a;"
run stmt "a = 3;"
run stmt "{ }"

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
