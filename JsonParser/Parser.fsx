//module Parser
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\FParsecCS.dll"
#r "bin\Debug\FParsec.dll"
#load "Ast.fs"
#load "Parser.fs"
#load "PrettyPrint.fs"
#load "Rewriter.fs"
#load "TreeDesign.fs"
#load "TreeUtils.fs"
#load "TreeDraw.fs"
#load "TreeConverter.fs"

open FParsec
open Ast
open Parser
open PrettyPrint
open Rewriter
open TreeDesign
open TreeUtils
open TreeDraw
open TreeConverter

let writeStrToFile name str =
    System.IO.File.WriteAllText(name + ".ps", str)

let ex str = run Parser.PartialParsers.expr str |> Parser.Helpers.getResult
let st str = run Parser.PartialParsers.stmt str |> Parser.Helpers.getResult

let completeProgram = Parser.parse "completeTest.java"
let ifProgram = Parser.parse "ifTest.java"

let drawProgram file rewrite = 
    let p = Parser.parse file
    let p' = Rewriter.rewriteProgram p
    let nodes = cProgram (if rewrite then p' else p)
    let tree,_ = design nodes
    let str = TreeDraw.drawTreeSbFormat tree
    let filename =
        match rewrite with
        | true -> file + "-rewrite"
        | false -> file
    writeStrToFile filename str

let drawExpr expr = 
    let reNodes = cExpr (Rewriter.Helpers.re expr)
    let nodes = cExpr expr
    let compare = Node("Compare", [Node("NoRewrite", [nodes]); Node("Rewritten", [reNodes])])
    let tree,_ = design compare
    let str = TreeDraw.drawTreeSbFormat tree
    writeStrToFile "expr" str

drawProgram "ifTest.java" false

drawExpr (ex "A().b[2]()()(2).a[1](1,2)")


let doc = pprogram ifProgram
printf "%s" (printDocument doc)


let test = """
class main {
    int a;
    public void test() {
        a.b.c();
        a[2]();
        a[2].b.c();
    }
}
"""
let testProgram = Parser.parseStr test
let rewritten = Rewriter.rewriteProgram testProgram
printf "%s" (printDocument (pprogram rewritten))
printf "%s" (printDocument (pprogram testProgram))


ex "c.b.a" |> Rewriter.Helpers.re
ex "Obj.field"
ex "Obj.field.field"
ex "Obj.method()"
ex "a.Obj().method()" |> Rewriter.Helpers.re
ex "Obj.a[0]()"
ex "Obj.a[0]()" |> Rewriter.Helpers.re
ex "b.a[1]"
ex "b.a[1]" |> Rewriter.Helpers.re
ex "A.b[0].c.d" |> Rewriter.Helpers.re
ex "a[1][1]"
ex "a[1][1]" |> Rewriter.Helpers.re
ex "a[1]"
ex "a.b.c[1]"
ex "a.b[2]().d.e.c[3]" |> Rewriter.Helpers.re
ex "a()" |> Rewriter.Helpers.re
ex "b.a()" |> Rewriter.Helpers.re
ex "a.c(1).b(2)" |> Rewriter.Helpers.re
st "a.c(1).b(2);"
ex "a.c[2](1)[2]" |> Rewriter.Helpers.re
st "a.c[2](1)[2];"
run Parser.PartialParsers.stmt "c[2](1)[2];"
run Parser.PartialParsers.stmt "if(true) { b();  }"
run Parser.PartialParsers.stmt "a;"







// Tests
run Parser.PartialParsers.ident "a"
run Parser.PartialParsers.stringLiteral "\"hej\""
run Parser.PartialParsers.mvalue "2"
run Parser.PartialParsers.mType "int[][]"
run Parser.PartialParsers.mType "p"
run Parser.PartialParsers.procType "void "
run Parser.PartialParsers.mvariable "a[] a"


run Parser.expr "hej"
run Parser.expr "!false"
run Parser.expr "true ==   false"
run Parser.expr "2 * 2"
run Parser.expr "a.b.c()"
run Parser.expr "a.b()[2]"

run Parser.expr "new int[-1]"
run Parser.expr "a.b[2].c(1,2,3)"
run Parser.expr "a(1)[2].b(3)"
run Parser.expr "new int[q(new A(), new int[2])]"
run Parser.expr "a[b[1](2)][3](4)"
run Parser.expr "a()()"
run Parser.expr "Parser[2](1,2)"
run Parser.expr "b.a(1)[2]"
run Parser.expr "a.b.a[2](1)"
run Parser.expr "a[2][2]"
run Parser.expr "a[2]"
run Parser.expr "(new int[2])[1]()"
run Parser.expr "b().c().d()"
run Parser.stmt "int a;"
run Parser.stmt "a = 3;"
run (many Parser.stmt) "if(3 == 3) a = 3; a = 2;"
run (many Parser.stmt) "int a; if(a < 3) { a = 3; } else if ( a == 4) { a = 5; } else { a = 6; }"
run Parser.stmt "a = b();"
run Parser.stmt "a(); "
run Parser.stmt "a.b();"
run Parser.stmt "a.b.c();"
run (many1 Parser.stmt) "a.b.c.d();a();"
run Parser.stmt "b[3] = 3;"
run Parser.stmt "b()[2] = 2;"

"""public int[] anders(int a, T q, b[] a) {
Field.method();
}"""
|> run Parser.methodDecl

"""class Hej extends Arne { Q a; int[] a; int a(int q) { System[2].out[2]()[2].println("Flemming"); } }
"""
|> run Parser.classDecl
"""class hej extends nej {
     int a;
     boolean test(int a) { return (a < 3); }
     int[] as;
 }"""
|> run classDecl

let testProgram = """class testmain {
  int main(String[] args) {
    System.out.println("Hello World");
  }
}
class hej extends nej {
     int a;
     bool test(int a) { return a < 3; }
     int[] as;
 }
"""
run program testProgram

