//module Parser
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
#r "bin\Debug\FParsecCS.dll"
#r "bin\Debug\FParsec.dll"
#load "Ast.fs"
#load "Parser.fs"
#load "PrettyPrint.fs"

open FParsec
open Ast
open Parser
open PrettyPrint

let prog =
    runParserOnFile program () "completeTest.java" (System.Text.Encoding.UTF8)
    |> function
    | Success(result,_,_) -> result
    | Failure(_,_,_) -> []

#load "PrettyPrint.fs"
open PrettyPrint
let doc = pprogram prog
printf "%s" (printDocument doc)









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
run Parser.expr "new int[-1]"
run Parser.expr "a.b[2].c(1,2,3)"
run Parser.expr "a(1)[2].b(3)"
run Parser.expr "new int[q(new A(), new int[2])]"
run Parser.expr "a[b[1](2)][3](4)"
run Parser.expr "a()()"
run Parser.expr "Parser[2](1,2)"
run Parser.expr "a(1)[2]"
run Parser.expr "a[2](1)"
run Parser.expr "a[2][2]"
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

