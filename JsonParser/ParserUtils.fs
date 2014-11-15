module ParserUtils

open Ast
open FParsec
open Parser
open TreeConverter
open TreeDesign
open PrettyPrint

let ex str = run Parser.PartialParsers.expr str |> Parser.Helpers.getResult
let st str = run Parser.PartialParsers.stmt str |> Parser.Helpers.getResult

module TypeCheck =
    let check p = TypeChecker.tcProgram p
    let checkFile file = Parser.parse file |> check

module Print =
    let program p = 
        let doc = pprogram p
        printf "%s" (printDocument doc)
    

module Draw =
    let writeStrToFile name str =
        System.IO.File.WriteAllText(name + ".ps", str)

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

    let drawExpr str = 
        let expr = ex str
        let reNodes = cExpr (Rewriter.Helpers.re expr)
        let nodes = cExpr expr
        let compare = Node("Compare", [Node("NoRewrite", [nodes]); Node("Rewritten", [reNodes])])
        let tree,_ = design compare
        let str = TreeDraw.drawTreeSbFormat tree
        writeStrToFile "expr" str

