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
#load "TypeChecker.fs"
#load "ParserUtils.fs"

open Ast
open Parser
open PrettyPrint
open Rewriter
open TreeDesign
open TreeUtils
open TreeDraw
open TreeConverter
open TypeChecker
open ParserUtils
