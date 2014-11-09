module Ast

type Program = Class list
and Class = {
    ClassName: string
    SuperClass: string option
    Variables: Variable list
    Methods: MethodDecl list }
//and Declaration
//    = Variable of Variable
//    | Method of MethodDecl
and Variable = MType * string
and MethodDecl = {
    MethodName: string
    Public: bool
    Static: bool
    ProcType: ProcType
    Parameters: Variable list
    Body: Stmt }
//and Block = Stmt list
and Stmt
    = Block of Stmt list
    | Decl of Variable
    | If of Expr * Stmt
    | IfElse of Expr * Stmt * Stmt
    | While of Expr * Stmt
    | Assign of Identifier * Expr
    // Alternatively, make assign a DU with reguler and decl
    //| DeclAssign of Variable * Expr
    | MethodCall of Expr * Expr list
    | Return of Expr option
and Expr
    = BinaryOp of Expr * string * Expr
    | UnaryOp of string * Expr
    | MethodCall of Expr * Expr list
    | Parens of Expr
    | Identfier of Identifier
    | New of New
    | Value of Value
and New
    = Object of MType
    | Array of MType * Expr
and Identifier
    = Ident of string
    | Selector of Identifier * Identifier
//    | Array of Expr * Expr
    | Array of Expr * Expr
and MType
    = Int
    | Boolean
    | String
    | Class of string
    | ArrayType of MType
and ProcType
    = Void
    | ProcType of MType
and Value
    = Int of int
    | Boolean of bool
    | String of string
