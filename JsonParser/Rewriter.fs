module Rewriter

open Ast

type NewSelector = NewSelector of Identifier list

module Helpers =
    let rec rst (stmt:Stmt) =
        match stmt with
        | Stmt.MethodCall(e) -> Stmt.MethodCall(re e)
        | Stmt.Block(ss) -> ss |> List.map rst |> Stmt.Block
        | Stmt.Single(s) -> s |> rst |> Stmt.Single
        | Stmt.Decl(_) -> stmt
        | Stmt.Assign(e1, e2) -> (re e1, re e2) |> Stmt.Assign
        | Stmt.Return(e) -> Option.map re e |> Stmt.Return
        | Stmt.If(e, s) -> Stmt.If(re e, rst s)
        | Stmt.IfElse(e, s1, s2) -> Stmt.IfElse(re e, rst s1, rst s2)
        | Stmt.While(e, s) -> Stmt.While(re e, rst s)
    and re e =
        match e with
        | Identifier(s) -> rsel s
        | _ -> e
    and rsel s =
        match s with
        | Selector(target, MethodCall(name, args)) ->
            MethodCall(re (Identifier(Selector(re target,re name))), args) 
        | Selector(target, Identifier(Array(e1, e2))) ->
            Identifier(Array(re (Identifier(Selector(re target, re e1))), re e2))
        | Selector(target, obj) ->
            Identifier(Selector(re target, obj))
        | _ -> Identifier s

let rewriteProgram (p:Program) =
    p
    |> List.map (fun c ->
        let methods =
            c.Methods |> List.map (fun m ->
                let body = m.Body |> List.map Helpers.rst
                { m with Body = body })
        { c with Methods = methods } )
