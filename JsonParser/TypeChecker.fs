module TypeChecker

open Ast

let rec typeCheckProgram (program:Program) =
    match program with
    | [] -> true
    | x :: xs -> List.exists (typeCheckClass) program
and typeCheckClass (cl : Class) =
    true