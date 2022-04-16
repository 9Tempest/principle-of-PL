(* This template can be used with or without Core, but requires Core.Poly if you use Core
open Core
open Core.Poly
*)
open Printf
open Fpie

(* NON-CONCURRENT BEHAVIOR *)
let rec createSubLists (params : string list) (args: expr list) (target: expr) : substitution list =
    match params, args with
    | [], [] -> [("this", target)]
    | param:: paramRest , arg :: argsRest -> (param, arg) :: (createSubLists paramRest argsRest target)

let rec fetchMethod (m : string) (methods: methodDecl list) : Fpie.methodDecl option =
    match methods with
    | [] -> None
    | (name, params, body) :: rest -> (
        if (String.equal m name) then Some (name, params, body)
        else (fetchMethod m rest)
    )

let rec fetchMethodFromClasses (m : string) (classes: classDecl list) : Fpie.methodDecl option =
    match classes with
    | [] -> None
    | (cname,sname,fields,methods) :: rest -> (
        match fetchMethod m methods with
        | None -> fetchMethodFromClasses m rest
        | Some met -> Some met
    )

let fetchMethodInProgram (m : string) (c : string) (decls: Fpie.program) : Fpie.methodDecl option =
    fetchMethodFromClasses m (classListRev decls c) 

(* Take a single step of execution *)
let rec step (expr : Fpie.expr) (decls : Fpie.program) : Fpie.expr option =
    let rec evalArgs (argList : Fpie.expr list) (decls : Fpie.program) (evaluatedLst : Fpie.expr list) : (bool * Fpie.expr list) =
    match argList with
    | [] -> (true, evaluatedLst)
    | arg :: rest ->
        match step arg decls with
            | None -> evalArgs rest decls (arg :: evaluatedLst)
            | Some arg' -> (false, (List.append evaluatedLst (arg' :: rest)))
    in
    match expr with
    | Num _ -> None

    (* Math *)
    | AddExpr (l, r) -> (
        match step l decls with
        | Some ls -> Some (AddExpr (ls, r))
        | None ->
        match step r decls with 
        | Some rs -> Some (AddExpr (l, rs))
        | None ->
        match l, r with
        | Num l, Num r -> Some (Num (l+r))
        | _ -> None
    )
    | MulExpr (l, r) -> (
        match step l decls with
        | Some ls -> Some (MulExpr (ls, r))
        | None ->
        match step r decls with
        | Some rs -> Some (MulExpr (l, rs))
        | None ->
        match l, r with
        | Num l, Num r -> Some (Num (l*r))
        | _ -> None
    )
    | NegExpr x -> (
        match step x decls with
        | Some xs -> Some (NegExpr xs)
        | None ->
        match x with
        | Num x -> Some (Num (-x))
        | _ -> None
    )
    | IfThenElse (l, o, r, t, e) -> (
        match step l decls with
        | Some ls -> Some (IfThenElse (ls, o, r, t, e))
        | None ->
        match step r decls with
        | Some rs -> Some (IfThenElse (l, o, rs, t, e))
        | None ->
        match l, r with
        | Num l, Num r -> (
            let b = (
                if o = "<" then l < r
                else if o = ">" then l > r
                else l = r
            ) in
            if b then Some t else Some e
        )
        | _ -> None
    )
    | Var _ -> None

    (* OO *)
    | FieldRead (t, f) -> None (* FILL IN THIS CASE *)
    | MethodCall (t, m, al) -> (
        match step t decls with
        | Some t' -> Some (MethodCall (t', m, al))
        | None -> (
            match t with 
            | ObjLiteral (c, fl) -> (
                match evalArgs al decls [] with (* eval args *)
                | (true, al') -> ( (* call method *)
                    match fetchMethodInProgram m c decls with
                    | None -> None
                    | Some (name, params, body) -> (
                        Some (subExpr body (createSubLists params al' t))
                    )
                )
                | (false, al') -> Some (MethodCall (t,c, al'))
            )    
            | _ -> None
        )

    )
    | ObjLiteral (c, fl) -> (
        match evalArgs fl decls [] with
        | (true, fl') -> (
            printExprList fl';
            None
        )
        | (false, fl') -> (
            printExprList fl';
            Some (ObjLiteral (c, fl')))
    )
        
    (* Concurrent *)
    | Receive (c,v,e) -> None
    | Send (c, v, b) -> None (* FILL IN THIS CASE *)
    | Spawn _ -> None

    (* Printing *)
    | Print v -> (
        match step v decls with
        | Some vs -> Some (Print vs)
        | None ->
        match v with
        | Num n -> (
            printf "%d\n" n;
            Some v
        )
        | _ -> Some v
    )


    

let rec eval (expr : Fpie.expr) (decls : Fpie.program) : Fpie.expr =
    match step expr decls with
    | Some res -> eval res decls
    | None -> expr


(* CONCURRENT BEHAVIOR *)

(* Run a Feather Pie program *)
let run (expr : Fpie.expr) (decls : Fpie.program) (rand : Fpie.randomizer) : Fpie.expr list =
    [] (* FILL IN THIS CODE *)
