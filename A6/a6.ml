(* This template can be used with or without Core, but requires Core.Poly if you use Core
open Core
open Core.Poly
*)
open Printf
open Fpie

(* NON-CONCURRENT BEHAVIOR *)

let rec createFieldLists (classes: classDecl list) : string list =
    match classes with
    | [] -> []
    | (cname,sname,fields,methods)  :: rest -> (List.append fields (createFieldLists rest))

let rec fieldRead (f : string) (fl : expr list) (fstrl : string list) : expr = 
    match fl, fstrl with
    | exp :: rest_e, f' :: rest_f -> (
        if (String.equal f f' ) then ( exp)
        else fieldRead f rest_e rest_f
    )

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
            | None -> evalArgs rest decls (List.append evaluatedLst [arg])
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
    | FieldRead (t, f) -> (
        match step t decls with
        | Some t' -> Some (FieldRead (t', f))
        | None -> (
            match t with
            | ObjLiteral (c, fl) -> Some (fieldRead f fl (createFieldLists (classList decls c)))
        )
    )
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
                | (false, al') -> Some (MethodCall (t,m, al'))
            )    
            | _ -> None
        )

    )
    | ObjLiteral (c, fl) -> (
        match evalArgs fl decls [] with
        | (true, fl') -> (
            None
        )
        | (false, fl') -> (
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
    | Some res -> ( eval res decls)
    | None -> expr

let checkComm (exprs: Fpie.expr array) (expr: Fpie.expr) (idx: int) : bool = 
    let res = ref false in
    (match expr with
    | Receive (c,v,e) -> (
        let i =  ref 0 in
        while (!i < (Array.length exprs)) do
            match (Array.get exprs !i) with
            | Send (c', v', b') -> (
                if (String.equal c c') 
                then (
                    i := (Array.length exprs);
                    res := true
                )   else (i := !i+1)
            )
            | _ -> (i := !i+1)
        done
    )
    | Send (c,v,e) -> (
        let i =  ref 0 in
        while (!i < (Array.length exprs)) do
            match (Array.get exprs !i) with
            | Receive (c', v', b') -> (
                if (String.equal c c') 
                then (
                    i := (Array.length exprs);
                    res := true
                )   else (i := !i+1)
            )
            | _ -> (i := !i+1)
        done
    )
    | _ -> ());
    !res

let matchComm (exprs: Fpie.expr array) (expr: Fpie.expr) (idx: int) (decls : Fpie.program) : unit = 
    match expr with
    | Receive (c,v,e) -> (
        let i =  ref 0 in
        while (!i < (Array.length exprs)) do
            match (Array.get exprs !i) with
            | Send (c', v', b') -> (
                if (String.equal c c') 
                then (
                    (Array.set exprs !i (eval b' decls));
                    (Array.set exprs idx (eval (subExpr e [(v, v')]) decls));
                    i := (Array.length exprs)
                )   else (i := !i+1)
            )
            | _ -> (i := !i+1)
        done
    )
    | Send (c,v,e) -> (
        let i =  ref 0 in
        while (!i < (Array.length exprs)) do
            match (Array.get exprs !i) with
            | Receive (c', v', b') -> (
                if (String.equal c c') 
                then (
                    (Array.set exprs !i (eval (subExpr b' [(v', v)]) decls));
                    (Array.set exprs idx (eval e decls));
                    i := (Array.length exprs)
                )   else (i := !i+1)
            )
            | _ -> (i := !i+1)
        done
    )

let checkEnd (exprs: Fpie.expr array) : bool = 
    let endFlag = ref true in
    for i = 0 to ((Array.length exprs) -1)
    do
        match (Array.get exprs i) with
        | Spawn (c, v, e) ->  endFlag := false;
        | Receive (c, v, e) -> if (checkComm exprs (Receive(c, v, e)) i) then ( endFlag := false)
        | Send (c, v, e)-> if (checkComm exprs (Send(c, v, e)) i) then endFlag := false
        | Num (n) -> if (n==0) then endFlag := false
        | _ -> ()
    done;
    !endFlag

let rec removeListAtIdx (exprs: Fpie.expr list) (idx: int): Fpie.expr list = 
    match exprs with
    | [] -> []
    | h :: t -> (
        if (idx == 0) then t
        else h :: (removeListAtIdx t (idx-1))
    )

(* CONCURRENT BEHAVIOR *)
let rec doProcess (exprs: Fpie.expr array ref) (rand : Fpie.randomizer) (decls : Fpie.program) : unit = 
    let len = (Array.length !exprs) in
    match checkEnd !exprs with
    | true -> ()
    | false ->(
    (*printf "in the step:\n";
    printExprList (Array.to_list !exprs);*)
    let idx = (rand len) in
    match (Array.get !exprs idx) with
    | Spawn (c, v, e) -> (
        let v' = Var(freshenName v) in
        (Array.set !exprs idx (eval (subExpr e [(v, v')]) decls));
        let m = Fpie.MethodCall (Fpie.ObjLiteral (c, [v']), "main", []) in 
        exprs := (Array.of_list ((eval m decls) :: (Array.to_list !exprs)));
        doProcess exprs rand decls
    )
    | Receive (c, v, e) -> (
        matchComm !exprs (Receive (c, v, e)) idx decls;
        doProcess exprs rand decls
    )
    | Send (c, v, e) -> (
        matchComm !exprs (Send (c, v, e)) idx decls;
        doProcess exprs rand decls
    )
    | Num (n) -> (
        if (n == 0) then (
            exprs := (Array.of_list (removeListAtIdx (Array.to_list !exprs) idx));
            doProcess exprs rand decls
        )
        else doProcess exprs rand decls
    )
    | _ -> doProcess exprs rand decls)
    

(* Run a Feather Pie program *)
let run (expr : Fpie.expr) (decls : Fpie.program) (rand : Fpie.randomizer) : Fpie.expr list =
    let res_exp = (eval expr decls) in
    let exprs = ref (Array.make 1 res_exp) in
    doProcess exprs rand decls;
    (Array.to_list !exprs) (* FILL IN THIS CODE *)
