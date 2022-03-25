open SIL
open Core

type storeVal =
    | NumVal of int
    | ProcDecl of procedure

let rec write store str (value: storeVal) =
    match store with
    | [] -> [(str, value)]
    | (name, oriVal) :: rest -> (
        if (String.equal name str) then (str, value) :: rest
        else (name, oriVal) :: (write rest str value)
    )

let rec get store str =
    match store with
    | [] -> NumVal (0)
    | (name, value) :: rest -> (
        if (String.equal name str) then value
        else get rest str
    ) 

let rec eval (exp: expr) store: (int) = 
    match exp with
    | Num num -> num
    | AddExpr (e1, e2) -> (eval e1 store) + (eval e2 store)
    | MulExpr (e1, e2) -> (eval e1 store) * (eval e2 store)
    | NegExpr exp -> - (eval exp store)
    | Var var -> match get store var with
            | NumVal n -> n
            | _ -> 0 

let rec eval_bool (exp: boolExpr) store: (bool) = 
    match exp with
    | True -> true
    | False -> false
    | Not exp -> not (eval_bool exp store)
    | And (exp1, exp2) -> (eval_bool exp1 store) && (eval_bool exp2 store)
    | Or (exp1, exp2) -> (eval_bool exp1 store) || (eval_bool exp2 store)
    | Gt (exp1, exp2) -> (eval exp1 store) > (eval exp2 store)
    | Lt (exp1, exp2) -> (eval exp1 store) < (eval exp2 store)
    | Eq (exp1, exp2) -> (eval exp1 store) == (eval exp2 store)

let rec evalArgs (args: expr list) store = 
    match args with
    | [] -> []
    | h::t -> (eval h store) :: (evalArgs t store)

let rec assignArgs (args: int list) (exprs: string list) : (statement list) =
    match args with
    | [] -> []
    | arg:: resta -> match exprs with
        | exp:: reste -> VarAssgStmt(exp, (Num(arg))) :: (assignArgs resta reste)

let expandWhile expr stmt = IfStmt (expr, Block([stmt; WhileStmt (expr, stmt)]),Skip)

let rec runMain (stmts: statement list) store = 
    let rec runStmt (stmt: statement) store = 
        match stmt with
        | Skip -> store
        | Block b -> (
            runMain b store
        )
        | VarAssgStmt (target, expr) ->  write store target (NumVal (eval expr store)) 
        | PrintStmt expr -> (
            printf "%d\n" (eval expr store);
            store
        )
        | IfStmt (exp, stmt1, stmt2) -> (
            if (eval_bool exp store) then runStmt stmt1 store
            else runStmt stmt2 store
        )
        | WhileStmt (expr, stmt) -> runStmt (expandWhile expr stmt) store
        | ProcDecl (proc) -> (
            match proc with
                | (name, params, decls,  pstmts) -> write store name (ProcDecl(proc));
            )
        | CallStmt (name, args) -> (
            match get store name with
            | ProcDecl (procName, params, decls, pstmts) -> (
                let (params', pstmts') = (freshenProcedure params decls pstmts) in
                let newArgs = (assignArgs (evalArgs args store) params') in
                runMain (List.append newArgs pstmts') store
            )
        )
    in
    match stmts with
    | [] -> store
    | stmt :: stmts -> (
        runMain stmts (runStmt stmt store)
    )

let run  (stmts: statement list) =
    let store = [] in
    runMain stmts store;
    ()