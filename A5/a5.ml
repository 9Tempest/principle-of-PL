open SIL
open Core

type storeVal =
    | NumVal of int
    | ProcDecl of procedure
    | ArrayVal of int array

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

let extractNum (exp: storeVal): int = 
    match exp with
    | NumVal n -> n
    | _ -> 0

let rec eval (exp: expr) store: storeVal = 
    match exp with
    | Num num -> NumVal num
    | AddExpr (e1, e2) -> (NumVal ((extractNum (eval e1 store)) + (extractNum (eval e2 store))))
    | MulExpr (e1, e2) -> (NumVal ((extractNum (eval e1 store)) * (extractNum (eval e2 store))))
    | NegExpr exp ->  (NumVal (- extractNum (eval exp store)))
    | Var var -> get store var 
    | ArrIndexExpr (e1, e2) -> (
        match eval e1 store with
        | ArrayVal a -> NumVal(Array.get a (extractNum (eval e2 store)))
    )

let rec eval_bool (exp: boolExpr) store: (bool) = 
    match exp with
    | True -> true
    | False -> false
    | Not exp -> not (eval_bool exp store)
    | And (exp1, exp2) -> (eval_bool exp1 store) && (eval_bool exp2 store)
    | Or (exp1, exp2) -> (eval_bool exp1 store) || (eval_bool exp2 store)
    | Gt (exp1, exp2) -> (extractNum (eval exp1 store)) > (extractNum(eval exp2 store))
    | Lt (exp1, exp2) -> (extractNum (eval exp1 store)) < (extractNum(eval exp2 store))
    | Eq (exp1, exp2) -> (extractNum (eval exp1 store)) == (extractNum(eval exp2 store))


let rec assignArgs (args: expr list) (exprs: string list) : (statement list) =
    match args with
    | [] -> []
    | arg:: resta -> match exprs with
        | exp:: reste -> VarAssgStmt(exp, arg) :: (assignArgs resta reste)

let expandWhile expr stmt = IfStmt (expr, Block([stmt; WhileStmt (expr, stmt)]),Skip)

let rec runMain (stmts: statement list) store = 
    let rec runStmt (stmt: statement) store = 
        match stmt with
        | Skip -> store
        | Block b -> (
            runMain b store
        )
        | VarAssgStmt (target, expr) ->  write store target (eval expr store)
        | PrintStmt expr -> (
            printf "%d\n" (extractNum (eval expr store));
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
                let newArgs = (assignArgs args params') in
                runMain (List.append newArgs pstmts') store
            )
        )
        | NewArrStmt (name, exp) -> (
            write store name (ArrayVal (Array.create (extractNum (eval exp store)) 0) )
        )
        | ArrAssgStmt (name, exp1, exp2) -> (
            match get store name with
            | ArrayVal a -> (
                let idx = extractNum (eval exp1 store) in
                let ele = extractNum (eval exp2 store) in
                Array.set a idx ele;
                store
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