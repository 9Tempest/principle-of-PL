open Conlog
open Core


let rec unificationExpr (l: string) (r: string): (substitution list) option = 
    if isVarName l then Some[(l, r)]
    else if isVarName r then Some [(r,l)]
    else if (String.equal l r) then Some []
    else None

let rec processLists (args1: string list) (args2: string list) (acc: substitution list): (substitution list)  option = 
    match args1 with
    | head1 :: body1 -> (
        match args2 with
        | head2 :: body2 -> (
            (*printf "unifying %s with %s\n" head1 head2;*)
            match (unificationExpr (subName head1 acc) (subName head2 acc)) with
            | Some (list) -> (
                match (processLists body1 body2 (List.append acc list)) with
                | Some (res) -> Some (List.append list res) 
                | None -> None
            )
            | None -> None
        )
        | [] -> Some []
    )
    | [] -> Some []     

let unificationRelation ((func1, args1): relation) ((func2, args2): relation): (substitution list) option = 
    if ( not (String.equal func1 func2)) then None
    else if (List.length args1) != (List.length args2) then None
    else (
        processLists args1 args2 []
        )

  

let eval (arg1: int) (arg2: int) (op: string) : bool = 
    match op with
    | ">=" -> (arg1 >= arg2)
    | ">" -> (arg1 > arg2)
    | "<" -> (arg1 < arg2)
    | "<=" -> (arg1 <= arg2)
    | "=\\=" -> (arg1 != arg2) 
    |_ -> false
    



let rec query (db: database ) (preds: predicate list) : (substitution list) option = 
    let rec queryRelation (db1: database ) (pred: relation) (preds1: predicate list)  : (substitution list) option = 
    match db1 with
    | [] -> None
    | hc :: rest -> (
        match(freshenHornClause hc) with
        |(rel, pl) -> (
            match (unificationRelation pred rel) with
            | None -> queryRelation rest pred preds1
            | Some res -> (
                match query db (subPredicateList (List.append pl preds1) res) with
                | None -> (
                    queryRelation rest pred preds1
                    )
                | Some realRes -> (
                    Some (List.append res realRes)
                    )
            )
        )
    ) in
    match preds with
     (** case 1 empty queries *)
    | [] -> Some []
     (** case 2 comparsion *)
    | p :: q -> (
        match p with
        | Comparison (var, "is", exp) -> (
            match exp with
            | Primary pri -> (
                match (unificationExpr var pri) with
                | None -> None
                | Some s -> (
                    match (query db (subPredicateList q s)) with
                    | None -> None
                    | Some (l) -> Some(List.append s l)
                )
            ) 
            | Binexp (_,_,_) -> (
                match solve exp with 
                | None -> None
                | Some res -> (match (unificationExpr var (string_of_int res)) with
                    | None -> None
                    | Some s -> (
                        match (query db (subPredicateList q s)) with
                        | None -> None
                        | Some (l) -> Some(List.append s l)
                    ))
            ) 
        )
        | Comparison (var, op, exp) -> (
            if (not (isNum var)) then None
            else match solve exp with
                | None -> None
                | Some res -> (
                     if (not (eval (int_of_string var) res op)) then None
                     else query db q
                )
        )
        | Relation rel ->  queryRelation db rel q
    )

 


    

