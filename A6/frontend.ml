open Core
open Core.Poly
open In_channel

let progIn = read_all ((Sys.get_argv ()).(1))
let prog = Option.value_exn (Fpie.parseFpie progIn)
let start = Fpie.MethodCall (Fpie.ObjLiteral ("Main", []), "main", [])

let rec printBad (hdrPrinted : bool) (exprs : Fpie.expr list) : unit =
    match exprs with
    | [] -> ()
    | first :: rest ->

    let nhdrPrinted = (
        match first with
        | Num _ -> hdrPrinted
        | _ ->
        if not hdrPrinted then
            printf "\nWARNING: Possibly non-terminated process(es):\n"
        else ();
        Fpie.printExpr first;
        printf "\n";
        true
    ) in
    printBad nhdrPrinted rest

let () =
    Random.self_init ();
    let res = A6.run start prog Random.int in
    printBad false res
