open Core
open Core.Poly
open In_channel

let progIn = read_all ((Sys.get_argv ()).(1))
let prog = Option.value_exn (Fpie.parseFpie progIn)
let start = Fpie.MethodCall (Fpie.ObjLiteral ("Main", []), "main", [])

let () =
    ignore (A6.eval start prog)
