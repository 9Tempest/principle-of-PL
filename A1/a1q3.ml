open Stdio;;

let stepLeft state = 
    match state with
    | (x :: tapeleft, tc, taperight) -> (tapeleft, x, tc :: taperight)
    |  ([], tc, taperight) -> ([], 0, tc :: taperight)
;;

let stepRight state = 
    match state with
    | (tapeleft, tc, x :: taperight) -> (tc :: tapeleft, x, taperight)
    |  (tapeleft, tc, []) -> (tc :: tapeleft, 0, [])
;;

let step ins idx length state = 
    if idx >= length || idx < 0 then None
    else let inst = ins.(idx) in
         let (tapeleft, tc, taperight) = state in
        match inst with
        | ("+",_) -> Some (idx+1, (tapeleft, tc+1, taperight))
        | ("-",_) -> Some (idx+1, (tapeleft, tc-1, taperight))
        | ("<",_) -> Some (idx+1, (stepLeft state))
        | (">",_) -> Some (idx+1, (stepRight state))
        | (".",_) -> 
            printf "%c" (Char.chr tc);
            Some (idx+1, state)
        | ("J", num) -> Some (idx+num, state)
        | ("B", num) -> if tc == 0 then Some (idx+num, state) else Some (idx+1, state)
;;      

let rec runSteps ins idx length state = 
    match step ins idx length state with
    | Some (pc, outState) -> runSteps ins pc length outState
    | None -> state
;;

let run ins = 
    let state = ([],0,[]) in
    let length = Array.length ins in
    ignore(runSteps ins 0 length state)
;;

