open Core
open Core.Poly

(* A "randomizer" is a function that returns a number in the range 0...n-1 *)
type randomizer = int -> int

(* Expressions in Feather Pie *)
type expr =
    (* SIL *)
    | Num of int
    | AddExpr of (expr * expr)
    | MulExpr of (expr * expr)
    | NegExpr of expr
        (* IfThenElse has this form:
           if <expr> <op> <expr> then <expr> else <expr>
           The condition is spread across two expressions and an operator,
           which must be one of "<", ">", or "=". This is to remove booleans
           from the language while still having boolean expressions. *)
    | IfThenElse of (expr * string * expr * expr * expr)
    (* Featherweight Java *)
    | Var of string
    | FieldRead of expr * string (* Target, field name *)
    | MethodCall of expr * string * expr list (* Target, method name, arguments *)
    | ObjLiteral of string * expr list (* Class name and field values *)
    (* Pi calculus *)
    | Receive of string * string * expr (* Channel, variable to receive, following expr *)
    | Send of string * expr * expr (* Channel, value to send, following expr *)
    (* Erlang-ish *)
    | Spawn of string * string * expr (* Class name, variable to receive the channel name into, following expr *)
    (* Extras *)
    | Print of expr


(* A method consists of a name, a list of parameters, and a body expression *)
type methodDecl = string * string list * expr

(* The only declarations in Feather Pie are class declarations: Class name, superclass name, field list, and method list *)
type classDecl = string * string * string list * methodDecl list

(* A Feather Pie program is a list of class declarations *)
type program = classDecl list

(* A substitution is a "from" and "to": string and expression *)
type substitution = string * expr


(* Is this token an ID-like? *)
let isID (s : string) =
    (s.[0] >= 'A' && s.[0] <= 'Z') || (s.[0] >= 'a' && s.[0] <= 'z')

(* Is this token a variable name? i.e., an ID other than a keyword *)
let isVar (s : string) =
    isID s &&
    not (List.exists ~f:(fun x -> (x = s)) [
        "if"; "class"; "extends"; "new"; "receive"; "send"; "spawn"; "print"
    ])

(* Is this token a number? *)
let isNum (s : string) =
    s.[0] >= '0' && s.[0] <= '9'

(* Is this a comparison operator? *)
let isCmpOp (s : string) =
    s = "<" || s = ">" || s = "="


(* Get an ID token *)
let rec idTok (l : char list) : char list * char list =
    match l with
    | c :: r ->
        if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
            let (tok, rest) = idTok r in
                (c :: tok, rest)
        else
            ([], c :: r)
    | [] -> ([], [])

(* Get a number token *)
let rec numTok (l : char list) : char list * char list =
    match l with
    | c :: r ->
        if (c >= '0' && c <= '9') then
            let (tok, rest) = numTok r in
                (c :: tok, rest)
        else
            ([], c :: r)
    | [] -> ([], [])

(* Skip the remainder of this line (for comments) *)
let rec skipLine (l : char list) : char list =
    match l with
    | [] -> []
    | '\n' :: r -> r
    | _ :: r -> skipLine r

(* Get a single token of any sort *)
let rec token (l : char list) : (char list * char list) option =
    match l with
    | '/' :: '/' :: r ->
        token (skipLine r)
    | c :: r ->
        if Char.is_whitespace c then
            token r
        else if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then
            Some (idTok l)
        else if (c >= '0' && c <= '9') then
            Some (numTok l)
        else
            Some ([c], r)
    | [] -> None

(* Tokenize a program *)
let rec tokenize (l : char list) : string list =
    match token l with
    | Some (tok, rest) -> String.of_char_list tok :: tokenize rest
    | None -> []


(* Parse an if-then-else *)
let rec parseIfThenElse (l : string list) : (expr * string list) option =
    (* if (... *)
    match l with
    | "(" :: rest -> (
        (* if (left ... *)
        match parseAddExpr rest with
        | None -> None
        | Some (left, rest) ->

        (* if (left op ... *)
        match rest with
        | [] -> None
        | op :: rest ->
        if not (isCmpOp op) then None else

        (* if (left op right ... *)
        match parseAddExpr rest with
        | None -> None
        | Some (right, rest) ->

        (* if (left op right) ... *)
        match rest with
        | ")" :: rest -> (
            (* if (left op right) thenExpr ... *)
            match parseAddExpr rest with
            | None -> None
            | Some (thenExpr, rest) ->

            (* if (left op right) thenExpr else ... *)
            match rest with
            | "else" :: rest -> (
                (* if (left op right) thenExpr else elseExpr *)
                match parseAddExpr rest with
                | None -> None
                | Some (elseExpr, rest) ->

                    Some (IfThenElse (left, op, right, thenExpr, elseExpr), rest)

            )
            | _ -> None

        )
        | _ -> None

    )
    | _ -> None

(* Parse an argument list *)
and parseArgList (l : string list) : (expr list * string list) option =
    match l with
    | ")" :: _ -> Some ([], l)
    | _ ->
    match parseAddExpr l with
    | None -> None
    | Some (first, rest) ->
    match rest with
    | ")" :: _ -> Some ([first], rest)
    | "," :: rest -> (
        match parseArgList rest with
        | None -> None
        | Some (al, rest) -> Some (first :: al, rest)
    )
    | _ -> None

(* Parse an object literal (new syntax) *)
and parseObjLiteral (l : string list) : (expr * string list) option =
    match l with
    | nm :: "(" :: rest -> (
        match parseArgList rest with
        | None -> None
        | Some (al, rest) ->
        match rest with
        | ")" :: rest -> Some (ObjLiteral (nm, al), rest)
        | _ -> None
    )
    | _ -> None

(* Parse a parenthetical *)
and parseParenthetical (l : string list) : (expr * string list) option =
    match parseExpr l with
    | None -> None
    | Some (expr, rest) ->
    match rest with
    | ")" :: rest -> Some (expr, rest)
    | _ -> None

(* Parse a primary expression, of which there are many forms *)
and parsePrimary (l : string list) : (expr * string list) option =
    match l with
    | [] -> None
    | first :: rest ->
    if isVar first then
        Some (Var first, rest)
    else if isNum first then
        Some (Num (int_of_string first), rest)
    else if first = "if" then
        parseIfThenElse rest
    else if first = "new" then
        parseObjLiteral rest
    else if first = "(" then
        parseParenthetical rest
    else
        None

(* Parse the rest of a postfix expression *)
and parsePostfixRest (left : expr) (l : string list) : (expr * string list) option =
    match l with
    | "." :: nm :: "(" :: rest -> (
        match parseArgList rest with
        | None -> None
        | Some (al, rest) ->
        match rest with
        | ")" :: rest -> parsePostfixRest (MethodCall (left, nm, al)) rest
        | _ -> None
    )
    | "." :: nm :: rest -> (
        (* a .nm without a ( is a field access *)
        parsePostfixRest (FieldRead (left, nm)) rest
    )
    | _ -> Some (left, l)

(* Parse a postfix expression (field read or method call) *)
and parsePostfix (l : string list) : (expr * string list) option =
    match parsePrimary l with
    | None -> None
    | Some (left, rest) -> parsePostfixRest left rest

(* Parse a prefix expression (other than Pi calculus prefixes) *)
and parsePrefixExpr (l : string list) : (expr * string list) option =
    match l with
    | "-" :: rest -> (
        match parsePrefixExpr rest with
        | None -> None
        | Some (expr, rest) -> Some (NegExpr expr, rest)
    )
    | "print" :: rest -> (
        match parsePrefixExpr rest with
        | None -> None
        | Some (expr, rest) -> Some (Print expr, rest)
    )
    | _ -> parsePostfix l

(* Parse the rest of a multiplicative expression *)
and parseMulExprRest (left : expr) (l : string list) : (expr * string list) option =
    match l with
    | "*" :: rest -> (
        match parsePrefixExpr rest with
        | None -> None
        | Some (right, rest) -> Some (MulExpr (left, right), rest)
    )
    | _ -> Some (left, l)

(* Parse a multiplicative expression *)
and parseMulExpr (l : string list) : (expr * string list) option =
    match parsePrefixExpr l with
    | None -> None
    | Some (left, rest) -> parseMulExprRest left rest

(* Parse the rest of an additive expression *)
and parseAddExprRight (left : expr) (l : string list) : (expr * string list) option =
    match l with
    | "+" :: rest -> (
        match parseMulExpr rest with
        | None -> None
        | Some (right, rest) -> parseAddExprRight (AddExpr (left, right)) rest
    )
    | _ -> Some (left, l)

(* Parse an expression. The top-level expression is addition, so start there. *)
and parseAddExpr (l : string list) : (expr * string list) option =
    match parseMulExpr l with
    | None -> None
    | Some (left, rest) -> parseAddExprRight left rest

(* Parse the "header" of a pi calculus expression *)
and parsePiExpr (l : string list) : ((expr -> expr) * string list) option =
    match l with
    | "receive" :: chan :: "(" :: nm :: ")" :: "." :: rest -> (
        Some ((fun x -> Receive (chan, nm, x)), rest)
    )
    | "send" :: chan :: "(" :: rest -> (
        match parseAddExpr rest with
        | None -> None
        | Some (msg, rest) ->
        match rest with
        | ")" :: "." :: rest -> (
            Some ((fun x -> Send (chan, msg, x)), rest)
        )
        | _ -> None
    )
    | "spawn" :: klass :: "(" :: nm :: ")" :: "." :: rest -> (
        Some ((fun x -> Spawn (klass, nm, x)), rest)
    )
    | _ -> None

(* Parse an expression. Pi calculus is our outermost expression syntax, because
 * receive/send only work as outermost expressions. *)
and parseExpr (l : string list) : (expr * string list) option =
    match parsePiExpr l with
    | None -> parseAddExpr l
    | Some (p, rest) ->
    match parseExpr rest with
    | None -> None
    | Some (q, rest) -> Some (p q, rest)

(* Parse a field declaration list *)
let rec parseFieldDeclList (l : string list) : (string list * string list) option =
    match l with
    | nm :: ";" :: rest -> (
        match parseFieldDeclList rest with
        | None -> Some ([nm], rest)
        | Some (fdl, rest) -> Some (nm :: fdl, rest)
    )
    | _ -> Some ([], l)

(* Parse a parameter list *)
let rec parseParamList (l : string list) : (string list * string list) option =
    match l with
    | ")" :: _ -> Some ([], l)
    | nm :: "," :: rest -> (
        match parseParamList rest with
        | None -> None
        | Some (pl, rest) -> Some (nm :: pl, rest)
    )
    | nm :: rest -> Some ([nm], rest)
    | _ -> None

(* Parse a method declaration *)
let parseMethodDecl (l : string list) : (methodDecl * string list) option =
    match l with
    | nm :: "(" :: rest -> (
        match parseParamList rest with
        | None -> None
        | Some (pl, rest) ->
        match rest with
        | ")" :: "{" :: rest -> (
            match parseExpr rest with
            | None -> None
            | Some (body, rest) ->
            match rest with
            | "}" :: rest -> Some ((nm, pl, body), rest)
            | _ -> None
        )
        | _ -> None
    )
    | _ -> None

(* Parse a method declaration list *)
let rec parseMethodDeclList (l : string list) : (methodDecl list * string list) option =
    match parseMethodDecl l with
    | None -> Some ([], l)
    | Some (md, rest) ->
    match parseMethodDeclList rest with
    | None -> Some ([md], rest)
    | Some (mdl, rest) -> Some (md :: mdl, rest)

(* Parse a class declaration *)
let parseClassDecl (l : string list) : (classDecl * string list) option =
    match l with
    | "class" :: nm :: "extends" :: ext :: "{" :: rest -> (
        match parseFieldDeclList rest with
        | None -> None
        | Some (fdl, rest) ->
        match parseMethodDeclList rest with
        | None -> None
        | Some (mdl, rest) ->
        match rest with
        | "}" :: rest -> Some ((nm, ext, fdl, mdl), rest)
        | _ -> None
    )
    | _ -> None

(* Parse a class declaration list *)
let rec parseClassDeclList (l : string list) : (classDecl list * string list) option =
    match parseClassDecl l with
    | None -> Some ([], l)
    | Some (d1, rest) ->
    match parseClassDeclList rest with
    | None -> Some ([d1], rest)
    | Some (cdl, rest) -> Some (d1 :: cdl, rest)

(* Parse a single Feather Pie expression *)
let parseFpieExpr (s : string) : expr option =
    match parseExpr (tokenize (String.to_list s)) with
    | Some (e, []) -> Some e
    | _ -> None

(* Parse a Feather Pie program *)
let parseFpie (s: string) : classDecl list option =
    match parseClassDeclList (tokenize (String.to_list s)) with
    | Some (cdl, []) -> Some cdl
    | _ -> None


(* Remove a name from a substitution list *)
let rec subExcept (s : substitution list) (x : string) : substitution list =
    match s with
    | [] -> []
    | (y, e) :: rest ->
    if x = y then
        subExcept rest x
    else
        (y, e) :: subExcept rest x


(* Perform a substitution over a single name *)
let rec subName (nm : string) (s : substitution list) : expr =
    match s with
    | [] -> Var nm
    | (from, too) :: rest ->
    if nm = from then
        subExpr too rest
    else
        subName nm rest

(* Substitute names in this expression *)
and subExpr (expr : expr) (s : substitution list) : expr =
    match expr with
    | Num x -> expr
    | AddExpr (l, r) -> AddExpr (subExpr l s, subExpr r s)
    | MulExpr (l, r) -> MulExpr (subExpr l s, subExpr r s)
    | NegExpr x -> NegExpr (subExpr x s)
    | IfThenElse (l, o, r, t, e) -> IfThenElse (subExpr l s, o, subExpr r s, subExpr t s, subExpr e s)
    | Var v -> subName v s
    | FieldRead (e, f) -> FieldRead (subExpr e s, f)
    | MethodCall (e, m, al) -> MethodCall (subExpr e s, m, subExprList al s)
    | ObjLiteral (c, fl) -> ObjLiteral (c, subExprList fl s)
    | Receive (c, m, b) -> (
        match subName c s with
        | Var cn -> Receive (cn, m, subExpr b (subExcept s m))
        | _ -> expr (* Invalid substitution! *)
    )
    | Send (c, m, b) -> (
        match subName c s with
        | Var cn -> Send (cn, subExpr m s, subExpr b s)
        | _ -> expr (* Invalid substitution! *)
    )
    | Spawn (cl, ch, b) -> Spawn (cl, ch, subExpr b (subExcept s ch))
    | Print x -> Print (subExpr x s)

(* Substitute names in this expression list *)
and subExprList (el : expr list) (s : substitution list) : expr list =
    match el with
    | [] -> []
    | first :: rest -> (subExpr first s) :: (subExprList rest s)


(* Global counter for fresh names *)
let freshNameCounter = ref 0

(* Reset the counter *)
let resetCounter _ =
    freshNameCounter := 0

(* Get a fresh name suffix *)
let freshName _ =
    let fc = !freshNameCounter in
    freshNameCounter := fc + 1;
    string_of_int fc

(* Freshen a variable name *)
let freshenName nm =
    nm ^ "_" ^ (freshName ())

(* Get a fresh channel name *)
let freshChannel _ =
    freshenName "ch"


(* Get the class that corresponds to this name, if there is one *)
let rec findClass (dl : classDecl list) (nm : string) : classDecl option =
    match dl with
    | [] -> None
    | cd :: rest ->
    let (cn, _, _, _) = cd in
    if cn = nm then
        Some cd
    else
        findClass rest nm

(* Get the class list that corresponds to a given class name, in this
 * declaration list, reversed *)
let rec classListRev (dl: classDecl list) (nm : string) : classDecl list =
    match findClass dl nm with
    | None -> []
    | Some cd ->
    let (_, sc, _, _) = cd in
    cd :: (classListRev dl sc)

(* Get the class list that corresponds to a given class name, in this
 * declaration list *)
let classList (dl : classDecl list) (nm : string) : classDecl list =
    List.rev (classListRev dl nm)


(* Print an expression *)
let rec printExpr (expr : expr) =
    match expr with
    | Num v -> (
        printf "%d" v
    )
    | AddExpr (l, r) -> (
        printExpr l;
        printf "+";
        printExpr r
    )
    | MulExpr (l, r) -> (
        printExpr l;
        printf "*";
        printExpr r
    )
    | NegExpr x -> (
        printf "(-";
        printExpr x;
        printf ")"
    )
    | IfThenElse (l, o, r, t, e) -> (
        printf "(";
        printExpr l;
        printf "%s" o;
        printExpr r;
        printf " then ";
        printExpr t;
        printf " else ";
        printExpr e;
        printf ")"
    )
    | Var v -> (
        printf "%s" v
    )
    | FieldRead (t, f) -> (
        printf "(";
        printExpr t;
        printf ".%s)" f
    )
    | MethodCall (t, m, al) -> (
        printf "(";
        printExpr t;
        printf ".%s(" m;
        printExprList al;
        printf "))"
    )
    | ObjLiteral (c, fl) -> (
        printf "(new %s(" c;
        printExprList fl;
        printf "))"
    )
    | Receive (c, m, b) -> (
        printf "(receive %s(%s) . " c m;
        printExpr b;
        printf ")"
    )
    | Send (c, m, b) -> (
        printf "(send %s(" c;
        printExpr m;
        printf ") . ";
        printExpr b;
        printf ")"
    )
    | Spawn (c, v, b) -> (
        printf "(spawn %s(%s) . " c v;
        printExpr b;
        printf ")"
    )
    | Print x -> (
        printf "(print ";
        printExpr x;
        printf ")"
    )

(* Print a list of expressions (argument list) *)
and printExprList (el : expr list) =
    match el with
    | first :: second :: rest -> (
        printExpr first;
        printf ",";
        printExprList (second :: rest)
    )
    | first :: [] -> (
        printExpr first
    )
    | [] -> ()
