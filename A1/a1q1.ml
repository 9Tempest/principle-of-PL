open Base;;
open Stdio;;


let push lst v = v :: lst;;

let binary l op = 
    match l with
    | [] -> None
    | [_] -> None
    | first :: (second :: tail) ->
        let res = op second first in
        Some (res, res :: tail)
;;

let add l = 
    let adder x y = x +. y in
    binary l adder
;;

let sub l = 
    let subber x y = x -. y in
    binary l subber
;;

let mul l = 
    let muler x y = x *. y in
    binary l muler
;;

let div l =
    let diver x y = x /. y in
    binary l diver
;;



let rec printHelper l = 
    match l with
    | [] -> printf ")\n"
    | hd :: tl ->
        printf "%F " hd;
        printHelper tl
;;

let print lst = 
    printf "List (";
    printHelper lst
;;

(* Helper to test function *)
let printRes res = 
    match res with
    | None -> printf "error\n"
    | Some (result, lst) ->
        printf "result is %F " result;
        print lst
;;

(* Helper to test function *)
let getList res = 
    match res with
    | None -> []
    | Some (result, lst) -> lst
;;



