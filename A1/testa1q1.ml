open Stdio;;
open Base;;

let listA = [1.2];;
let listA = A1q1.push listA 1.4;;
let listA = A1q1.push listA 2.;;
let listA = A1q1.push listA 3.;;
let listA = A1q1.push listA 4.;;
let listA = A1q1.push listA 5.;;
let listA = A1q1.push listA 6.;;
let res = A1q1.sub listA;;
A1q1.printRes res;;
let listA = A1q1.getList res;;
let listA = A1q1.push listA 2.;;
A1q1.print listA;;
let res = A1q1.mul listA;;
A1q1.printRes res;;
let listA = A1q1.getList res;;

A1q1.print listA;;
let res = A1q1.sub listA;;
let listA = A1q1.getList res;;
A1q1.print listA;;

let res = A1q1.div listA;;
let listA = A1q1.getList res;;
A1q1.print listA;;

