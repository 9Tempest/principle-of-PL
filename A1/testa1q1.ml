open Base ;;

let x = A1q1.push [2.0;1.0] 3;;

let x_1 = A1q1.add x ;;
A1q1.printRes x_1 ;;
let x_1 = A1q1.getList x_1 ;;
let x_2 = A1q1.add x_1 ;;
A1q1.printRes x_2 ;;
let x_2 = A1q1.getList x_2 ;;
let x_3 = A1q1.add x_2 ;;
A1q1.printRes x_3 ;;

let y = A1q1.push [-1.0; 2.5; -3.0] 4.0 ;;
A1q1.print y ;;
let y1 = A1q1.sub y ;;
A1q1.printRes y1 ;;
let y1 = A1q1.getList y1 ;;
let y2 = A1q1.sub y1 ;;
A1q1.printRes y2 ;;
let y2 = A1q1.getList y2 ;;
let y3 = A1q1.sub y2 ;;
A1q1.printRes y3 ;;
let y3 = A1q1.getList y3 ;;
let y4 = A1q1.sub y3 ;;
A1q1.printRes y4 ;;

let z = A1q1.push [0.; 2.; 4.; 7.] 5. ;;
A1q1.print z ;;
let z1 = A1q1.mul z ;;
A1q1.printRes z1 ;;
let z1 = A1q1.getList z1 ;;
let z2 = A1q1.sub z1 ;;
A1q1.printRes z2 ;;
let z2 = A1q1.getList z2 ;;
let z3 = A1q1.mul z2 ;;
A1q1.printRes z3 ;;

let m = A1q1.push [-1.0; 3.0; -2.0; 6.0] 5.0 ;;
A1q1.print m ;;
let m1 = A1q1.div m ;;
A1q1.printRes m1 ;;
let m1 = A1q1.getList m1 ;;
let m2 = A1q1.div m1 ;;
A1q1.printRes m2 ;;
let m2 = A1q1.getList m2 ;;
let m3 = A1q1.div m2 ;;
A1q1.printRes m3 ;;
let m3 = A1q1.getList m3 ;;
let m4 = A1q1.div m3 ;;
A1q1.printRes m4 ;;

A1q1.print [1.;2.;3.] ;;
A1q1.print [-4.;0.;5.] ;;
A1q1.print [] ;;
A1q1.print [1.] ;;
