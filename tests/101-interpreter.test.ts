
import * as Parser from '../src/Parser'
import { Interpreter } from '../src/Interpreter'

let ast = Parser.parse(`(
    (defun (add x y) (+ x y))
    (add 10 20)

    (defun (adder x) (lambda (y) (add x y)))

    ((adder 16) 24)
)`);
let m   = new Interpreter();
let got = m.run( ast );

//console.log(JSON.stringify(got, null, 4));
console.log('GOT', Parser.format(got));


/*

(defun factorial (n)
    (if (== n 0)
        (1)
        (* n (factorial (- n 1)))))

(defun even? (n) (cond ((== n 0) true) (else (odd? (- n 1)))))
(defun odd? (n) (cond ((== n 0) false) (else (even? (- n 1)))))


*/
