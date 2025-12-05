
import * as Runtime from '../src/Runtime'
import * as Parser  from '../src/Parser'

let src = `

(let (adder (lambda (x) (lambda (y) (+ x y))))
    ((adder 10) ((lambda (x y) (+ x y)) 10 (* 4 5)))
)

`

console.log('== PARSE '+'='.repeat(72));
console.log('SOURCE:\n'+src+'\n');
let code = Parser.parse(src);
console.log('-- GOT '+'-'.repeat(73));
console.log(Parser.deparse(code));
console.log('='.repeat(80)+"\n");

console.log('== EVAL '+'='.repeat(72));
let env    = Runtime.initEnv();
let result = Runtime.evaluate(code, env);
console.log('-- GOT '+'-'.repeat(73));
console.log(Parser.deparse(result));
console.log('='.repeat(80)+"\n");


/*

((lambda (x y) (+ x y)) 10 (* 4 5))

(((lambda (x) (lambda (y) (+ x y))) 10) 20)


    (defun (add x y) (+ x y))
    (add 10 20)

    (defun (adder x) (lambda (y) (add x y)))

    ((adder 16) 24)

(defun factorial (n)
    (if (== n 0)
        (1)
        (* n (factorial (- n 1)))))

(defun even? (n) (cond ((== n 0) true) (else (odd? (- n 1)))))
(defun odd? (n) (cond ((== n 0) false) (else (even? (- n 1)))))


        `30`,
        `(+ 10 20)`,
        `(+ 10 (+ 10 10))`,
        `(+ (* 2 5) 20)`,
        `(+ (+ 5 5) (* 2 10))`,
        `((lambda (x y) (+ x y)) 10 20)`,
        `((lambda (x y) (+ x y)) (+ 5 5) 20)`,
        `((lambda (x y) (+ x y)) 10 (* 2 10))`,
        `((lambda (x y) (+ x y)) (+ 5 5) (* 2 10))`,
        `(((lambda (x) (lambda (y) (+ x y))) 10) 20)`,


*/






