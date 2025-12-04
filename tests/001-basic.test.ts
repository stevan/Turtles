
import * as Runtime from '../src/Runtime'
import * as Parser  from '../src/Parser'

let env = Runtime.initEnv();

console.log(Parser.deparse(Runtime.evaluate(Parser.parse(`

    (let (x 2)
    (let (y 10)
        ((lambda (x) (== y x)) (* 5 x))
    ))

`), env)));


/*
(== (((lambda (x) (lambda (y) (+ x y))) 10) 23) 30)
*/









