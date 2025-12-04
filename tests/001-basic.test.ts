
import * as Runtime from '../src/Runtime'
import * as Parser  from '../src/Parser'

let src = `

(let (adder (lambda (x) (lambda (y) (+ x y))))
    ((adder 10) 20)
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

(((lambda (x) (lambda (y) (+ x y))) 10) 20)

*/






