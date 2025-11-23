
import   * as Parser   from '../src/Parser'
import { Interpreter } from '../src/Interpreter'

let ast = Parser.parse(`((lambda (x y) (+ x y)) (+ 5 5) (* 2 10))`);

//console.log(JSON.stringify(ast, null, 4));
console.log(Parser.format(ast));

let i   = new Interpreter();
let got = i.run( ast );

//console.log(JSON.stringify(got, null, 4));
console.log(Parser.format(got));


