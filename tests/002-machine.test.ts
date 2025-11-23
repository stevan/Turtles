

import * as Parser from '../src/Parser'
import { Machine } from '../src/Machine'

let ast = Parser.parse(`(+ (+ 5 5) (* 2 10))`);

//console.log(JSON.stringify(ast, null, 4));
console.log(Parser.format(ast));

let m   = new Machine();
let got = m.run( ast );

//console.log(JSON.stringify(got, null, 4));
console.log(Parser.format(got));

