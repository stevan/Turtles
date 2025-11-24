
import * as Parser from '../src/Parser'
import { Machine } from '../src/Machine'

let ast = Parser.parse(`(if (== 10 10) (* 2 10) (+ 2 10))`);
let m   = new Machine();
let got = m.run( ast );

//console.log(JSON.stringify(got, null, 4));
console.log('GOT', Parser.format(got));

