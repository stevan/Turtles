

import { Runtime, Parser } from '../src/Corgi'

let env     = Runtime.createRootEnvironment();
let program = Parser.parse(`
    ((lambda (x y)
        (if (== x 10)
            (+ x y)
            (* x y))) 11 20)
`);

console.log(Parser.format(program));

Runtime.DUMP( 'RESULT', Runtime.evaluate( program, env ), env );
