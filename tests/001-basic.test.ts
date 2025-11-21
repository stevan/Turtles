

import { Runtime, Parser } from '../src/Corgi'

let env     = Runtime.createRootEnvironment();
let program = Parser.parse(`(
    (set! addit
        (lambda (x y)
            (if (== x 10)
                (+ x y)
                (* x y))))
    (addit 11 20)
)`);

console.log(Parser.format(program));

Runtime.DUMP( 'RESULT', new Runtime.Machine().evaluate( program, env ), env );
