
import * as Parser from '../src/Parser'
import { Machine } from '../src/Machine'

let ast = Parser.parse(`
    (((lambda (x)
        (lambda (y)
            (lambda (z)
                (+ x (* y z))
            )
        )
    ) 5) 2)
`);
let m   = new Machine();
let got = m.run( ast );

//console.log(JSON.stringify(got, null, 4));
console.log('GOT', Parser.format(got));

