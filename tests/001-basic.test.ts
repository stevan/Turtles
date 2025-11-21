

import { Runtime, Parser, ASTUtil, ListUtil } from '../src/Corgi'

let machine = new Runtime.Machine();
let env     = machine.createRootEnvironment();

/*
let program = Parser.parse(`(
    (set! addit
        (lambda (x y)
            (if (== x 10)
                (+ x y)
                (* x y))))
    (addit 11 20)
)`);

*/

let program = ListUtil.create(
    ListUtil.create(
        ASTUtil.Word('set!'),
        ASTUtil.Var('addit'),
        ListUtil.create(
            ASTUtil.Word('lambda'),
            ListUtil.create( ASTUtil.Var('x'), ASTUtil.Var('y') ),
            ListUtil.create(
                ASTUtil.Word('if'),
                ListUtil.create( ASTUtil.Word('=='), ASTUtil.Var('x'), ASTUtil.Num(10) ),
                ListUtil.create( ASTUtil.Word('+'),  ASTUtil.Var('x'), ASTUtil.Var('y') ),
                ListUtil.create( ASTUtil.Word('*'),  ASTUtil.Var('x'), ASTUtil.Var('y') ),
            )
        )
    ),
    ListUtil.create( ASTUtil.Var('addit'), ASTUtil.Num(11), ASTUtil.Num(20) )
)

console.log(Parser.format(program));

Runtime.DUMP( 'RESULT', machine.evaluate( program, env ), env );
