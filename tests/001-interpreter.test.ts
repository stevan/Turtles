
import { test } from "node:test"
import  assert  from "node:assert"

import { DEBUG }       from '../src/Runtime'
import   * as AST      from '../src/AST'
import   * as Parser   from '../src/Parser'
import { Interpreter } from '../src/Interpreter'

test("... basic test", (t) => {

    let ast = Parser.parse(`
        ((lambda (x y) (+ x y)) (+ 5 5) (* 2 10))

    `);

    console.log('>>>', DEBUG.SHOW(ast));

    let i   = new Interpreter();
    let got = i.run( ast );

    console.log('<<<', DEBUG.SHOW(got));

    assert.strictEqual(
        JSON.stringify(got),
        JSON.stringify(AST.Num(30)),
        '... got the expected result'
    );

})
