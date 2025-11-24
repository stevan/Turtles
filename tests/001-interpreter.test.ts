
import { test } from "node:test"
import  assert  from "node:assert"

import { DEBUG }       from '../src/Runtime'
import   * as Parser   from '../src/Parser'
import { Interpreter } from '../src/Interpreter'


test("... basic test", (t) => {

    let testCode = [
        `30`,
        `(+ 10 20)`,
        `(+ 10 (+ 10 10))`,
        `(+ (* 2 5) 20)`,
        `(+ (+ 5 5) (* 2 10))`,
        `((lambda (x y) (+ x y)) 10 20)`,
        `((lambda (x y) (+ x y)) (+ 5 5) 20)`,
        `((lambda (x y) (+ x y)) 10 (* 2 10))`,
        `((lambda (x y) (+ x y)) (+ 5 5) (* 2 10))`,
        `(((lambda (x) (lambda (y) (+ x y))) 10) 20)`,
    ];

    testCode.forEach((src) => {
        let ast = Parser.parse(src);
        console.log('>>>', DEBUG.SHOW(ast));
        let i   = new Interpreter();
        let got = i.run( ast );
        console.log('<<<', DEBUG.SHOW(got));
        assert.strictEqual(got.type, 'NUM', '... got the expected type');
        assert.strictEqual(got.value, 30, '... got the expected value');
    })
})

test("... basic test", (t) => {
    let ast = Parser.parse(`(if (== 10 10) (* 2 10) (+ 2 10))`);
    console.log('>>>', DEBUG.SHOW(ast));
    let i   = new Interpreter();
    let got = i.run( ast );
    console.log('<<<', DEBUG.SHOW(got));
    assert.strictEqual(got.type, 'NUM', '... got the expected type');
    assert.strictEqual(got.value, 20, '... got the expected value');
})

test("... basic test", (t) => {
    let ast = Parser.parse(`(if (== 11 10) (* 2 10) (+ 2 10))`);
    console.log('>>>', DEBUG.SHOW(ast));
    let i   = new Interpreter();
    let got = i.run( ast );
    console.log('<<<', DEBUG.SHOW(got));
    assert.strictEqual(got.type, 'NUM', '... got the expected type');
    assert.strictEqual(got.value, 12, '... got the expected value');
})

