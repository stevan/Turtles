
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'
import * as ListUtil from './ListUtil'
import * as Parser   from './Parser'

import { Environment } from './Environment'

const DEBUG = true;
const LOG   = (msg : string, e : any = undefined) =>
    console.log(`LOG - ${msg} - `, e ? `${Parser.format(e)}` : '...' );

export const DUMP = (label : string, expr : AST.Expr, env : Environment) => {
    console.group(`-- ${label} `, '-'.repeat(80 - (label.length + 5)));
    console.log('%.ENV  : ', env.DUMP());
    console.log('@.EXPR : ', Parser.format(expr));
    console.groupEnd();
    console.log('-'.repeat(80));
}

export function evaluate (expr : AST.Expr, env : Environment) : AST.Expr {
    if (DEBUG) DUMP( 'TICK', expr, env );
    switch (true) {
    case TypeUtil.isCons(expr):
        if (DEBUG) LOG('Got CONS');
        let top = evaluate(ListUtil.head(expr), env);
        switch (true) {
        case TypeUtil.isCallable(top):
            if (DEBUG) LOG('*CALL*', top);
            return evaluateCall( top, ListUtil.tail(expr), env );
        default:
            if (DEBUG) LOG('*LIST*');
            return ASTUtil.Cons( top, evaluate(ListUtil.tail(expr), env) as AST.List );
        }
    case TypeUtil.isIdentifier(expr):
        if (DEBUG) LOG('Got Idenfifier = Var | Word | Special', expr);
        return env.lookup(expr);
    case TypeUtil.isLiteral(expr):
        if (DEBUG) LOG('Got Literal', expr);
        return expr;
    case TypeUtil.isNil(expr):
        if (DEBUG) LOG('()', expr);
        return expr;
    default:
        throw new Error('WTF!');
    }
}

function evaluateCall (top : AST.Callable, rest : AST.List, env : Environment) : AST.Expr {
    switch (true) {
    case TypeUtil.isFExpr(top):
        if (DEBUG) LOG('++ APPLY *FEXPR*', top);
        return top.body( ListUtil.flatten( rest ), env );
    case TypeUtil.isNative(top):
        if (DEBUG) LOG('++ APPLY *NATIVE*', top);
        return top.body( ListUtil.flatten( evaluate( rest, env ) as AST.List ) );
    case TypeUtil.isLambda(top):
        if (DEBUG) LOG('++ APPLY *LAMBDA*', top);
        let params = ListUtil.flatten(top.params);
        let args   = ListUtil.flatten(evaluate( rest, env ) as AST.List);
        let localE = env.derive();
        for (let i = 0; i < params.length; i++) {
            let param = params[i];
            let arg   = args[i];
            TypeUtil.assertIdentifier(param);
            TypeUtil.assertExpr(arg);
            localE.assign(param, arg);
        }
        return evaluate( top.body, localE );
    default:
        throw new Error(`Unknown Callable Type`);
    }
}

type Predicate = (lhs : string | number | boolean, rhs : string | number | boolean) => boolean
type NumBinOp  = (lhs : number, rhs : number) => number

function liftPredicate (pred : Predicate) : AST.Native {
    return ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (args : AST.Expr[]) : AST.Expr => {
            let [ lhs, rhs ] = args;
            TypeUtil.assertLiteral(lhs);
            TypeUtil.assertLiteral(rhs);
            return pred(lhs.value, rhs.value) ? ASTUtil.True() : ASTUtil.False();
        }
    )
}

function liftNumBinOp (binop : NumBinOp) : AST.Native {
    return ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (args : AST.Expr[]) : AST.Expr => {
            let [ lhs, rhs ] = args;
            TypeUtil.assertNum(lhs);
            TypeUtil.assertNum(rhs);
            return ASTUtil.Num( binop(lhs.value, rhs.value) );
        }
    )
}

export function createRootEnvironment () : Environment {
    let env = new Environment();

    env.assign( ASTUtil.Special('lambda'), ASTUtil.FExpr(
        ListUtil.create( ASTUtil.Var('params'), ASTUtil.Var('body') ),
        (args : AST.Expr[], env : Environment) : AST.Expr => {
            let [ params, body ] = args;
            TypeUtil.assertList(params);
            TypeUtil.assertList(body);
            return ASTUtil.Lambda( params, body );
        }
    ));

    env.assign( ASTUtil.Special('set!'), ASTUtil.FExpr(
        ListUtil.create( ASTUtil.Var('name'), ASTUtil.Var('value') ),
        (args : AST.Expr[], env : Environment) : AST.Expr => {
            let [ name, value ] = args;
            TypeUtil.assertIdentifier(name);
            TypeUtil.assertExpr(value);
            env.assign( name, evaluate( value, env ) );
            return ASTUtil.Nil();
        }
    ));

    env.assign( ASTUtil.Special('if'), ASTUtil.FExpr(
        ListUtil.create( ASTUtil.Var('cond'), ASTUtil.Var('then'), ASTUtil.Var('else') ),
        (args : AST.Expr[], env : Environment) : AST.Expr => {
            let [ cond, thenBranch, elseBranch ] = args;
            TypeUtil.assertCons(cond);
            TypeUtil.assertCons(thenBranch);
            TypeUtil.assertCons(elseBranch);
            let result = evaluate( cond, env );
            return evaluate( TypeUtil.isFalse(result) ? elseBranch : thenBranch, env );
        }
    ));

    env.assign( ASTUtil.Word('=='), liftPredicate((n, m) => n == m));
    env.assign( ASTUtil.Word('!='), liftPredicate((n, m) => n != m));

    env.assign( ASTUtil.Word('>'),  liftPredicate((n, m) => n >  m));
    env.assign( ASTUtil.Word('>='), liftPredicate((n, m) => n >= m));
    env.assign( ASTUtil.Word('<='), liftPredicate((n, m) => n <= m));
    env.assign( ASTUtil.Word('<'),  liftPredicate((n, m) => n <  m));

    env.assign( ASTUtil.Word('+'),  liftNumBinOp((n, m) => n + m));
    env.assign( ASTUtil.Word('-'),  liftNumBinOp((n, m) => n - m));
    env.assign( ASTUtil.Word('*'),  liftNumBinOp((n, m) => n * m));
    env.assign( ASTUtil.Word('/'),  liftNumBinOp((n, m) => n / m));
    env.assign( ASTUtil.Word('%'),  liftNumBinOp((n, m) => n % m));

    return env;
}
