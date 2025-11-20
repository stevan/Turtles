
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'
import * as ListUtil from './ListUtil'
import * as Parser   from './Parser'

import { Environment } from './Environment'

const DEBUG = true;
const LOG   = (d : number, msg : string, e : any = undefined) =>
    console.log(`LOG[ ${d.toString().padStart(2, '0')} ] ${msg} - `, e ? `${Parser.format(e)}` : '...' );

export const DUMP = (label : string, expr : AST.Expr, env : Environment) => {
    console.group(`-- ${label} `, '-'.repeat(80 - (label.length + 5)));
    console.log('%.ENV  : ', env.DUMP());
    console.log('@.EXPR : ', Parser.format(expr));
    console.groupEnd();
    console.log('-'.repeat(80));
}

export function evaluate (expr : AST.Expr, env : Environment, depth : number = 0) : AST.Expr {
    if (DEBUG) DUMP( 'TICK', expr, env );
    switch (true) {
    case TypeUtil.isCons(expr):
        if (DEBUG) LOG(depth, 'Got CONS');
        let top = evaluate(ListUtil.head(expr), env, depth + 1);
        if (DEBUG) LOG(depth, 'EVAL(h)?', top);
        switch (true) {
        case TypeUtil.isFExpr(top):
            if (DEBUG) LOG(depth, '++ APPLY *FEXPR*', top);
            return top.body( ListUtil.flatten( ListUtil.tail(expr) ), env );
        case TypeUtil.isNative(top):
            if (DEBUG) LOG(depth, '++ APPLY *NATIVE*', top);
            return top.body( ListUtil.flatten( evaluate(ListUtil.tail(expr), env, depth + 1) as AST.List ) );
        case TypeUtil.isLambda(top):
            if (DEBUG) LOG(depth, '++ APPLY *LAMBDA*', top);
            let params = ListUtil.flatten(top.params);
            let args   = ListUtil.flatten(evaluate(ListUtil.tail(expr), env, depth + 1) as AST.List);
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
            if (DEBUG) LOG(depth, '*LIST*');
            return ASTUtil.Cons( top, evaluate(ListUtil.tail(expr), env, depth + 1) as AST.List );
        }
    case TypeUtil.isIdentifier(expr):
        if (DEBUG) LOG(depth, 'Got Idenfifier = Var | Word | Special', expr);
        return env.lookup(expr);
    case TypeUtil.isLiteral(expr):
        if (DEBUG) LOG(depth, 'Got Literal', expr);
        return expr;
    case TypeUtil.isNil(expr):
        if (DEBUG) LOG(depth, '()', expr);
        return expr;
    default:
        throw new Error('WTF!');
    }
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

    env.assign( ASTUtil.Word('=='), ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (args : AST.Expr[]) : AST.Expr => {
            let [ lhs, rhs ] = args;
            TypeUtil.assertLiteral(lhs);
            TypeUtil.assertLiteral(rhs);
            return (lhs.value == rhs.value) ? ASTUtil.True() : ASTUtil.False();
        }
    ));

    env.assign( ASTUtil.Word('+'), ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (args : AST.Expr[]) : AST.Expr => {
            let [ lhs, rhs ] = args;
            TypeUtil.assertInt(lhs);
            TypeUtil.assertInt(rhs);
            return ASTUtil.Int(lhs.value + rhs.value);
        }
    ));

    env.assign( ASTUtil.Word('*'), ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (args : AST.Expr[]) : AST.Expr => {
            let [ lhs, rhs ] = args;
            TypeUtil.assertInt(lhs);
            TypeUtil.assertInt(rhs);
            return ASTUtil.Int(lhs.value * rhs.value);
        }
    ));

    return env;
}
