
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'
import * as ListUtil from './ListUtil'
import * as Parser   from './Parser'

import { Environment } from './Environment'

// -----------------------------------------------------------------------------
// Runtime
// -----------------------------------------------------------------------------

type Kontinuation =
    | { op : 'HALT', expr : AST.Expr }

    | { op : 'JUST', expr : AST.Expr }
    | { op : 'LKUP', expr : AST.Identifier }

    | { op : 'CALL', call : AST.Callable, args : AST.List }

    | { op : 'EVAL_HEAD', expr : AST.Cons }
    | { op : 'EVAL_TAIL', expr : AST.Cons }

export class Machine {

    step (expr : AST.Expr, env : Environment, kont : Kontinuation) : boolean {
        return false;
    }

    // Expression evaluator
    evaluate (expr : AST.Expr, env : Environment) : AST.Expr {
        if (DEBUG) DUMP( 'TICK', expr, env );
        switch (true) {
        case TypeUtil.isCons(expr):
            if (DEBUG) LOG('Got CONS');
            return this.kontinue({ op : 'EVAL_HEAD', expr }, env);
        case TypeUtil.isIdentifier(expr):
            if (DEBUG) LOG('Got Idenfifier = Var | Word | Special', expr);
            return this.kontinue({ op : 'LKUP', expr }, env)
        case TypeUtil.isLiteral(expr):
            if (DEBUG) LOG('Got Literal', expr);
            return this.kontinue({ op : 'JUST', expr }, env);
        case TypeUtil.isNil(expr):
            if (DEBUG) LOG('()', expr);
            // XXX - not sure if this is correct ...
            return this.kontinue({ op : 'JUST', expr }, env);
        default:
            throw new Error('WTF!');
        }
    }

    // what to do next ...
    kontinue (k : Kontinuation, env : Environment) : AST.Expr {
        switch (k.op) {
        case 'JUST': return k.expr;
        case 'LKUP': return env.lookup(k.expr);

        case 'CALL': return this.apply( k.call, k.args, env );

        case 'EVAL_HEAD':
            let top = this.evaluate( ListUtil.head(k.expr), env );
            switch (true) {
            case TypeUtil.isCallable(top):
                if (DEBUG) LOG('*CALL*', top);
                // collect args ... THEN call
                return this.kontinue({ op : 'CALL', call : top, args : ListUtil.tail(k.expr) }, env)
            default:
                if (DEBUG) LOG('*LIST*');
                // collect rest of list ...
                return ASTUtil.Cons( top, this.evaluate( ListUtil.tail(k.expr), env ) as AST.List );
            }
        case 'EVAL_TAIL':
            throw new Error('TODO');
        default:
            throw new Error(`Unknown Kontinuation type`);
        }
    }

    // apply a function, builtin or fexpr
    apply (top : AST.Callable, rest : AST.List, env : Environment) : AST.Expr {

        const evalArgs = () => ListUtil.flatten(this.evaluate( rest, env ) as AST.List);

        const makeLocalEnv = () => {
            let params = ListUtil.flatten(top.params);
            let args   = evalArgs();
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                TypeUtil.assertIdentifier(param);
                TypeUtil.assertExpr(arg);
                localE.assign(param, arg);
            }
            return localE;
        }

        switch (true) {
        case TypeUtil.isFExpr(top):
            if (DEBUG) LOG('++ APPLY *FEXPR*', top);
            return top.body( ListUtil.flatten( rest ), env );
        case TypeUtil.isNative(top):
            if (DEBUG) LOG('++ APPLY *NATIVE*', top);
            return top.body( makeLocalEnv() );
        case TypeUtil.isLambda(top):
            if (DEBUG) LOG('++ APPLY *LAMBDA*', top);
            return this.evaluate( top.body, makeLocalEnv() );
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    // root environment
    createRootEnvironment () : Environment {
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
                env.assign( name, this.evaluate( value, env ) );
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
                let result = this.evaluate( cond, env );
                return this.evaluate( TypeUtil.isFalse(result) ? elseBranch : thenBranch, env );
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

}

// -----------------------------------------------------------------------------
// Debugging
// -----------------------------------------------------------------------------

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

// -----------------------------------------------------------------------------
// Builtin Helpers
// -----------------------------------------------------------------------------

type Predicate = (lhs : string | number | boolean, rhs : string | number | boolean) => boolean
type NumBinOp  = (lhs : number, rhs : number) => number

function liftPredicate (pred : Predicate) : AST.Native {
    return ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (env: Environment) : AST.Expr => {
            let lhs = env.lookup(ASTUtil.Var('n'));
            let rhs = env.lookup(ASTUtil.Var('m'));
            TypeUtil.assertLiteral(lhs);
            TypeUtil.assertLiteral(rhs);
            return pred(lhs.value, rhs.value) ? ASTUtil.True() : ASTUtil.False();
        }
    )
}

function liftNumBinOp (binop : NumBinOp) : AST.Native {
    return ASTUtil.Native(
        ListUtil.create( ASTUtil.Var('n'), ASTUtil.Var('m') ),
        (env: Environment) : AST.Expr => {
            let lhs = env.lookup(ASTUtil.Var('n'));
            let rhs = env.lookup(ASTUtil.Var('m'));
            TypeUtil.assertNum(lhs);
            TypeUtil.assertNum(rhs);
            return ASTUtil.Num( binop(lhs.value, rhs.value) );
        }
    )
}
