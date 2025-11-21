
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'
import * as ListUtil from './ListUtil'
import * as Parser   from './Parser'

import { Environment, MaybeEnvironment } from './Environment'


// -----------------------------------------------------------------------------
// Values
// -----------------------------------------------------------------------------

type Closure = {
    type : 'CLOSURE',
    env  : Environment,
    proc : AST.Callable
}

type Result = { type : 'RESULT', env : Environment, value : AST.Expr }

type Value = Closure | Result

function isClosure (v : any) : v is Closure { return v.type == 'CLOSURE' }
function isResult  (v : any) : v is Result  { return v.type == 'RESULT' }
function isValue   (v : any) : v is Value   { return isClosure(v) || isResult(v) }

function assertValue   (v : any) : asserts v is Value   { if (!isValue(v))   throw new Error("Not Value")   }
function assertResult  (v : any) : asserts v is Result  { if (!isResult(v))  throw new Error("Not Result")  }
function assertClosure (v : any) : asserts v is Closure { if (!isClosure(v)) throw new Error("Not Closure") }

function Closure (env : Environment, proc : AST.Callable) : Closure {
    return { type : 'CLOSURE', env, proc }
}

function Result (env : Environment, value : AST.Expr) : Result {
    return { type : 'RESULT', env, value }
}

// -----------------------------------------------------------------------------
// Ops
// -----------------------------------------------------------------------------

type Halt = { op : 'HALT' }
type Just = { op : 'JUST', expr : AST.Expr }
type Lkup = { op : 'LKUP', expr : AST.Identifier }
type Bind = { op : 'BIND', expr : AST.Identifier, value : AST.Expr }
type Call = { op : 'CALL', closure : Closure, args : AST.List };
type Eval = { op : 'EVAL', expr : AST.List };

type Frame = Halt | Just | Lkup | Bind | Call | Eval

function isHalt (v : any) : v is Halt { return v.op == 'HALT' }
function isJust (v : any) : v is Just { return v.op == 'JUST' }
function isLkup (v : any) : v is Lkup { return v.op == 'LKUP' }
function isBind (v : any) : v is Bind { return v.op == 'BIND' }
function isCall (v : any) : v is Call { return v.op == 'CALL' }
function isEval (v : any) : v is Eval { return v.op == 'EVAL' }

function isFrame (v : any) : v is Frame {
    return isHalt(v) || isJust(v) || isLkup(v) || isBind(v) || isCall(v) || isEval(v)
}

function assertHalt  (v : any) : asserts v is Halt  { if (!isHalt(v))  throw new Error("Not Halt!")  }
function assertJust  (v : any) : asserts v is Just  { if (!isJust(v))  throw new Error("Not Just!")  }
function assertLkup  (v : any) : asserts v is Lkup  { if (!isLkup(v))  throw new Error("Not Lkup!")  }
function assertBind  (v : any) : asserts v is Bind  { if (!isBind(v))  throw new Error("Not Bind!")  }
function assertCall  (v : any) : asserts v is Call  { if (!isCall(v))  throw new Error("Not Call!")  }
function assertEval  (v : any) : asserts v is Eval  { if (!isEval(v))  throw new Error("Not Eval!")  }
function assertFrame (v : any) : asserts v is Frame { if (!isFrame(v)) throw new Error("Not Frame!") }

function Halt ()                                        : Halt { return { op : 'HALT' }                }
function Just (expr : AST.Expr)                         : Just { return { op : 'JUST', expr }          }
function Lkup (expr : AST.Identifier)                   : Lkup { return { op : 'LKUP', expr }          }
function Bind (expr : AST.Identifier, value : AST.Expr) : Bind { return { op : 'BIND', expr, value }   }
function Call (closure : Closure, args : AST.List)      : Call { return { op : 'CALL', closure, args } }
function Eval (expr : AST.List)                         : Eval { return { op : 'EVAL', expr }          }

// -----------------------------------------------------------------------------
// Continuations
// -----------------------------------------------------------------------------

type Kontinuation = Frame

// Welcome to the Machine

export class Machine {

    run (expr : AST.Expr, env? : MaybeEnvironment) : AST.Expr {
        // TODO - handle errors
        let result = this.evaluate(
            expr,
            env ?? this.createRootEnvironment()
        );

        return result;
    }

    // Expression evaluator
    evaluate (expr : AST.Expr, env : Environment) : AST.Expr {
        if (DEBUG) DUMP( 'TICK', expr, env );
        switch (true) {
        case TypeUtil.isCons(expr):
            if (DEBUG) LOG('Got CONS');
            return this.kontinue(Eval(expr), env);
        case TypeUtil.isIdentifier(expr):
            if (DEBUG) LOG('Got Idenfifier = Var | Word | Special', expr);
            return this.kontinue(Lkup(expr), env)
        case TypeUtil.isLiteral(expr):
            if (DEBUG) LOG('Got Literal', expr);
            return this.kontinue(Just(expr), env);
        case TypeUtil.isNil(expr):
            if (DEBUG) LOG('()', expr);
            // XXX - not sure if this is correct ...
            return this.kontinue(Just(expr), env);
        default:
            throw new Error('WTF!');
        }
    }

    // what to do next ...
    kontinue (k : Kontinuation, env : Environment) : AST.Expr {
        switch (k.op) {
        case 'JUST': return k.expr;
        case 'LKUP': return env.lookup(k.expr);
        case 'EVAL':
            let top = this.evaluate( ListUtil.head(k.expr), env );
            switch (true) {
            case TypeUtil.isCallable(top):
                if (DEBUG) LOG('*CALL*', top);
                return this.apply( top, ListUtil.tail(k.expr), env )
            default:
                if (DEBUG) LOG('*LIST*');
                return ASTUtil.Cons( top, this.evaluate( ListUtil.tail(k.expr), env ) as AST.List );
            }
        case 'CALL':
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
