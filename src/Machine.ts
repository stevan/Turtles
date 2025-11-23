
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

import { Context }       from './Context'
import { Env, MaybeEnv } from './Env'
import {
    DEBUG,
    createBaseEnvironment
} from './Runtime'

type Halt   = { op : 'HALT',  stack : Types.Expr[] }
type Just   = { op : 'JUST',  stack : Types.Expr[] }
type Eval   = { op : 'EVAL',  stack : Types.Expr[], expr : Types.Expr }
type EHead  = { op : 'EHEAD', stack : Types.Expr[], cons : Types.Cons }
type Call   = { op : 'CALL?', stack : Types.Expr[], args : Types.List }
type Apply  = { op : 'APPLY', stack : Types.Expr[], call : Types.Callable }

function Halt   ()                      : Halt  { return { op : 'HALT',  stack : [] } }
function Just   (just : Types.Expr)     : Just  { return { op : 'JUST',  stack : [ just ] } }
function Eval   (expr : Types.Expr)     : Eval  { return { op : 'EVAL',  stack : [], expr } }
function EHead  (cons : Types.Cons)     : EHead { return { op : 'EHEAD', stack : [], cons } }
function Call   (args : Types.List)     : Call  { return { op : 'CALL?', stack : [], args } }
function Apply  (call : Types.Callable) : Apply { return { op : 'APPLY', stack : [], call } }

type Kontinue     = Halt | Just | Eval | EHead| Call | Apply
type Kontinuation = Kontinue[];

DEBUG.SHOWK = (k : Kontinue) : string => {
    switch (k.op) {
    case 'HALT'  : return `${k.op}()[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'JUST'  : return `${k.op}()[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'EVAL'  : return `${k.op}<${DEBUG.SHOW(k.expr)}>[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'EHEAD' : return `${k.op}<${DEBUG.SHOW(k.cons)}>[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'CALL?' : return `${k.op}<${DEBUG.SHOW(k.args)}>[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'APPLY' : return `${k.op}<${DEBUG.SHOW(k.call)}>[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    }
}

const DEBUG_ON = false;

export class Machine {
    public rootEnv : Env;
    public rootCtx : Context;
    public stack   : Context[] = [];

    constructor (env? : MaybeEnv) {
        this.rootEnv = env ?? this.createRootEnvironment();
        this.rootCtx = new Context(this.rootEnv, (expr) => this.run(expr));
        this.stack.push(this.rootCtx);
    }

    // c(urrent)c(ontext)
    get cc () : Context {
        return this.stack.at(-1) as Context;
    }

    run (expr : Types.Expr) : Types.Expr {

        let queue : Kontinuation = [ Halt(), Eval(expr) ];

        let result;

        while (queue.length > 0) {
            console.log('__ TICK '+('_'.repeat(72)));

            let operation = queue.pop() as Kontinue;

            if (this.step(operation, this.cc.env, queue)) {
                console.log('='.repeat(80));
                console.log(` %ENV :`, DEBUG.DUMP(this.cc.env));
                console.log('QUEUE :', queue.map((k) => `${k.op}[${
                    k.stack.map(DEBUG.SHOW).join(', ')
                }]`).join('; '));
                console.log('-'.repeat(80));
            } else {
                if (operation.op != 'HALT') {
                    throw new Error('ONLY HALT!');
                }

                console.log('!! HALT '+('_'.repeat(72)));
                console.log(` %ENV :`, DEBUG.DUMP(this.cc.env));
                console.log('QUEUE :', queue.map((k) => `${k.op}[${
                    k.stack.map(DEBUG.SHOW).join(', ')
                }]`).join('; '));
                console.log('-'.repeat(80));

                result = operation.stack.shift() as Types.Expr;
                break;
            }
        }

        return result ?? AST.Nil();
    }

    step (k : Kontinue, env : Env, queue : Kontinuation) : boolean {
        let caller = queue.at(-1) as Kontinue;

        switch (k.op) {
        case 'EVAL':
            queue.push( this.evaluate( k.expr, env ) );
            //if (k.stack.length > 0) queue.push(Just(rest));
            return true;
        case 'EHEAD':
            queue.push( Call( k.cons.tail ), Eval( k.cons.head ) );
            return true;
        case 'APPLY':
            queue.push( this.apply( k.call, k.stack, env ) );
            return true;
        case 'CALL?':
            let [ call ] = k.stack;
            if (Util.Type.isCallable(call)) {
                queue.push( Apply(call as Types.Callable), Eval(k.args) );
            } else {
                queue.push( Just(call as Types.Expr), Eval(k.args) );
            }
            return true;
        case 'JUST':
            caller.stack.push( ...k.stack );
            return true;
        case 'HALT':
            return false;
        default:
            throw new Error(`Unrecognized K op (${JSON.stringify(k)}`);
        }
    }


    evaluate (expr : Types.Expr, env : Env) : Kontinue {
        console.log(`>> EVAL [${expr.type}]`, DEBUG.SHOW(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return Just(expr);
        case 'SYM'   : return Just(env.lookup(expr));
        case 'CONS'  : return EHead(expr)
        case 'NIL'   : return Just(expr)
        default:
            throw new Error('FUCK!');
        }
    }

    apply (call : Types.Callable, args : Types.Expr[], env : Env) : Kontinue {
        console.log(`>> APPLY ${DEBUG.SHOW(call)} -> `, args.map(DEBUG.SHOW));

        const makeLocalEnv = () => {
            let params = Util.List.flatten(call.params);
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                Util.Type.assertSym(param);
                if (arg == undefined) throw new Error('BAD ARG!');
                localE.assign( param, arg as Types.Expr );
            }
            console.log(`(local) %ENV :`, DEBUG.DUMP(localE));
            return localE;
        }

        switch (call.type) {
        case 'FEXPR':
            return Eval(call.body( args, this.cc ));
        case 'NATIVE':
            return Just(call.body( args, this.cc ));
        case 'LAMBDA':
            return Eval(call.body); // BROKEN!
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        // TODO

        return env;
    }
}
