
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

const KSHOW = (k : Kontinue) : string => {
    switch (k.op) {
    case 'HALT'  : return `${k.op}()[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'JUST'  : return `${k.op}()[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'EVAL'  : return `${k.op}{${DEBUG.SHOW(k.expr)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'EHEAD' : return `${k.op}{${DEBUG.SHOW(k.cons)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'CALL?' : return `${k.op}{${DEBUG.SHOW(k.args)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'APPLY' : return `${k.op}{${DEBUG.SHOW(k.call)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    }
}

const KDUMP = (ctx : Context, queue : Kontinuation) => {
    console.log('-'.repeat(80));
    console.log(` %ENV :`, DEBUG.DUMP(ctx.env));
    console.log('QUEUE :', queue.map(KSHOW).join('; '));
    console.log('='.repeat(80));
}

const DEBUG_ON = true;

export class Machine {
    public rootEnv : Env;
    public rootCtx : Context;
    public stack   : Context[]    = [];
    public queue   : Kontinuation = [];

    constructor (env? : MaybeEnv) {
        this.rootEnv = env ?? this.createRootEnvironment();
        this.rootCtx = new Context(this.rootEnv.derive(), (expr) => this.run(expr));
        this.stack.push(this.rootCtx);
        this.queue.push(Halt());
    }

    // c(urrent)c(ontext)
    get cc () : Context {
        return this.stack.at(-1) as Context;
    }

    run (expr : Types.Expr) : Types.Expr {
        this.queue.push( Eval(expr) );

        let result;
        while (this.queue.length > 0) {
            let k = this.queue.pop() as Kontinue;

            if (!this.step(k)) {
                if (k.op != 'HALT') throw new Error('ONLY HALT!');
                if (DEBUG_ON) {
                console.log('!! HALT '+('_'.repeat(72)));
                                   KDUMP(this.cc, this.queue);}

                result = k.stack.shift() as Types.Expr;
                break;
            }

            if (DEBUG_ON) KDUMP(this.cc, this.queue);
        }

        return result ?? AST.Nil();
    }

    returnK (k : Kontinue) : void {
        let caller = this.queue.at(-1) as Kontinue;
        caller.stack.push( ...k.stack );
    }

    continueK (...next : Kontinuation) : void {
        this.queue.push( ...next );
    }

    step (k : Kontinue) : boolean {
        if (DEBUG_ON) {
        console.log('^ STEP :', KSHOW(k));
        console.log('.'.repeat(80));}

        switch (k.op) {
        case 'EVAL':
            this.continueK( this.evaluate( k.expr ) );
            if (k.stack.length > 0) throw new Error("WRFD!!!");
            return true;
        case 'EHEAD':
            this.continueK( Call( k.cons.tail ), Eval( k.cons.head ) );
            return true;
        case 'APPLY':
            this.continueK( this.apply( k.call, k.stack ) );
            return true;
        case 'CALL?':
            let [ call ] = k.stack;
            if (Util.Type.isCallable(call)) {
                this.continueK( Apply(call as Types.Callable), Eval(k.args) );
            } else {
                this.continueK( Just(call as Types.Expr), Eval(k.args) );
            }
            return true;
        case 'JUST':
            this.returnK( k );
            return true;
        case 'HALT':
            return false;
        default:
            throw new Error(`Unrecognized K op (${JSON.stringify(k)}`);
        }
    }


    evaluate (expr : Types.Expr) : Kontinue {
        console.log(`>> EVAL [${expr.type}]`, DEBUG.SHOW(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return Just(expr);
        case 'SYM'   : return Just(this.cc.env.lookup(expr));
        case 'CONS'  : return EHead(expr)
        case 'NIL'   : return Just(expr)
        default:
            throw new Error('FUCK!');
        }
    }

    apply (call : Types.Callable, args : Types.Expr[]) : Kontinue {
        console.log(`>> APPLY ${DEBUG.SHOW(call)} -> `, args.map(DEBUG.SHOW));

        const makeLocalEnv = () => {
            let params = Util.List.flatten(call.params);
            let localE = this.cc.env.derive();
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
