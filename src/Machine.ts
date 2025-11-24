
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
type Call   = { op : 'CALL?', stack : Types.Expr[], args : Types.List }
type Apply  = { op : 'APPLY', stack : Types.Expr[], call : Types.Callable }
type Choice = { op : 'CHEWZ', stack : Types.Expr[], cond : Types.Cond }
type Enter  = { op : 'ECTX',  stack : Types.Expr[], call : Types.Lambda }
type Bind   = { op : 'BIND',  stack : Types.Expr[], params : Types.List }
type Leave  = { op : 'LCTX',  stack : Types.Expr[] }

function Eval   (expr : Types.Expr)     : Eval   { return { op : 'EVAL',  stack : [], expr } }
function Call   (args : Types.List)     : Call   { return { op : 'CALL?', stack : [], args } }
function Apply  (call : Types.Callable) : Apply  { return { op : 'APPLY', stack : [], call } }
function Just   (...j : Types.Expr[])   : Just   { return { op : 'JUST',  stack : [ ...j ] } }
function Halt   ()                      : Halt   { return { op : 'HALT',  stack : [] } }
function Leave  ()                      : Leave  { return { op : 'LCTX',  stack : [] } }
function Choice (cond : Types.Cond)     : Choice { return { op : 'CHEWZ', stack : [], cond } }
function Enter  (call : Types.Lambda)   : Enter  { return { op : 'ECTX',  stack : [], call } }
function Bind   (params : Types.List, args : Types.Expr[]) : Bind  {
    return { op : 'BIND', stack : [ ...args ], params }
}

type Kontinue =
    | Halt
    | Just
    | Eval
    | Call
    | Apply
    | Enter
    | Leave
    | Bind
    | Choice

type Kontinuation = Kontinue[];

const KSHOW = (k : Kontinue) : string => {
    switch (k.op) {
    case 'HALT'  : return `${k.op}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'JUST'  : return `${k.op}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'EVAL'  : return `${k.op}{${DEBUG.SHOW(k.expr)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'CALL?' : return `${k.op}{${DEBUG.SHOW(k.args)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'APPLY' : return `${k.op}{${DEBUG.SHOW(k.call)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'ECTX'  : return `${k.op}{${DEBUG.SHOW(k.call)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'BIND'  : return `${k.op}{${DEBUG.SHOW(k.params)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'CHEWZ' : return `${k.op}{${DEBUG.SHOW(k.cond)}}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    case 'LCTX'  : return `${k.op}:[${k.stack.map(DEBUG.SHOW).join(', ')}]`
    }
}

const KDUMP = (ctx : Context, queue : Kontinuation) => {
    console.log('─'.repeat(80));
    console.log('QUEUE :\n  ◇', queue.map(KSHOW).reverse().join(';\n  ◆ '));
    console.log('─'.repeat(80));
    console.log(` %ENV :`, DEBUG.DUMP(ctx.env));
    console.log('═'.repeat(80));
}

const DEBUG_DEEP = false;
const DEBUG_BASE = DEBUG_DEEP || false;

export class Machine {
    public rootEnv : Env;
    public rootCtx : Context;
    public stack   : Context[]    = [];
    public queue   : Kontinuation = [];

    public pc : number = 0;

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
        if (DEBUG_DEEP) {
            console.log('~~   RUN :', DEBUG.SHOW(expr));
            console.log('═'.repeat(80));}

        this.queue.push( Eval(expr) );

        let result : Types.Expr = AST.Nil();
        while (this.queue.length > 0) {
            let k = this.queue.pop() as Kontinue;
            if (!this.step(k)) {
                result = k.stack.shift() as Types.Expr;
                break;
            }
            if (DEBUG_DEEP) KDUMP(this.cc, this.queue);
        }

        if (DEBUG_DEEP) {
            console.log('!!  HALT :' + DEBUG.SHOW(result));
            console.log('═'.repeat(80));}

        return result;
    }

    returnK (k : Kontinue) : void {
        let caller = this.queue.at(-1) as Kontinue;
        caller.stack.push( ...k.stack );
    }

    continueK (...next : Kontinuation) : void {
        this.queue.push( ...next );
    }

    step (k : Kontinue) : boolean {
        if (DEBUG_DEEP) {
            console.log('─'.repeat(80));
            console.log('^STEP.'+(++this.pc).toString().padStart(3, '0')+':');
            console.log('─'.repeat(80));
            console.group('○', KSHOW(k));
            console.log('─'.repeat(78));}

        switch (k.op) {
        case 'EVAL':
            this.continueK( ...this.evaluate( k.expr ), Just(...k.stack) );
            break;
        case 'APPLY':
            this.continueK( ...this.apply( k.call, k.stack ) );
            break;
        case 'CHEWZ':
            let [ cond ] = k.stack;
            Util.Type.assertBool(cond);
            this.continueK( cond.value ? Eval(k.cond.ifTrue) : Eval(k.cond.ifFalse) );
            break;
        case 'CALL?':
            let [ call ] = k.stack;
            if (Util.Type.isCallable(call)) {
                switch (call.type) {
                case 'FEXPR':
                    this.continueK( Apply(call), Just(k.args) );
                    break;
                case 'NATIVE':
                case 'LAMBDA':
                    this.continueK( Apply(call), Eval(k.args) );
                    break;
                default:
                    throw new Error(`Unknown Callable Type`);
                }
            } else {
                this.continueK( Just(call as Types.Expr) );
                if (Util.Type.isNil(k.args)) break;
                this.continueK( Eval(k.args) );
            }
            break;
        case 'ECTX':
            this.enterContext( k.call )
            break;
        case 'LCTX':
            this.leaveContext()
            this.returnK( k );
            break;
        case 'BIND':
            this.bindParams( k.params, k.stack );
            break;
        case 'JUST':
            this.returnK( k );
            break;
        case 'HALT':
            if (DEBUG_DEEP) console.groupEnd();
            return false;
        default:
            throw new Error(`Unrecognized K op (${JSON.stringify(k)}`);
        }

        if (DEBUG_DEEP) console.groupEnd();
        return true;
    }


    evaluate (expr : Types.Expr) : Kontinuation {
        if (DEBUG_BASE) console.log(`>>  eval : [${expr.type}]`, DEBUG.SHOW(expr));
        switch (expr.type) {
        // callables
        case 'FEXPR' :
        case 'NATIVE':
        case 'LAMBDA':
        // literals
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  :
        // and nil, ... all evaluate to themselves
        case 'NIL'   : return [ Just(expr)                       ];
        case 'SYM'   : return [ Just(this.cc.env.lookup(expr))   ];
        case 'CONS'  : return [ Call(expr.tail), Eval(expr.head) ];
        case 'COND'  : return [ Choice(expr), Eval(expr.cond)    ];
        default:
            throw new Error('FUCK!');
        }
    }

    apply (call : Types.Callable, args : Types.Expr[]) : Kontinuation {
        if (DEBUG_BASE) console.log(`&& apply : ${DEBUG.SHOW(call)} -> `, args.map(DEBUG.SHOW));
        switch (call.type) {
        case 'FEXPR':
            return [ Eval(call.body( Util.List.flatten(args[0] as Types.List), this.cc )) ];
        case 'NATIVE':
            return [ Just(call.body( args, this.cc )) ];
        case 'LAMBDA':
            if (DEBUG_DEEP) {
            console.group('─'.repeat(78));
            console.log(`>> CALLING LAMBDA ${DEBUG.SHOW(call)}`);
            console.log('─'.repeat(78));
            console.log(`   E -> `, DEBUG.DUMP(this.cc.env));
            console.log(`   e -> `, DEBUG.DUMP(call.env));
            console.log(`args -> `, args.map(DEBUG.SHOW).join(', '));
            console.log('─'.repeat(78));
            console.groupEnd();}
            return [
                Leave(),
                Eval( call.body ),
                Bind( call.params, args ),
                Enter( call ),
            ];
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    enterContext (call : Types.Lambda) : void {
        this.stack.push(new Context( call.env, this.cc.evaluate ));
        this.cc.enterScope( call.env );
    }

    leaveContext () : void {
        this.cc.leaveScope();
        this.stack.pop();
    }

    bindParams (params : Types.List, args : Types.Expr[]) : void {
        let flatParams = Util.List.flatten( params );
        for (let i = 0; i < flatParams.length; i++) {
            let param = flatParams[i];
            Util.Type.assertSym(param);
            // FIXME ...
            let arg = args[i] as Types.Expr;
            this.cc.env.assign( param, arg );
        }
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        env.assign( AST.Sym('lambda'), AST.FExpr(
            Util.List.make( AST.Sym('params'), AST.Sym('body') ),
            (args : Types.Expr[], ctx : Context) : Types.Expr => {
                let [ params, body ] = args;
                Util.Type.assertList(params);
                Util.Type.assertList(body);
                if (DEBUG_DEEP) {
                console.log("... creating Lambda with", DEBUG.DUMP(ctx.env));}
                return AST.Lambda( params as Types.List, body as Types.Expr, ctx.env );
            }
        ));

        return env;
    }
}
