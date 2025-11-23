
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

import { Context }       from './Context'
import { Env, MaybeEnv } from './Env'
import {
    DEBUG,
    createBaseEnvironment,
} from './Runtime'

const DEBUG_ON = false;

export class Interpreter {
    public rootEnv : Env;
    public rootCtx : Context;
    public stack   : Context[] = [];

    constructor (env? : MaybeEnv) {
        this.rootEnv = env ?? this.createRootEnvironment();
        this.rootCtx = new Context(this.rootEnv, (expr) => this.evaluate(expr));
        this.stack.push(this.rootCtx);
    }

    // c(urrent)c(ontext)
    get cc () : Context {
        return this.stack.at(-1) as Context;
    }

    run (expr : Types.Expr) : Types.Expr {
        let result = this.cc.evaluate(expr);

        if (DEBUG_ON) {
        console.log(`HALT `, DEBUG.SHOW(result));
        console.log(`%ENV `, DEBUG.DUMP(this.cc.env));}

        return result;
    }

    evaluate (expr : Types.Expr) : Types.Expr {
        if (DEBUG_ON) {
        console.log(`%ENV `, DEBUG.DUMP(this.cc.env));
        console.log(`EVAL (${expr.type})`, DEBUG.SHOW(expr));}

        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return expr;
        case 'SYM'   : return this.cc.env.lookup(expr);
        case 'NIL'   : return expr;
        case 'CONS'  :
            let head = this.cc.evaluate(expr.head);
            if (Util.Type.isCallable(head)) {
                return this.apply( head, expr.tail );
            } else {
                return AST.Cons( head, this.cc.evaluate(expr.tail) as Types.List );
            }
        default:
            throw new Error('WTF!');
        }
    }

    // apply a function, builtin or fexpr
    apply (call : Types.Callable, args : Types.List) : Types.Expr {
        if (DEBUG_ON) {
        console.log(`APPLY ${DEBUG.SHOW(call)} -> `, DEBUG.SHOW(args));}

        const evaluateArgs = () : Types.List => this.cc.evaluate( args ) as Types.List;

        switch (call.type) {
        case 'FEXPR':
            return call.body( Util.List.flatten( args ), this.cc );
        case 'NATIVE':
            return call.body( Util.List.flatten( evaluateArgs() ), this.cc );
        case 'LAMBDA':
            return this.callLambda( call, evaluateArgs() );
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    enterContext (ctx : Context) : void {
        this.stack.push(ctx);
    }

    leaveContext () : void {
        this.stack.pop();
    }

    callLambda (call : Types.Lambda, args : Types.List) : Types.Expr {
        this.enterContext( call.ctx );
        // enter new scope
        this.cc.enterScope();
        // ...
        let flatArgs = Util.List.flatten( args );
        let params   = Util.List.flatten( call.params );
        for (let i = 0; i < params.length; i++) {
            let param = params[i];
            Util.Type.assertSym(param);
            // FIXME ...
            let arg = flatArgs[i] as Types.Expr;
            this.cc.env.assign( param, arg );
        }
        // evalute lambda in new scope
        let result = this.cc.evaluate( call.body );
        // leave scope
        this.cc.leaveScope();
        // ...
        this.leaveContext();
        // return ...
        return result;
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        env.assign( AST.Sym('lambda'), AST.FExpr(
            Util.List.make( AST.Sym('params'), AST.Sym('body') ),
            (args : Types.Expr[], ctx : Context) : Types.Expr => {
                let [ params, body ] = args;
                Util.Type.assertList(params);
                Util.Type.assertList(body);
                return AST.Lambda( params as Types.List, body as Types.Expr, ctx );
            }
        ));

        env.assign( AST.Sym('if'), AST.FExpr(
            Util.List.make( AST.Sym('cond'), AST.Sym('then'), AST.Sym('else') ),
            (args : Types.Expr[], ctx : Context) : Types.Expr => {
                let [ cond, thenBranch, elseBranch ] = args;
                Util.Type.assertList(cond);
                Util.Type.assertList(thenBranch);
                Util.Type.assertList(elseBranch);
                let result = ctx.evaluate( cond as Types.Expr );
                Util.Type.assertBool(result);
                return ctx.evaluate( result.value ? thenBranch : elseBranch );
            }
        ));

        return env;
    }
}
