
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

    public pc : number = 0;

    constructor (env? : MaybeEnv) {
        this.rootEnv = env ?? this.createRootEnvironment();
        this.rootCtx = new Context(this.rootEnv.derive(), (expr) => this.evaluate(expr));
        this.stack.push(this.rootCtx);
    }

    // c(urrent)c(ontext)
    get cc () : Context {
        return this.stack.at(-1) as Context;
    }

    run (expr : Types.Expr) : Types.Expr {
        if (DEBUG_ON) {
        console.log('~~   RUN :', DEBUG.SHOW(expr));
        console.log('═'.repeat(80));}

        let result = this.cc.evaluate(expr);

        if (DEBUG_ON) {
        console.log(`HALT `, DEBUG.SHOW(result));
        console.log(`%ENV `, DEBUG.DUMP(this.cc.env));
        console.log('═'.repeat(80));}

        return result;
    }

    evaluate (expr : Types.Expr) : Types.Expr {
        if (DEBUG_ON) {
        console.log('─'.repeat(80));
        console.log(`EVAL.${(++this.pc).toString().padStart(3, '0')} (${expr.type})`, DEBUG.SHOW(expr));
        console.log('─'.repeat(80));
        console.log(`%ENV `, DEBUG.DUMP(this.cc.env));
        console.log('─'.repeat(80));}

        switch (expr.type) {
        // values
        case 'FEXPR':
        case 'NATIVE':
        case 'LAMBDA':
        // literals
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  :
        // and nil, ... all evaluate to themselves
        case 'NIL'   : return expr;
        case 'SYM'   : return this.cc.env.lookup(expr);
        case 'COND':
            let cond = this.cc.evaluate( expr.cond );
            Util.Type.assertBool(cond);
            return this.cc.evaluate( cond.value ? expr.ifTrue : expr.ifFalse );
        case 'CONS'  :
            let head = this.cc.evaluate(expr.head);
            if (Util.Type.isCallable(head)) {
                return this.apply( head, expr.tail );
            } else {
                return AST.Cons( head, this.cc.evaluate(expr.tail) as Types.List );
            }
        default:
            throw new Error('WTF!'+JSON.stringify(expr));
        }
    }

    // apply a function, builtin or fexpr
    apply (call : Types.Callable, args : Types.List) : Types.Expr {
        if (DEBUG_ON) {
        console.log(`APPLY ${DEBUG.SHOW(call)} -> `, DEBUG.SHOW(args));}

        const evaluateArgs = () : Types.List => this.cc.evaluate( args ) as Types.List;

        switch (call.type) {
        case 'FEXPR':
            // NOTE:
            // I am not sure if we should be evaluating the FExpr result
            // here or if it makes more sense to evaluate it inside the
            // FExpr body. I suspect it might make a difference in the
            // Machine implementation more, so we will see
            return this.cc.evaluate( call.body( Util.List.flatten( args ), this.cc ) );
        case 'NATIVE':
            return call.body( Util.List.flatten( evaluateArgs() ), this.cc );
        case 'LAMBDA':
            return this.callLambda( call, evaluateArgs() );
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    callLambda (call : Types.Lambda, args : Types.List) : Types.Expr {

        this.stack.push(new Context( call.env, this.cc.evaluate ));
        this.cc.enterScope( call.env );
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

        if (DEBUG_ON) {
        console.group(`>> CALLING LAMBDA ${DEBUG.SHOW(call)}`);
        console.log('─'.repeat(78));
        console.log(`   E -> `, DEBUG.DUMP(this.cc.env));
        console.log(`   e -> `, DEBUG.DUMP(call.env));
        console.log(`args -> `, DEBUG.SHOW(args));
        console.log('─'.repeat(78));
        console.groupEnd();}

        // evalute lambda in new scope
        let result = this.cc.evaluate( call.body );
        // ...
        this.cc.leaveScope();
        this.stack.pop();
        // return ...
        return result;
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        // ...

        return env;
    }
}
