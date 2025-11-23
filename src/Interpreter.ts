
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

import { Context }       from './Context'
import { Env, MaybeEnv } from './Env'
import {
    DEBUG,
    createBaseEnvironment,
} from './Runtime'

export class Interpreter {
    public rootEnv : Env;
    public context : Context;

    constructor (env? : MaybeEnv) {
        this.rootEnv = env ?? this.createRootEnvironment();
        this.context = new Context(this.rootEnv, (expr, env) => this.evaluate(expr, env));
    }

    run (expr : Types.Expr) : Types.Expr {
        return this.evaluate(expr, this.context);
    }

    evaluate (expr : Types.Expr, ctx : Context) : Types.Expr {
        console.log(`%ENV `, DEBUG.DUMP(ctx.env));
        console.log(`EVAL (${expr.type})`, DEBUG.SHOW(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return expr;
        case 'SYM'   : return ctx.env.lookup(expr);
        case 'NIL'   : return expr;
        case 'CONS'  :
            let head = ctx.evaluate(expr.head);
            if (Util.Type.isCallable(head)) {
                return this.apply( head, expr.tail, ctx );
            } else {
                return AST.Cons( head, ctx.evaluate(expr.tail) as Types.List );
            }
        default:
            throw new Error('WTF!');
        }
    }

    // apply a function, builtin or fexpr
    apply (call : Types.Callable, args : Types.List, ctx : Context) : Types.Expr {
        console.log(`APPLY ${DEBUG.SHOW(call)} -> `, DEBUG.SHOW(args));

        const evalArgs = () => Util.List.flatten( ctx.evaluate( args ) as Types.List );
        const bindArgs = () => {
            let params = Util.List.flatten(call.params);
            let args   = evalArgs();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                Util.Type.assertSym(param);
                // FIXME ...
                let arg = args[i] as Types.Expr;
                ctx.env.assign( param, arg );
            }
            return ctx;
        }

        ctx.enterScope();

        let result;
        switch (call.type) {
        case 'FEXPR':
            result = call.body( Util.List.flatten( args ), ctx );
            break;
        case 'NATIVE':
            result = call.body( bindArgs() );
            break;
        case 'LAMBDA':
            result = this.evaluate( call.body, bindArgs() );
            break;
        default:
            throw new Error(`Unknown Callable Type`);
        }

        ctx.leaveScope();

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
                return ctx.evaluate( result.value ? elseBranch : thenBranch );
            }
        ));

        return env;
    }
}
