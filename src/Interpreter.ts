
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

import { Env, MaybeEnv } from './Env'
import {
    DEBUG,
    createBaseEnvironment
} from './Runtime'

export class Interpreter {
    public env : Env;

    constructor (env? : MaybeEnv) {
        this.env = env ?? this.createRootEnvironment()
    }

    run (expr : Types.Expr) : Types.Expr {
        return this.evaluate(expr, this.env)
    }

    evaluate (expr : Types.Expr, env : Env) : Types.Expr {
        console.log(`%ENV `, DEBUG.DUMP(env));
        console.log(`EVAL (${expr.type})`, DEBUG.SHOW(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return expr;
        case 'SYM'   : return env.lookup(expr);
        case 'NIL'   : return expr;
        case 'CONS'  :
            let head = this.evaluate(expr.head, env);
            if (Util.Type.isCallable(head)) {
                return this.apply( head, expr.tail, env );
            } else {
                return AST.Cons( head, this.evaluate(expr.tail, env) as Types.List );
            }
        default:
            throw new Error('WTF!');
        }
    }

    // apply a function, builtin or fexpr
    apply (call : Types.Callable, args : Types.List, env : Env) : Types.Expr {
        console.log(`APPLY ${DEBUG.SHOW(call)} -> `, DEBUG.SHOW(args));

        const evalArgs     = () => Util.List.flatten( this.evaluate( args, env ) as Types.List );
        const makeLocalEnv = () => {
            let params = Util.List.flatten(call.params);
            let args   = evalArgs();
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                Util.Type.assertSym(param);
                // FIXME ...
                let arg = args[i] as Types.Expr;
                localE.assign( param, arg );
            }
            return localE;
        }

        switch (call.type) {
        case 'FEXPR':
            return call.body( Util.List.flatten( args ), env );
        case 'NATIVE':
            return call.body( makeLocalEnv() );
        case 'LAMBDA':
            return this.evaluate( call.body, makeLocalEnv() );
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        env.assign( AST.Sym('lambda'), AST.FExpr(
            Util.List.make( AST.Sym('params'), AST.Sym('body') ),
            (args : Types.Expr[], env : Env) : Types.Expr => {
                let [ params, body ] = args;
                Util.Type.assertList(params);
                Util.Type.assertList(body);
                return AST.Lambda( params as Types.List, body as Types.Expr, env );
            }
        ));

        env.assign( AST.Sym('if'), AST.FExpr(
            Util.List.make( AST.Sym('cond'), AST.Sym('then'), AST.Sym('else') ),
            (args : Types.Expr[], env : Env) : Types.Expr => {
                let [ cond, thenBranch, elseBranch ] = args;
                Util.Type.assertList(cond);
                Util.Type.assertList(thenBranch);
                Util.Type.assertList(elseBranch);
                let result = this.evaluate( cond as Types.Expr, env );
                Util.Type.assertBool(result);
                return this.evaluate( result.value ? elseBranch : thenBranch, env );
            }
        ));

        return env;
    }
}
