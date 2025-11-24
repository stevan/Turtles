
import * as Types  from './Types'
import * as AST    from './AST'
import * as Util   from './Util'
import * as Parser from './Parser'

import { Context }       from './Context'
import { Env, MaybeEnv } from './Env'

export namespace DEBUG {
    export function SHOW (expr : Types.Expr) : string {
        return Parser.format(expr)
    }

    export function DUMP (env : Env) : string {
        return `%E[${env.depth()}]{${ [ ...env.bindings.entries() ].map(([k,v]) => (`'${k}:${SHOW(v)}`)).join(', ') }} `
            + ((env.parent?.parent == undefined) ? '%E[_]' : `${ DUMP(env.parent) }`)
    }
}

export function createBaseEnvironment () : Env {
    let env = new Env();

    env.assign( AST.Sym('defun'), AST.FExpr(
        Util.List.make( AST.Sym('signature'), AST.Sym('body') ),
        (args : Types.Expr[], ctx : Context) : Types.Expr => {
            let [ signature, body ] = args;
            Util.Type.assertList(signature);
            Util.Type.assertList(body);
            let [ name, ...params ] = Util.List.flatten(signature);
            Util.Type.assertSym(name);
            params.map((p) => Util.Type.assertSym(p));
            return AST.Bind(
                name,
                AST.Lambda( Util.List.make(...params), body as Types.Expr, ctx.env )
            );
        }
    ));

    env.assign( AST.Sym('lambda'), AST.FExpr(
        Util.List.make( AST.Sym('params'), AST.Sym('body') ),
        (args : Types.Expr[], ctx : Context) : Types.Expr => {
            let [ params, body ] = args;
            Util.Type.assertList(params);
            Util.Type.assertList(body);
            return AST.Lambda( params as Types.List, body as Types.Expr, ctx.env );
        }
    ));

    env.assign( AST.Sym('if'), AST.FExpr(
        Util.List.make( AST.Sym('cond'), AST.Sym('then'), AST.Sym('else') ),
        (args : Types.Expr[], ctx : Context) : Types.Expr => {
            let [ cond, thenBranch, elseBranch ] = args;
            Util.Type.assertList(cond);
            Util.Type.assertList(thenBranch);
            Util.Type.assertList(elseBranch);
            return AST.Cond( cond, thenBranch, elseBranch );
        }
    ));

    env.assign( AST.Sym('=='), liftPredicate((n, m) => n == m));
    env.assign( AST.Sym('!='), liftPredicate((n, m) => n != m));

    env.assign( AST.Sym('>'),  liftPredicate((n, m) => n >  m));
    env.assign( AST.Sym('>='), liftPredicate((n, m) => n >= m));
    env.assign( AST.Sym('<='), liftPredicate((n, m) => n <= m));
    env.assign( AST.Sym('<'),  liftPredicate((n, m) => n <  m));

    env.assign( AST.Sym('+'),  liftNumBinOp((n, m) => n + m));
    env.assign( AST.Sym('-'),  liftNumBinOp((n, m) => n - m));
    env.assign( AST.Sym('*'),  liftNumBinOp((n, m) => n * m));
    env.assign( AST.Sym('/'),  liftNumBinOp((n, m) => n / m));
    env.assign( AST.Sym('%'),  liftNumBinOp((n, m) => n % m));

    return env;
}

export type Predicate = (lhs : string | number | boolean, rhs : string | number | boolean) => boolean
export type NumBinOp  = (lhs : number, rhs : number) => number

export function liftPredicate (pred : Predicate) : Types.Native {
    return AST.Native(
        Util.List.make( AST.Sym('n'), AST.Sym('m') ),
        (args : Types.Expr[], ctx : Context) : Types.Expr => {
            let [ lhs, rhs ] = args;
            Util.Type.assertLiteral(lhs);
            Util.Type.assertLiteral(rhs);
            return pred(lhs.value, rhs.value) ? AST.True() : AST.False();
        }
    )
}

export function liftNumBinOp (binop : NumBinOp) : Types.Native {
    return AST.Native(
        Util.List.make( AST.Sym('n'), AST.Sym('m') ),
        (args : Types.Expr[], ctx : Context) : Types.Expr => {
            let [ lhs, rhs ] = args;
            Util.Type.assertNum(lhs);
            Util.Type.assertNum(rhs);
            return AST.Num( binop(lhs.value, rhs.value) );
        }
    )
}
