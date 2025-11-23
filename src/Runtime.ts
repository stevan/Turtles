
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
        return `%E[${env.depth()}]{${ [ ...env.bindings.keys() ].map((e) => ("`" + e)).join(', ') }} `
            + (env.parent == undefined ? '' : ` ^(${ DUMP(env.parent) })`)
    }
}

export function createBaseEnvironment () : Env {
    let env = new Env();

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
