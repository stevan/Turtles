
import type * as Types  from './Types'
import type { Context } from './Context'
import type { Env }     from './Env'

export function True  () : Types.Bool { return { type : 'BOOL', value : true  } }
export function False () : Types.Bool { return { type : 'BOOL', value : false } }

export function Num (value : number) : Types.Num { return { type : 'NUM', value } }
export function Str (value : string) : Types.Str { return { type : 'STR', value } }

export function Nil  () : Types.Nil  { return { type : 'NIL'} }
export function Cons (head : Types.Expr, tail : Types.List) : Types.List { return { type : 'CONS', head, tail } }

export function Native (params : Types.List, body : Types.NativeFunc)  : Types.Native  { return { type : 'NATIVE', params, body } }
export function FExpr  (params : Types.List, body : Types.NativeFExpr) : Types.FExpr   { return { type : 'FEXPR',  params, body } }
export function Lambda (params : Types.List, body : Types.Expr, env : Env) : Types.Lambda {
    return { type : 'LAMBDA', params, body, env }
}
export function Cond (cond : Types.Expr, ifTrue : Types.Expr, ifFalse : Types.Expr) : Types.Cond {
    return { type : 'COND', cond, ifTrue, ifFalse }
}

export function Sym (ident : string) : Types.Sym { return { type : 'SYM',  ident } }
