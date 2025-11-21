
import * as AST from './AST'

// Values

export function True  () : AST.True  { return { type : '*T*', value : true  } }
export function False () : AST.False { return { type : '*F*', value : false } }

export function Num (value : number) : AST.Num { return { type : 'NUM', value } }
export function Str (value : string) : AST.Str { return { type : 'STR', value } }

export function Pair (first : AST.Value, second : AST.Value) : AST.Pair { return { type : 'PAIR', first, second } }

export function Nil  ()                                 : AST.Nil  { return { type : 'NIL'} }
export function Cons (head : AST.Expr, tail : AST.List) : AST.List { return { type : 'CONS', head, tail } }

export function Lambda (params : AST.List, body : AST.List, env : AST.Env) : AST.Lambda { return { type : 'LAMBDA', params, body, env } }
export function Native (params : AST.List, body : AST.NativeFunc)          : AST.Native { return { type : 'NATIVE', params, body } }
export function FExpr  (params : AST.List, body : AST.NativeFExpr)         : AST.FExpr  { return { type : 'FEXPR',  params, body } }

// Expressions

export function Word (ident : AST.Ident) : AST.Word { return { type : 'WORD', ident } }
export function Var  (ident : AST.Ident) : AST.Var  { return { type : 'VAR',  ident } }

export function Apply (call : AST.Identifier, args : AST.List) : AST.Apply { return { type : 'APPLY', call, args } }
