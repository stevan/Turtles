
import * as AST from './AST'

export function True  () : AST.True  { return { type : '*T*', value : true  } }
export function False () : AST.False { return { type : '*F*', value : false } }

export function Int (value : number) : AST.Int { return { type : 'INT', value } }
export function Flt (value : number) : AST.Flt { return { type : 'FLT', value } }
export function Str (value : string) : AST.Str { return { type : 'STR', value } }

export function Nil  ()                                 : AST.Nil  { return { type : 'NIL'} }
export function Cons (head : AST.Expr, tail : AST.List) : AST.List { return { type : 'CONS', head, tail } }

export function Lambda (params : AST.List, body : AST.List)        : AST.Lambda { return { type : 'LAMBDA', params, body } }
export function Native (params : AST.List, body : AST.NativeFunc)  : AST.Native { return { type : 'NATIVE', params, body } }
export function FExpr  (params : AST.List, body : AST.NativeFExpr) : AST.FExpr  { return { type : 'FEXPR',  params, body } }

export function Word    (ident : AST.Ident) : AST.Word    { return { type : 'WORD',    ident } }
export function Var     (ident : AST.Ident) : AST.Var     { return { type : 'VAR',     ident } }
export function Special (ident : AST.Ident) : AST.Special { return { type : 'SPECIAL', ident } }
