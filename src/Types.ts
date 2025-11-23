
import type { Context } from './Context'

// Literals

export type Bool = { type : 'BOOL', value : boolean }
export type Num  = { type : 'NUM',  value : number  }
export type Str  = { type : 'STR',  value : string  }

export type Literal = Num | Str | Bool

// Lists

export type Nil  = { type : 'NIL' }
export type Cons = { type : 'CONS', head : Expr, tail : List }

export type List = Cons | Nil

// Named Things

export type Sym = { type : 'SYM', ident : string }

// Callables

export type NativeFunc  = ( args : Expr[], ctx : Context ) => Expr
export type NativeFExpr = ( args : Expr[], ctx : Context ) => Expr

export type Native = { type : 'NATIVE', params : List, body : NativeFunc  }
export type FExpr  = { type : 'FEXPR',  params : List, body : NativeFExpr }
export type Lambda = { type : 'LAMBDA', params : List, body : Expr, ctx : Context }

export type Callable = Lambda | Native | FExpr

// all of them at once ...

export type Expr  = Literal | List | Sym | Callable
