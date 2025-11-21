// -----------------------------------------------------------------------------
// Core Types
// -----------------------------------------------------------------------------

export type True   = { type : '*T*', value : true  }
export type False  = { type : '*F*', value : false }

export type Bool = True | False

export type Num = { type : 'NUM', value : number  }
export type Str = { type : 'STR', value : string  }

export type Literal = Num | Str | Bool

export type Nil  = { type : 'NIL' }
export type Cons = { type : 'CONS', head : Expr, tail : List }

export type List = Cons | Nil

export type NativeFunc  = ( env: Environment ) => Expr
export type NativeFExpr = ( args : Expr[], env: Environment ) => Expr

export type Lambda  = { type : 'LAMBDA',  params : List, body : List }
export type Native  = { type : 'NATIVE',  params : List, body : NativeFunc  }
export type FExpr   = { type : 'FEXPR',   params : List, body : NativeFExpr }

export type Callable = Lambda | Native | FExpr

export type Ident = string

export type Word    = { type : 'WORD',    ident : Ident }
export type Var     = { type : 'VAR',     ident : Ident }
export type Special = { type : 'SPECIAL', ident : Ident }

export type Identifier = Word | Var | Special

export type Expr = Literal | List | Callable | Identifier




