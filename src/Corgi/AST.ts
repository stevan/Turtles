// -----------------------------------------------------------------------------
// Core Types
// -----------------------------------------------------------------------------

export interface Env {
    lookup (name : Identifier) : Value;
    assign (name : Identifier, value : Value) : void;
    derive () : Env
}

// -----------------------------------------------------------------------------
// Values
// -----------------------------------------------------------------------------

// Literals

export type True   = { type : '*T*', value : true  }
export type False  = { type : '*F*', value : false }

export type Bool = True | False

export type Num = { type : 'NUM', value : number  }
export type Str = { type : 'STR', value : string  }

export type Literal = Num | Str | Bool

// Pairs

export type Pair = { type : 'PAIR', first : Value, second : Value }

// Lists

export type Nil  = { type : 'NIL' }
export type Cons = { type : 'CONS', head : Expr, tail : List }

export type List = Cons | Nil

// Callables

export type NativeFunc  = ( env : Env ) => Value
export type NativeFExpr = ( args : Expr[], env : Env ) => Value

export type Native  = { type : 'NATIVE', params : List, body : NativeFunc  }
export type FExpr   = { type : 'FEXPR',  params : List, body : NativeFExpr }
export type Lambda  = { type : 'LAMBDA', params : List, body : List, env : Env }

export type Callable = Lambda | Native | FExpr

// ...

export type Value = Literal | List | Callable

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------

// Named Things

export type Ident = string

export type Word = { type : 'WORD', ident : Ident }
export type Var  = { type : 'VAR',  ident : Ident }

export type Identifier = Word | Var

// Callings things

export type Apply = { type : 'APPLY', call : Identifier, args : List }

// ...

export type Expr = Apply | Identifier | Value

// -----------------------------------------------------------------------------

