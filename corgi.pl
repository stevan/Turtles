#!perl

use v5.40;
use experimental qw[ class switch ];

## -----------------------------------------------------------------------------

class Value {
    method type; # BOOL, NUM, STR, NATIVE, FEXPR, CLOSURE
}

class Literal :isa(Value) {
    method value; # boolean, number, string
}

class Bool :isa(Literal) { method type { 'BOOL' } field $value :param :reader; }
class Num  :isa(Literal) { method type { 'NUM'  } field $value :param :reader; }
class Str  :isa(Literal) { method type { 'STR'  } field $value :param :reader; }

class Callable :isa(Value) {
    method params; # list of var()
    method body;   # native function, native fexpr, user lambdas
}

class Native  :isa(Callable) { method type { 'NATIVE'  } field $params :param :reader; field $body :param :reader; }
class FExpr   :isa(Callable) { method type { 'FEXPR'   } field $params :param :reader; field $body :param :reader; }
class Closure :isa(Callable) { method type { 'CLOSURE' }
    field $params :param :reader;
    field $body   :param :reader;
    field $env    :param :reader; # environment
}

## -----------------------------------------------------------------------------

class Expression { method type; }

class List :isa(Expression) {}

# ---------------------------
# ... create lists
# ---------------------------
class Nil  :isa(List) { method type { 'NIL' } }
class Cons :isa(List) { method type { 'CONS' } }
    field $head :param :reader; # Expr or Value
    field $tail :param :reader; # Cons or Nil
}

class ListExpression :isa(Expression) { method list; }

# ... these evaluate parts of the list
class Head  :isa(ListExpression) { method type { 'HEAD' } field $list :param :reader; }
class Tail  :isa(ListExpression) { method type { 'TAIL' } field $list :param :reader; }
class NullP :isa(ListExpression) { method type { 'NIL?' } field $list :param :reader; }

# ---------------------------
# ... create closures
# ---------------------------
class Lambda :isa(Expression) { method type { 'LAMBDA' }
    field $params :param :reader; # List
    field $body   :param :reader; # List
}

# ---------------------------
# ... create abstractions
# ---------------------------
class Identifier :isa(Expression) {
    method ident; # string
}

# ... builtin words, user defined variables
class Word :isa(Identifier) { method type { 'WORD' } field $ident :param :reader; }
class Var  :isa(Identifier) { method type { 'VAR'  } field $ident :param :reader; }

# ... apply fexprs, builtins, and lamdas
class Apply :isa(Expression) { method type { 'APPLY' }
    field $func :param :reader; # Var, Word, or Callable
    field $args :param :reader; # List
}

# ---------------------------
# Literal values
# ---------------------------
class Const :isa(Expression) { method type { 'CONST'  }
    field $literal :param :reader; # Value->Literal
}


## -----------------------------------------------------------------------------

