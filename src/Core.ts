

// -----------------------------------------------------------------------------
// Terms
// -----------------------------------------------------------------------------

export type NativeFunc  = (arg : Term) => Term;
export type NativeFExpr = (arg : Term, env : Term) => [ Term, Term ];

export type NIL     = { kind : 'NIL' }
export type TRUE    = { kind : 'TRUE' }
export type FALSE   = { kind : 'FALSE' }
export type SYM     = { kind : 'SYM', value : string }
export type STR     = { kind : 'STR', value : string }
export type NUM     = { kind : 'NUM', value : number }
export type PAIR    = { kind : 'PAIR',    fst : Term, snd : Term }
export type LAMBDA  = { kind : 'LAMBDA',  param : Term, body : Term }
export type NATIVE  = { kind : 'NATIVE',  body : NativeFunc }
export type FEXPR   = { kind : 'FEXPR',   body : NativeFExpr }
export type CLOSURE = { kind : 'CLOSURE', abs : Term, env : Term }

// Vau-style
export type Operative   = FEXPR
export type Applicative = CLOSURE | NATIVE
export type Callable    = Operative | Applicative

export type Term =
          | NIL
          | TRUE
          | FALSE
          | SYM
          | STR
          | NUM
          | PAIR
          | LAMBDA
          | Callable

export function Nil   () : NIL   { return { kind : 'NIL'   } }
export function True  () : TRUE  { return { kind : 'TRUE'  } }
export function False () : FALSE { return { kind : 'FALSE' } }

export function Sym (value : string) : SYM {
    return { kind : 'SYM', value }
}

export function Str (value : string) : STR {
    return { kind : 'STR', value }
}

export function Num (value : number) : NUM {
    return { kind : 'NUM', value }
}

export function Pair (fst : Term, snd : Term) : PAIR {
    return { kind : 'PAIR', fst, snd }
}

export function Lambda (param : Term, body : Term) : LAMBDA {
    return { kind : 'LAMBDA', param, body }
}

export function Closure (abs : Term, env : Term) : CLOSURE {
    return { kind : 'CLOSURE', abs, env }
}

export function Native (body : NativeFunc) : NATIVE {
    return { kind : 'NATIVE', body }
}

export function FExpr (body : NativeFExpr) : FEXPR {
    return { kind : 'FEXPR', body }
}

// -----------------------------------------------------------------------------
// Core API
// -----------------------------------------------------------------------------

export function isNil   (t : Term) : t is NIL   { return t.kind == 'NIL'   }
export function isTrue  (t : Term) : t is TRUE  { return t.kind == 'TRUE'  }
export function isFalse (t : Term) : t is FALSE { return t.kind == 'FALSE' }

export function isNum (t : Term) : t is NUM { return t.kind == 'NUM' }
export function isStr (t : Term) : t is STR { return t.kind == 'STR' }
export function isSym (t : Term) : t is SYM { return t.kind == 'SYM' }

export function isPair (t : Term) : t is PAIR { return t.kind == 'PAIR' }

export function isLambda  (t : Term) : t is LAMBDA  { return t.kind == 'LAMBDA'  }
export function isNative  (t : Term) : t is NATIVE  { return t.kind == 'NATIVE'  }
export function isFExpr   (t : Term) : t is FEXPR   { return t.kind == 'FEXPR'   }
export function isClosure (t : Term) : t is CLOSURE { return t.kind == 'CLOSURE' }

export function isOperative   (t : Term) : t is Operative   { return isFExpr(t) }
export function isApplicative (t : Term) : t is Applicative {
    return isNative(t) || isClosure(t)
}

export function isCallable (t : Term) : t is Callable {
    return isOperative(t) || isApplicative(t)
}

export function isList (t : Term) : boolean {
    return isNil(t) || (isPair(t) && isList(t.snd))
}

export function makeList (...args : Term[]) : Term {
    let list : Term = Nil();
    while (args.length > 0) {
        list = Pair( args.pop() as Term, list );
    }
    return list;
}

export function equalTo (lhs : Term, rhs : Term) : boolean {
    if (lhs.kind == rhs.kind) {
        switch (true) {
        case isNil(lhs)   :
        case isTrue(lhs)  :
        case isFalse(lhs) : return true;
        case isSym(lhs)   :
        case isStr(lhs)   :
        case isNum(lhs)   :
            if (lhs.kind != rhs.kind) return false;
            return lhs.value == rhs.value;
        case isPair(lhs)  :
            if (lhs.kind != rhs.kind) return false;
            return equalTo( lhs.fst, rhs.fst )
                && equalTo( lhs.snd, rhs.snd );
        case isLambda(lhs):
            if (lhs.kind != rhs.kind) return false;
            return equalTo( lhs.param, rhs.param )
                && equalTo( lhs.body, rhs.body );
        case isClosure(lhs):
            if (lhs.kind != rhs.kind) return false;
            return equalTo( lhs.env, rhs.env )
                && equalTo( lhs.abs, rhs.abs );
        case isNative(lhs):
        case isFExpr(lhs) :
            if (lhs.kind != rhs.kind) return false;
            return lhs.body === rhs.body;
        default:
            throw new Error('Cannot equalTo a non-Term');
        }
    }
    return false;
}


// -----------------------------------------------------------------------------
