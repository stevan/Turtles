// -----------------------------------------------------------------------------

type True   = { type : '*T*' }
type False  = { type : '*F*' }

type Bool = True | False

type Int    = { type : 'INT', value : number  }
type Float  = { type : 'FLT', value : number  }
type String = { type : 'STR', value : string  }

type Literal = Int | Float | String | Bool

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Expr, tail : List }

type List = Cons | Nil

type NativeFunc  = ( args : Expr[] ) => Expr
type NativeFExpr = ( args : Expr[] ) => Expr

type Lambda  = { type : 'LAMBDA',  params : List, body : List }
type Native  = { type : 'NATIVE',  params : List, body : NativeFunc  }
type FExpr   = { type : 'FEXPR',   params : List, body : NativeFExpr }

type Callable = Lambda | Native | FExpr

type Ident = string

type Word    = { type : 'WORD',    ident : Ident }
type Var     = { type : 'VAR',     ident : Ident }
type Special = { type : 'SPECIAL', ident : Ident }

type Expr = Literal | List | Callable  | Word | Var | Special

// -----------------------------------------------------------------------------

function isWord     (v : any) : v is Word     { return v.type == 'WORD' }
function isVar      (v : any) : v is Var      { return v.type == 'VAR'  }
function isInt      (v : any) : v is Int      { return v.type == 'INT'  }
function isFloat    (v : any) : v is Float    { return v.type == 'FLT'  }
function isString   (v : any) : v is String   { return v.type == 'STR'  }
function isNil      (v : any) : v is Nil      { return v.type == 'NIL'  }
function isCons     (v : any) : v is Cons     { return v.type == 'CONS' }
function isTrue     (v : any) : v is True     { return v.type == '*T*'  }
function isFalse    (v : any) : v is False    { return v.type == '*F*'  }
function isFExpr    (v : any) : v is FExpr    { return v.type == 'FEXPR'   }
function isNative   (v : any) : v is Native   { return v.type == 'NATIVE'  }
function isLambda   (v : any) : v is Lambda   { return v.type == 'LAMBDA'  }
function isSpecial  (v : any) : v is Special  { return v.type == 'SPECIAL' }
function isList     (v : any) : v is List     { return isCons(v)    || isNil(v)    }
function isBool     (v : any) : v is Bool     { return isTrue(v)    || isFalse(v)  }
function isCallable (v : any) : v is Callable { return isNative(v)  || isLambda(v) || isFExpr(v) }
function isLiteral  (v : any) : v is Literal  { return isInt(v)  || isFloat(v) || isString(v)  || isBool(v)     }
function isExpr     (v : any) : v is Expr     { return isList(v) || isWord(v)  || isLiteral(v) || isCallable(v) || isVar(v) || isSpecial(v) }

function assertInt      (v : any) : asserts v is Int      { if (!isInt(v))      throw new Error("Not Int")      }
function assertFloat    (v : any) : asserts v is Float    { if (!isFloat(v))    throw new Error("Not Float")    }
function assertString   (v : any) : asserts v is String   { if (!isString(v))   throw new Error("Not String")   }
function assertTrue     (v : any) : asserts v is True     { if (!isTrue(v))     throw new Error("Not True")     }
function assertFalse    (v : any) : asserts v is False    { if (!isFalse(v))    throw new Error("Not False")    }
function assertBool     (v : any) : asserts v is Bool     { if (!isBool(v))     throw new Error("Not Bool")     }
function assertNil      (v : any) : asserts v is Nil      { if (!isNil(v))      throw new Error("Not Nil")      }
function assertCons     (v : any) : asserts v is Cons     { if (!isCons(v))     throw new Error("Not Cons")     }
function assertList     (v : any) : asserts v is List     { if (!isList(v))     throw new Error("Not List")     }
function assertFExpr    (v : any) : asserts v is FExpr    { if (!isFExpr(v))    throw new Error("Not FExpr")    }
function assertLambda   (v : any) : asserts v is Lambda   { if (!isLambda(v))   throw new Error("Not Lambda")   }
function assertNative   (v : any) : asserts v is Native   { if (!isNative(v))   throw new Error("Not Native")   }
function assertSpecial  (v : any) : asserts v is Special  { if (!isSpecial(v))  throw new Error("Not Special")  }
function assertVar      (v : any) : asserts v is Var      { if (!isVar(v))      throw new Error("Not Var")      }
function assertWord     (v : any) : asserts v is Word     { if (!isWord(v))     throw new Error("Not Word")     }
function assertCallable (v : any) : asserts v is Callable { if (!isCallable(v)) throw new Error("Not Callable") }
function assertLiteral  (v : any) : asserts v is Literal  { if (!isLiteral(v))  throw new Error("Not Literal")  }
function assertExpr     (v : any) : asserts v is Expr     { if (!isExpr(v))     throw new Error("Not Expr")     }

// -----------------------------------------------------------------------------

function True  () : True  { return { type : '*T*' } }
function False () : False { return { type : '*F*' } }

function Int    (value : number) : Int    { return { type : 'INT', value } }
function Float  (value : number) : Float  { return { type : 'FLT', value } }
function String (value : string) : String { return { type : 'STR', value } }

function Nil  ()                          : Nil  { return { type : 'NIL'} }
function Cons (head : Expr, tail : List) : List { return { type : 'CONS', head, tail } }

function Lambda (params : List, body : List)        : Lambda { return { type : 'LAMBDA', params, body } }
function Native (params : List, body : NativeFunc)  : Native { return { type : 'NATIVE', params, body } }
function FExpr  (params : List, body : NativeFExpr) : FExpr  { return { type : 'FEXPR',  params, body } }

function Word    (ident : Ident) : Word    { return { type : 'WORD',    ident } }
function Var     (ident : Ident) : Var     { return { type : 'VAR',     ident } }
function Special (ident : Ident) : Special { return { type : 'SPECIAL', ident } }

// -----------------------------------------------------------------------------

function list (...items : Expr[]) : List {
    let list : List = Nil();
    while (items.length > 0) {
        let next = items.pop();
        assertExpr(next);
        list = Cons(next, list);
    }
    return list;
}

function head (l : List) : Expr {
    if (isNil(l)) return l;
    return l.head;
}

function tail (l : List) : List {
    if (isNil(l)) return l;
    return l.tail;
}

// -----------------------------------------------------------------------------

type EnvKey = Word | Special | Var

class Environment extends Map<Ident, Expr> {

    lookup (name : EnvKey) : Expr {
        let result = this.get(name.ident);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (key : EnvKey, val : Expr) : void {
        this.set(key.ident, val);
    }
}

// -----------------------------------------------------------------------------

const DEBUG = true;
const LOG   = (d : number, msg : string, e : any = undefined) => console.log('  '.repeat(d), msg, e ? `<${e.type}>` : '' );

const deCons = (l : List) : Expr[] => isNil(l) ? [] : [ head(l), ...deCons(tail(l)) ]

function evaluate (expr : Expr, env : Environment, depth : number = 0) : Expr {
    switch (true) {
    case isCons(expr):
        if (DEBUG) LOG(depth, 'Got CONS');
        let top = evaluate(head(expr), env, depth + 1);
        if (DEBUG) LOG(depth, 'APPLY?', top);
        switch (true) {
        case isFExpr(top):
            if (DEBUG) LOG(depth, '++ APPLY *FEXPR*', top);
            return top.body( deCons( tail(expr) ) );
        case isNative(top):
            if (DEBUG) LOG(depth, '++ APPLY *NATIVE*', top);
            return top.body( deCons( evaluate(tail(expr), env, depth + 1) as List ) );
        case isLambda(top):
            if (DEBUG) LOG(depth, '++ APPLY *LAMBDA*', top);
            let params = deCons(top.params);
            let args   = deCons(evaluate(tail(expr), env, depth + 1) as List);
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                assertVar(param);
                assertExpr(arg);
                env.assign(param, arg);
            }
            return evaluate( top.body, env );
        default:
            if (DEBUG) LOG(depth, '*LIST*');
            return Cons( top, evaluate(tail(expr), env, depth + 1) as List );
        }
    case isVar(expr):
    case isWord(expr):
    case isSpecial(expr):
        if (DEBUG) LOG(depth, 'Got Var | Word | Special', expr);
        return env.lookup(expr);
    case isLiteral(expr):
        if (DEBUG) LOG(depth, 'Got Literal', expr);
        return expr;
    case isNil(expr):
        if (DEBUG) LOG(depth, '()');
        return expr;
    default:
        throw new Error('WTF!');
    }
}

// -----------------------------------------------------------------------------

let env = new Environment();

env.assign( Special('lambda'), FExpr(
    list( Var('params'), Var('body') ),
    (args : Expr[]) : Expr => {
        let [ params, body ] = args;
        assertList(params);
        assertList(body);
        return Lambda( params, body );
    }
));

env.assign( Word('+'), Native(
    list( Var('n'), Var('m') ),
    (args : Expr[]) : Expr => {
        let [ lhs, rhs ] = args;
        assertInt(lhs);
        assertInt(rhs);
        return Int(lhs.value + rhs.value);
    }
));

//let expr = list( Word('+'), Int(20), list( Word('+'), Int(2), list( Word('+'), Int(20), list( Word('+'), Int(2), Int(5) ) ) ) );

let expr = list(
    list(
        Special('lambda'),
        list( Var('x'), Var('y') ),
        list( Word('+'), Var('x'), Var('y') ),
    ),
    Int(10),
    Int(20)
);

//console.log(JSON.stringify(expr, null, 4));
console.log(JSON.stringify(evaluate(expr, env), null, 4));

// -----------------------------------------------------------------------------


