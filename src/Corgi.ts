// -----------------------------------------------------------------------------

type Ident = string

type True   = { type : '*T*' }
type False  = { type : '*F*' }

type Bool = True | False

type Int    = { type : 'INT', value : number  }
type Float  = { type : 'FLT', value : number  }
type String = { type : 'STR', value : string  }

type Literal = Int | Float | String | Bool

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Value, tail : List }

type List = Cons | Nil

type Lambda = { type : 'LAMBDA', params : List, body: List }
type Word   = { type : 'WORD', ident : Ident }
type Var    = { type : 'VAR',  ident : Ident }

type Value = Literal | List | Lambda | Word | Var

// -----------------------------------------------------------------------------

function isWord   (v : any) : v is Word   { return v.type == 'WORD' }
function isInt    (v : any) : v is Int    { return v.type == 'INT'  }
function isFloat  (v : any) : v is Float  { return v.type == 'FLT'  }
function isString (v : any) : v is String { return v.type == 'STR'  }
function isNil    (v : any) : v is Nil    { return v.type == 'NIL'  }
function isCons   (v : any) : v is Cons   { return v.type == 'CONS' }
function isTrue   (v : any) : v is True   { return v.type == '*T*'  }
function isFalse  (v : any) : v is False  { return v.type == '*F*'  }
function isVar    (v : any) : v is Var    { return v.type == 'VAR'  }
function isLambda (v : any) : v is Lambda { return v.type == 'LAMBDA' }
function isList   (v : any) : v is List   { return isCons(v) || isNil(v) }
function isBool   (v : any) : v is Bool   { return isTrue(v) || isFalse(v) }
function isLiteral (v : any) : v is Literal {
    return isInt(v) || isFloat(v) || isString(v) || isBool(v)
}
function isValue (v : any) : v is Value {
    return isLiteral(v) || isList(v) || isLambda(v) || isWord(v) || isVar(v)
}

function assertInt     (v : any) : asserts v is Int     { if (!isInt(v))     throw new Error("Not Int")     }
function assertFloat   (v : any) : asserts v is Float   { if (!isFloat(v))   throw new Error("Not Float")   }
function assertString  (v : any) : asserts v is String  { if (!isString(v))  throw new Error("Not String")  }
function assertTrue    (v : any) : asserts v is True    { if (!isTrue(v))    throw new Error("Not True")    }
function assertFalse   (v : any) : asserts v is False   { if (!isFalse(v))   throw new Error("Not False")   }
function assertBool    (v : any) : asserts v is Bool    { if (!isBool(v))    throw new Error("Not Bool")    }
function assertNil     (v : any) : asserts v is Nil     { if (!isNil(v))     throw new Error("Not Nil")     }
function assertCons    (v : any) : asserts v is Cons    { if (!isCons(v))    throw new Error("Not Cons")    }
function assertList    (v : any) : asserts v is List    { if (!isList(v))    throw new Error("Not List")    }
function assertLambda  (v : any) : asserts v is Lambda  { if (!isLambda(v))  throw new Error("Not Lambda")  }
function assertWord    (v : any) : asserts v is Word    { if (!isWord(v))    throw new Error("Not Word")    }
function assertVar     (v : any) : asserts v is Var     { if (!isVar(v))     throw new Error("Not Var")     }
function assertLiteral (v : any) : asserts v is Literal { if (!isLiteral(v)) throw new Error("Not Literal") }
function assertValue   (v : any) : asserts v is Value   { if (!isValue(v))   throw new Error("Not Value")   }

// -----------------------------------------------------------------------------

function Word (ident : Ident) : Word { return { type : 'WORD', ident } }
function Var  (ident : Ident) : Var  { return { type : 'VAR',  ident } }

function True  () : True  { return { type : '*T*' } }
function False () : False { return { type : '*F*' } }

function Int    (value : number) : Int    { return { type : 'INT', value } }
function Float  (value : number) : Float  { return { type : 'FLT', value } }
function String (value : string) : String { return { type : 'STR', value } }

function Nil  () : Nil { return { type : 'NIL'} }
function Cons (head : Value, tail : List) : List {
    return { type : 'CONS', head, tail }
}

function Lambda (params : List, body : List) : Lambda {
    return { type : 'LAMBDA', params, body }
}

// -----------------------------------------------------------------------------

function list (...items : Value[]) : List {
    let list : List = Nil();
    while (items.length > 0) {
        let next = items.pop();
        assertValue(next);
        list = Cons(next, list);
    }
    return list;
}

function head (l : List) : Value {
    if (isNil(l)) return l;
    return l.head;
}

function tail (l : List) : List {
    if (isNil(l)) return l;
    return l.tail;
}

// -----------------------------------------------------------------------------

type Native = (args : Value[]) => Value

class Environment {
    public builtins : Map<Ident, Native> = new Map<Ident, Native>();
    public locals   : Map<Ident, Value>  = new Map<Ident, Value>();

    addBuiltin (ident : Ident, bif : Native) : void {
        this.builtins.set(ident, bif);
    }

    lookupBuiltin (word : Word) : any {
        let bif = this.builtins.get(word.ident);
        if (bif == undefined) throw new Error(`Unable to find ${word.ident} in E.builtins`);
        return bif;
    }

    addVar (v : Var, val : Value) : void {
        this.locals.set(v.ident, val);
    }

    lookupVar (v : Var) : Value {
        let value = this.locals.get(v.ident);
        if (value == undefined) throw new Error(`Unable to find ${v.ident} in E.locals`);
        return value;
    }

}

// -----------------------------------------------------------------------------

const DEBUG = true;
const LOG   = (d : number, msg : string, e : any = undefined) => console.log('  '.repeat(d), msg, e ? `<${e.type}>` : '' );

const deCons = (l : List) : Value[] => isNil(l) ? [] : [ head(l), ...deCons(tail(l)) ]

function evaluate (expr : Value, env : Environment, depth : number = 0) : Value {
    switch (true) {
    case isCons(expr):
        if (DEBUG) LOG(depth, 'Got CONS');
        let top = evaluate(head(expr), env, depth + 1);
        if (DEBUG) LOG(depth, 'Got CONS HEAD?', top);
        switch (true) {
        case isWord(top):
            if (DEBUG) LOG(depth, 'Got CONS HEAD? WORD?', top);
            switch(top.ident) {
            // special form ...
            case 'lambda':
                if (DEBUG) LOG(depth, 'Got CONS HEAD? WORD? `lambda (special form)', top);
                let rest   = tail(expr);
                let params = head(rest);
                assertList(params);
                let body   = head(tail(rest));
                assertList(body);
                return Lambda( params, body );
            default:
                if (DEBUG) LOG(depth, 'Got CONS HEAD? WORD? *BIF*', top);
                let bif = env.lookupBuiltin(top);
                return bif( deCons( evaluate(tail(expr), env, depth + 1) as List ) );
            }
        case isLambda(top):
            if (DEBUG) LOG(depth, 'Got CONS HEAD? LAMBDA', top);

            let params = deCons(top.params);
            let args   = deCons(evaluate(tail(expr), env, depth + 1) as List);

            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                assertVar(param);
                assertValue(arg);
                env.addVar(param, arg);
            }

            return evaluate( top.body, env );
        default:
            if (DEBUG) LOG(depth, 'Got CONS *LIST*');
            return Cons( top, evaluate(tail(expr), env, depth + 1) as List );
        }
    case isVar(expr):
        if (DEBUG) LOG(depth, 'Got VAR', expr);
        return env.lookupVar(expr);
    case isValue(expr):
        if (DEBUG) LOG(depth, 'Got VAL', expr);
        return expr;
    default:
        throw new Error('WTF!');
    }
}

// -----------------------------------------------------------------------------

let env = new Environment();

env.addBuiltin('+', (args : Value[]) : Value => {
    let [ lhs, rhs ] = args;
    assertInt(lhs);
    assertInt(rhs);
    return Int(lhs.value + rhs.value);
});

//let expr = list( Word('+'), Int(20), list( Word('+'), Int(2), list( Word('+'), Int(20), list( Word('+'), Int(2), Int(5) ) ) ) );

let expr = list(
    list(
        Word('lambda'),
        list( Var('x'), Var('y') ),
        list( Word('+'), Var('x'), Var('y') ),
    ),
    Int(10),
    Int(20)
);

//console.log(JSON.stringify(expr, null, 4));
console.log(JSON.stringify(evaluate(expr, env), null, 4));

// -----------------------------------------------------------------------------


