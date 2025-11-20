// -----------------------------------------------------------------------------

type Ident = string

type Word   = { type : 'WORD', ident : Ident  }

type True   = { type : '*T*' }
type False  = { type : '*F*' }

type Bool = True | False

type Int    = { type : 'INT', value : number  }
type Float  = { type : 'FLT', value : number  }
type String = { type : 'STR', value : string  }

type Literal = Int | Float | String | Bool | Word

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Value, tail : List }
type List = Cons | Nil

type Value = Literal | List

// -----------------------------------------------------------------------------

function isWord   (v : any) : v is Word   { return v.type == 'WORD' }
function isInt    (v : any) : v is Int    { return v.type == 'INT'  }
function isFloat  (v : any) : v is Float  { return v.type == 'FLT'  }
function isString (v : any) : v is String { return v.type == 'STR'  }
function isNil    (v : any) : v is Nil    { return v.type == 'NIL'  }
function isCons   (v : any) : v is Cons   { return v.type == 'CONS' }
function isTrue   (v : any) : v is True   { return v.type == '*T*'  }
function isFalse  (v : any) : v is False  { return v.type == '*F*'  }
function isList   (v : any) : v is List   { return isCons(v) || isNil(v) }
function isBool   (v : any) : v is Bool   { return isTrue(v) || isFalse(v) }
function isLiteral (v : any) : v is Literal {
    return isInt(v) || isFloat(v) || isString(v) || isBool(v) || isWord(v)
}
function isValue (v : any) : v is Value { return isLiteral(v) || isList(v) }

function assertWord    (v : any) : asserts v is Word    { if (!isWord(v))    throw new Error("Not Word")    }
function assertInt     (v : any) : asserts v is Int     { if (!isInt(v))     throw new Error("Not Int")     }
function assertFloat   (v : any) : asserts v is Float   { if (!isFloat(v))   throw new Error("Not Float")   }
function assertString  (v : any) : asserts v is String  { if (!isString(v))  throw new Error("Not String")  }
function assertTrue    (v : any) : asserts v is True    { if (!isTrue(v))    throw new Error("Not True")    }
function assertFalse   (v : any) : asserts v is False   { if (!isFalse(v))   throw new Error("Not False")   }
function assertBool    (v : any) : asserts v is Bool    { if (!isBool(v))    throw new Error("Not Bool")    }
function assertNil     (v : any) : asserts v is Nil     { if (!isNil(v))     throw new Error("Not Nil")     }
function assertCons    (v : any) : asserts v is Cons    { if (!isCons(v))    throw new Error("Not Cons")    }
function assertList    (v : any) : asserts v is List    { if (!isList(v))    throw new Error("Not List")    }
function assertLiteral (v : any) : asserts v is Literal { if (!isLiteral(v)) throw new Error("Not Literal") }
function assertValue   (v : any) : asserts v is Value   { if (!isValue(v))   throw new Error("Not Value")   }

// -----------------------------------------------------------------------------

function Word (ident : Ident) : Word { return { type : 'WORD', ident  } }

function True  () : True  { return { type : '*T*' } }
function False () : False { return { type : '*F*' } }

function Int    (value : number) : Int    { return { type : 'INT', value } }
function Float  (value : number) : Float  { return { type : 'FLT', value } }
function String (value : string) : String { return { type : 'STR', value } }

function Nil  () : Nil { return { type : 'NIL'} }
function Cons (head : Value, tail : List) : List {
    return { type : 'CONS', head, tail }
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

class Environment extends Map<Ident, any> {

    lookup (word : Word) : any {
        let bif = env.get(word.ident);
        if (bif == undefined) throw new Error(`Unable to find ${word.ident} in E`);
        return bif;
    }

}

// -----------------------------------------------------------------------------

const deCons = (l : List) : Value[] => isNil(l) ? [] : [ head(l), ...deCons(tail(l)) ]


function evaluate (expr : Value, env : Environment) : Value {
    switch (true) {
    case isCons(expr):
        let word = head(expr);
        if (isWord(word)) {
            let bif  = env.lookup(word);
            let args = evaluate(tail(expr), env) as List;
            return bif( ...deCons(args) );
        } else {
            return Cons( evaluate(head(expr), env), evaluate(tail(expr), env) as List );
        }
    case isValue(expr):
        return expr;
    default:
        throw new Error('WTF!');
    }
}

// -----------------------------------------------------------------------------

let env = new Environment();

env.set('+', (lhs : Value, rhs : Value) : Value => {
    assertInt(lhs);
    assertInt(rhs);
    return Int(lhs.value + rhs.value);
});

let expr = list( Word('+'), Int(20), list( Word('+'), Int(2), list( Word('+'), Int(20), list( Word('+'), Int(2), Int(5) ) ) ) );

console.log(JSON.stringify(evaluate(expr, env), null, 4));

// -----------------------------------------------------------------------------


