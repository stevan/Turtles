// -----------------------------------------------------------------------------

type Ident = string

type Word   = { type : 'WORD', ident : Ident  }

type True   = { type : '*T*' }
type False  = { type : '*F*' }

type Bool = True | False

type Int    = { type : 'INT', value : number  }
type Float  = { type : 'FLT', value : number  }
type String = { type : 'STR', value : string  }

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Literal, tail : List }

type Literal = Int | Float | String | Bool | Word | Nil

type List = Cons | Nil

type Expr = List | Literal

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
    return isInt(v) || isFloat(v) || isString(v) || isBool(v) || isWord(v) || isNil(v)
}
function isExpr (v : any) : v is Expr { return isLiteral(v) || isList(v) }

function assertWord    (v : any) : asserts v is Word    { if (!isWord(v))    throw new Error("Not Word")    }
function assertInt     (v : any) : asserts v is Int     { if (!isInt(v))     throw new Error("Not Int")     }
function assertFloat   (v : any) : asserts v is Float   { if (!isFloat(v))   throw new Error("Not Float")   }
function assertString  (v : any) : asserts v is String  { if (!isString(v))  throw new Error("Not String")  }
function assertNil     (v : any) : asserts v is Nil     { if (!isNil(v))     throw new Error("Not Nil")     }
function assertCons    (v : any) : asserts v is Cons    { if (!isCons(v))    throw new Error("Not Cons")    }
function assertTrue    (v : any) : asserts v is True    { if (!isTrue(v))    throw new Error("Not True")    }
function assertFalse   (v : any) : asserts v is False   { if (!isFalse(v))   throw new Error("Not False")   }
function assertList    (v : any) : asserts v is List    { if (!isList(v))    throw new Error("Not List")    }
function assertBool    (v : any) : asserts v is Bool    { if (!isBool(v))    throw new Error("Not Bool")    }
function assertLiteral (v : any) : asserts v is Literal { if (!isLiteral(v)) throw new Error("Not Literal") }
function assertExpr    (v : any) : asserts v is Expr    { if (!isExpr(v))    throw new Error("Not Expr")    }

// -----------------------------------------------------------------------------

function $Word (ident : Ident) : Word { return { type : 'WORD', ident  } }

function $True  () : True  { return { type : '*T*' } }
function $False () : False { return { type : '*F*' } }

function $Int    (value : number) : Int    { return { type : 'INT', value } }
function $Float  (value : number) : Float  { return { type : 'FLT', value } }
function $String (value : string) : String { return { type : 'STR', value } }

function $Nil  () : Nil { return { type : 'NIL'} }
function $Cons (head : Literal, tail : List) : List {
    return { type : 'CONS', head, tail }
}

function $List (...items : Literal[]) : List {
    if (items.length == 0) {
        return $Nil();
    }

    let next = items.pop();
    assertLiteral(next);

    let list = $Cons(next, $Nil());
    while (items.length > 0) {
        let next = items.pop();
        assertLiteral(next);
        list = $Cons(next, list);
    }
    return list;
}

// -----------------------------------------------------------------------------

function head (l : List) : Literal {
    assertList(l);
    if (l.type == 'NIL') return l;
    return l.head;
}

function tail (l : List) : List {
    assertList(l);
    if (l.type == 'NIL') return l;
    return l.tail;
}

// -----------------------------------------------------------------------------

class Environment extends Map<Ident, any> {}

// -----------------------------------------------------------------------------

function isApplication (expr : Expr) : expr is List    { return isList(expr) && isWord(head(expr)) }
function isValue       (expr : Expr) : expr is Literal { return isLiteral(expr) }

function evaluate (expr : Expr, env : Environment) : Literal {
    switch (true) {
    case isApplication(expr):

        let word = head(expr);
        assertWord(word);

        let bif  = env.get(word.ident);
        if (bif == undefined) throw new Error(`Unable to find ${word.ident} in E`);

        let rest = tail(expr);
        let args = [];
        while (!isNil(rest)) {
            args.push( evaluate( head(rest), env ) );
            rest = tail(rest);
        }

        return bif( ...args );
    case isValue(expr):
        return expr;
    default:
        throw new Error('WTF!');
    }
}

// -----------------------------------------------------------------------------

let env = new Environment();

env.set('+', (lhs : Literal, rhs : Literal) : Literal => {
    assertInt(lhs);
    assertInt(rhs);
    return $Int(lhs.value + rhs.value);
});

let expr = $List( $Word('+'), $Int(20), $Int(10) );

console.log(evaluate(expr, env));

// -----------------------------------------------------------------------------








