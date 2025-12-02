import { Console } from 'console';

// -----------------------------------------------------------------------------

enum Kind {
    BOOL   = 'BOOL',
    NUM    = 'NUM',
    STR    = 'STR',
    SYM    = 'SYM',
    NIL    = 'NIL',
    PAIR   = 'PAIR',
}

interface Value { kind : Kind }

interface Atom extends Value {}

interface Bool extends Atom  { kind : Kind.BOOL, value : boolean }
interface Num  extends Atom  { kind : Kind.NUM,  value : number  }
interface Str  extends Atom  { kind : Kind.STR,  value : string  }
interface Sym  extends Atom  { kind : Kind.SYM,  ident : string  }

interface Nil  extends Value  { kind : Kind.NIL }
interface Pair extends Value  {
    kind   : Kind.PAIR,
    first  : Value,
    second : Value
}

type List = Nil | Pair

function bool (value : boolean) : Bool { return { kind : Kind.BOOL, value } }
function str  (value : string)  : Str  { return { kind : Kind.STR,  value } }
function num  (value : number)  : Num  { return { kind : Kind.NUM,  value } }

function nil () : Nil { return { kind : Kind.NIL } }

function pair (first : Value, second : Value) : Pair {
    return { kind : Kind.PAIR, first, second }
}

function tokenize (src : string) : List {
    return (
        src.match(/'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g) ?? []
    ).reverse()
        .reduce((acc : List, token : string) : List => {
            return pair( str(token), acc);
        },
        nil() as List
    )
}

const isBool  = (v : Value) : v is Bool => v.kind == Kind.BOOL;
const isNum   = (v : Value) : v is Num  => v.kind == Kind.NUM;
const isStr   = (v : Value) : v is Str  => v.kind == Kind.STR;
const isSym   = (v : Value) : v is Sym  => v.kind == Kind.SYM;
const isNil   = (v : Value) : v is Nil  => v.kind == Kind.NIL;
const isPair  = (v : Value) : v is Pair => v.kind == Kind.PAIR;

function dump (value : Value) : void {
    const logger = new Console({
        stdout : process.stdout, stderr : process.stderr,
        inspectOptions : { depth : 80, breakLength : 80 }});
    logger.log(value);
}

function show (value : Value) : void {

    const terminal = new Console({
        stdout : process.stdout, stderr : process.stderr,
        inspectOptions : { depth : 20, breakLength : 80 }});

    const pprint = (v : Value) : string => {
        switch (true) {
        case isNil(v)   : return '()';
        case isBool(v)  : return v.value ? '#t' : '#f';
        case isNum(v)   : return v.value.toString();
        case isStr(v)   : return `"${v.value}"`;
        case isSym(v)   : return v.ident;
        case isPair(v)  : return `(${pprint(v.first)} . ${pprint(v.second)})`;
        default: throw new Error(`Unknown value type (${JSON.stringify(v)})`);
        }
    }

    return terminal.log(pprint(value));
}

let list = tokenize('(1 2 3)');
dump(list);
show(list);
