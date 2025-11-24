
import * as Types from './Types'
import * as AST   from './AST'

export namespace List {
    export function make (...items : Types.Expr[]) : Types.List {
        let list : Types.List = AST.Nil();
        while (items.length > 0) {
            let next = items.pop() as Types.Expr;
            list = AST.Cons(next, list);
        }
        return list;
    }

    export function head (l : Types.List) : Types.Expr {
        if (l.type == 'NIL') return l;
        return l.head;
    }

    export function tail (l : Types.List) : Types.List {
        if (l.type == 'NIL') return l;
        return l.tail;
    }

    export function map (l : Types.List, f : (i : Types.Expr) => Types.Expr) : Types.List {
        if (l.type == 'NIL') return l;
        return AST.Cons( f( head(l) ), map( tail(l), f ) );
    }

    export function flatten (l : Types.List) : Types.Expr[] {
        if (l.type == 'NIL') return [];
        return [ head(l), ...flatten( tail(l) ) ]
    }
}

export namespace Type {
    export function isNil  (v : any) : v is Types.Nil  { return v.type == 'NIL'  }
    export function isCons (v : any) : v is Types.Cons { return v.type == 'CONS' }

    export function isCallable (v : any) : v is Types.Callable {
        return v.type == 'FEXPR'
            || v.type == 'NATIVE'
            || v.type == 'LAMBDA'
    }


    export function assertSym (v : any) : asserts v is Types.Sym {
        if (v.type != 'SYM') throw new Error(`Expected Sym and got ${JSON.stringify(v)}`);
    }

    export function assertList (v : any) : asserts v is Types.List {
        if (v.type != 'CONS' && v.type != 'NIL')
            throw new Error(`Expected List and got ${JSON.stringify(v)}`);
    }

    export function assertCallable (v : any) : asserts v is Types.Callable {
        if (!isCallable(v)) throw new Error(`Expected Callable and got ${JSON.stringify(v)}`);
    }

    export function assertLiteral (v : any) : asserts v is Types.Literal {
        if (v.type != 'NUM' && v.type != 'STR' && v.type != 'BOOL')
            throw new Error(`Expected Literal and got ${JSON.stringify(v)}`);
    }

    export function assertNum (v : any) : asserts v is Types.Num {
        if (v.type != 'NUM') throw new Error(`Expected Num and got ${JSON.stringify(v)}`);
    }

    export function assertBool (v : any) : asserts v is Types.Bool {
        if (v.type != 'BOOL') throw new Error(`Expected Bool and got ${JSON.stringify(v)}`);
    }
}
