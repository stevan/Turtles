
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'

export function create (...items : AST.Expr[]) : AST.List {
    let list : AST.List = ASTUtil.Nil();
    while (items.length > 0) {
        let next = items.pop();
        TypeUtil.assertExpr(next);
        list = ASTUtil.Cons(next, list);
    }
    return list;
}

export function head (l : AST.List) : AST.Expr {
    if (TypeUtil.isNil(l)) return l;
    return l.head;
}

export function tail (l : AST.List) : AST.List {
    if (TypeUtil.isNil(l)) return l;
    return l.tail;
}

export function map (l : AST.List, f : (i : AST.Expr) => AST.Expr) : AST.List {
    if (TypeUtil.isNil(l)) return l;
    return ASTUtil.Cons( f( head(l) ), map( tail(l), f ) );
}

export function flatten (l : AST.List) : AST.Expr[] {
    if (TypeUtil.isNil(l)) return [];
    return [ head(l), ...flatten( tail(l) ) ]
}
