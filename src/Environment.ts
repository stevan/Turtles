
import * as Core   from './Core'
import * as Parser from './Parser'

// -----------------------------------------------------------------------------
// Environment
// -----------------------------------------------------------------------------


export function Env (sym : Core.Term, val : Core.Term, env : Core.Term = Core.Nil()) : Core.Term {
    if (!Core.isCons(env)) throw new Error(`Env(syn, val, env) env must be a List not ${env.kind}`);
    return Core.Cons(Core.Pair(sym, val), env);
}

export function isEmpty (env : Core.Term) : boolean {
    return Core.isNil(env);
}

export function head (env : Core.Term) : Core.PAIR {
    if (!Core.isList(env))      throw new Error(`first(env) env must be a pair not ${env.kind}`);
    if (!Core.isPair(env.head)) throw new Error(`Expected pair in Env not ${env.head.kind}`);
    return env.head;
}

export function tail (env : Core.Term) : Core.Term {
    if (!Core.isList(env))      throw new Error(`second(env) env must be a pair not ${env.kind}`);
    if (!Core.isList(env.tail)) throw new Error(`Expected list in Env not ${env.tail.kind}`);
    return env.tail;
}

export function lookup (t : Core.Term, env : Core.Term, depth : number = 0) : Core.Term {
    let indent = '  '.repeat(depth);
    if (!Core.isSym(t)) throw new Error(`lookup(t, env) t must be a symbol not ${t.kind}`);

    console.log(`${indent} /lookup/ ? ${Parser.deparse(t)} IN ${showLocalEnv(env)}`);
    if (isEmpty(env)) throw new Error(`Cannot find ${Parser.deparse(t)} in an empty Env`);

    let curr = head(env);
    console.log(`${indent} /lookup/ ? ${Parser.deparse(t)} EQ ${Parser.deparse(curr.fst)}`);
    if (Core.equalTo(t, curr.fst)) {
        console.log(`${indent} !lookup! @ ${Parser.deparse(t)} => ${Parser.deparse(curr.snd)}`);
        return curr.snd;
    }

    return lookup( t, tail(env), depth + 1 );
}

export function define (sym : Core.Term, t : Core.Term, env : Core.Term) : Core.Term {
    if (!Core.isSym(sym)) throw new Error(`define(sym, t, env) sym must be a symbol not ${t.kind}`);
    return Env( sym, t, env );
}

// debugging

export function showEnv (env : Core.Term) : string {
    if (isEmpty(env)) return '';
    let curr = head(env);
    return `${Parser.deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${showEnv( tail(env) )}`
}

export function showLocalEnv (env : Core.Term) : string {
    if (isEmpty(env)) return '';
    let curr = head(env);
    if (Core.isNative(curr.snd) || Core.isFExpr(curr.snd)) return '%builtins';
    return `${Parser.deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${showLocalEnv( tail(env) )}`
}

// -----------------------------------------------------------------------------
