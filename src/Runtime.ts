
import * as Core        from './Core'
import * as Parser      from './Parser'
import * as Environment from './Environment'

// -----------------------------------------------------------------------------
// Runtime
// -----------------------------------------------------------------------------

export function evaluate (t : Core.Term, env : Core.Term, depth : number = 0) : Core.Term {
    let indent = (depth < 0) ? ' @@ ' : '  '.repeat(depth);
    console.log(`● [${depth.toString().padStart(3, (depth < 0) ? ' ' : '0')}] `+'–'.repeat(72));
    console.log(`${indent}EVAL:${t.kind} ${Parser.deparse(t)}`);
    console.log(`${indent}   %:ENV => `+Environment.showLocalEnv(env));
    switch (true) {
    case Core.isNil(t)     :
    case Core.isTrue(t)    :
    case Core.isFalse(t)   :
    case Core.isNum(t)     :
    case Core.isStr(t)     :
    case Core.isNative(t)  :
    case Core.isFExpr(t)   :
    case Core.isClosure(t) :
        console.log(`${indent} $ eval – just ${t.kind}`);
        return t;
    case Core.isSym(t)     :
        console.log(`${indent} $ eval – looking up ${Parser.deparse(t)}`);
        return Environment.lookup( t, env, depth );
    case Core.isLambda(t)  :
        console.log(`${indent} $ eval – creating closure for ${t.kind}`);
        return Core.Closure( t, env );
    case Core.isPair(t)    :
        console.log(`${indent} $ eval – Apply or Pair? (${t.fst.kind} :: ${t.snd.kind})`);
        let head = evaluate( t.fst, env, depth + 1 );
        let tail = t.snd;

        console.log(`◯ [${depth.toString().padStart(3, '0')}] `+'–'.repeat(72));
        console.log(`${indent} $ eval – ${head.kind} ${Parser.deparse(head)}`);

        switch (true) {
        case Core.isOperative(head):
            // FEXPRs
            console.log(`${indent} $ eval – ... Apply FExpr`);
            let [ expr, local ] = head.body( tail, env );
            console.log(`${indent} $ eval – ... Apply FExpr got ${Parser.deparse(expr)} % ${Environment.showLocalEnv(local)}`);
            return evaluate( expr, local, depth + 1 );
        case Core.isApplicative(head):
            return apply( head, evaluate( tail, env, depth + 1 ), depth + 1 );
        default:
            console.log(`${indent} $ eval – ... is Pair! ${Parser.deparse(head)}`);
            let rest = evaluate( tail, env, depth + 1 );
            console.log(`◯ [${depth.toString().padStart(3, '0')}] `+'–'.repeat(72));
            console.log(`${indent} $ eval – Pair( fst: ${Parser.deparse(head)}, snd: <${rest.kind}> ${Parser.deparse(rest)} )`);
            return Core.Pair( head, rest );
        }
    default:
        throw new Error('Cannot eval a non-Term');
    }
}

export function apply (call : Core.Term, arg : Core.Term, depth : number = 0) : Core.Term {
    let indent = (depth < 0) ? ' @@ ' : '  '.repeat(depth);
    console.log(`● [${depth.toString().padStart(3, (depth < 0) ? ' ' : '0')}] `+'–'.repeat(72));
    console.log(`${indent}APPLY:${call.kind} ${Parser.deparse(call)} <- ${Parser.deparse(arg)}`);
    // run what we got ...
    switch (true) {
    case Core.isNative(call) :
        console.log(`${indent} & apply – ... Apply Native`);
        return call.body( arg );
    case Core.isClosure(call) :
        // unpack closures
        console.log(`${indent} & apply – ^unpack Closure ~~ ${Parser.deparse(call)}`);
        console.log(`${indent} & apply – ... Apply Lambda`);
        let lambda = call.abs;
        if (!Core.isLambda(lambda)) throw new Error(`Expected LAMBDA for lambda, but got ${lambda.kind}`);

        let param = lambda.param;
        if (!Core.isSym(param))     throw new Error(`Expected SYM for lambda param, but got ${param.kind}`);
        if (!Core.isPair(arg))      throw new Error(`Expected PAIR for lambda arg, but got ${arg.kind}`);

        console.log(`${indent} w/%:ENV => `+Environment.showLocalEnv(call.env));

        return evaluate( lambda.body, Environment.Env( param, arg.fst, call.env ), depth + 1 );
    default:
        throw new Error(`WTF! ${JSON.stringify(call)}`);
    }
}

export function initEnv () : Core.Term {
    let env : Core.Term = Core.Nil();

    const liftComparisonBinOp = (op : string, f : (n : number | string, m : number | string) => boolean) : Core.NativeFunc => {
        return (arg : Core.Term) : Core.Term => {
            console.log(`▶ CALL &:(${op}) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg)) throw new Error(`Expected Pair in &:(${op}) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!Core.isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(${op}) not(${rhs.kind})`);
            rhs = rhs.fst;
            if (!(Core.isNum(lhs) || Core.isStr(lhs))) throw new Error(`Expected lhs to be Num/Str in &:(${op}) not(${lhs.kind})`);
            if (!(Core.isNum(rhs) || Core.isStr(rhs))) throw new Error(`Expected rhs to be Num/Str in &:(${op}) not(${rhs.kind})`);
            return f( lhs.value, rhs.value ) ? Core.True() : Core.False();
        }
    }

    const liftNumericBinOp = (op : string, f : (n : number, m : number) => number) : Core.NativeFunc => {
        return (arg : Core.Term) : Core.Term => {
            console.log(`▶ CALL &:(${op}) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg)) throw new Error(`Expected Pair in &:(${op}) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!Core.isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(${op}) not(${rhs.kind})`);
            rhs = rhs.fst;
            if (!Core.isNum(lhs)) throw new Error(`Expected lhs to be Num in &:(${op}) not(${lhs.kind})`);
            if (!Core.isNum(rhs)) throw new Error(`Expected rhs to be Num in &:(${op}) not(${rhs.kind})`);
            return Core.Num( f( lhs.value, rhs.value ) );
        }
    }

    // equality for all ...
    env = Environment.define(
        Core.Sym('=='), Core.Native((arg : Core.Term) : Core.Term => {
            console.log(`▶ CALL &:(==) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg)) throw new Error(`Expected Pair in &:(==) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!Core.isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(==) not(${rhs.kind})`);
            return Core.equalTo( lhs, rhs.fst ) ? Core.True() : Core.False();
        }),
        env
    );

    env = Environment.define(
        Core.Sym('!='), Core.Native((arg : Core.Term) : Core.Term => {
            console.log(`▶ CALL &:(!=) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg)) throw new Error(`Expected Pair in &:(!=) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!Core.isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(!=) not(${rhs.kind})`);
            return Core.equalTo( lhs, rhs.fst ) ? Core.False() : Core.True();
        }),
        env
    );

    // just for strings and numbers
    env = Environment.define( Core.Sym('>'),  Core.Native(liftComparisonBinOp('>',  (n, m) => n >  m)), env );
    env = Environment.define( Core.Sym('>='), Core.Native(liftComparisonBinOp('>=', (n, m) => n >= m)), env );
    env = Environment.define( Core.Sym('<'),  Core.Native(liftComparisonBinOp('<',  (n, m) => n <  m)), env );
    env = Environment.define( Core.Sym('<='), Core.Native(liftComparisonBinOp('<=', (n, m) => n <= m)), env );

    // just for numbers
    env = Environment.define( Core.Sym('+'), Core.Native(liftNumericBinOp('+', (n, m) => n + m)), env );
    env = Environment.define( Core.Sym('-'), Core.Native(liftNumericBinOp('-', (n, m) => n - m)), env );
    env = Environment.define( Core.Sym('*'), Core.Native(liftNumericBinOp('*', (n, m) => n * m)), env );
    env = Environment.define( Core.Sym('/'), Core.Native(liftNumericBinOp('/', (n, m) => n / m)), env );
    env = Environment.define( Core.Sym('%'), Core.Native(liftNumericBinOp('%', (n, m) => n % m)), env );

    // lambda special form
    env = Environment.define(
        Core.Sym('lambda'), Core.FExpr((arg : Core.Term, env : Core.Term) : [ Core.Term, Core.Term ] => {
            console.log(`▶ CALL @:(lambda) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg))     throw new Error(`Expected Pair as arg in @:(lambda) not(${arg.kind})`);
            if (!Core.isPair(arg.fst)) throw new Error(`Expected Pair as param in @:(lambda) not(${arg.fst.kind})`);
            if (!Core.isPair(arg.snd)) throw new Error(`Expected Pair as body in @:(lambda) not(${arg.snd.kind})`);
            let param = arg.fst;
            let body  = arg.snd;
            return [ Core.Lambda( param.fst, body.fst ), env ];
        }),
        env
    );

    env = Environment.define(
        Core.Sym('let'), Core.FExpr((arg : Core.Term, env : Core.Term) : [ Core.Term, Core.Term ] => {
            console.log(`▶ CALL @:(let) w/ ${Parser.deparse(arg)}`);
            if (!Core.isPair(arg))     throw new Error(`Expected Pair as arg in @:(let) not(${arg.kind})`);
            if (!Core.isPair(arg.fst)) throw new Error(`Expected Pair as bind in @:(let) not(${arg.fst.kind})`);
            let bind = arg.fst;
            if (!Core.isSym(bind.fst))  throw new Error(`Expected Sym as bind/sym in @:(let) not(${bind.fst.kind})`);
            if (!Core.isPair(bind.snd)) throw new Error(`Expected Pair as bind/value in @:(let) not(${bind.snd.kind})`);
            let sym   = bind.fst;
            let value = bind.snd; // need to evaluate this!
            let body  = arg.snd;
            if (!Core.isPair(body))  throw new Error(`Expected Pair as body in @:(let) not(${body.kind})`);
            return [
                body.fst,
                Environment.define( sym, evaluate(value.fst, env, -1), env )
            ];
        }),
        env
    );

    return env;
}

// -----------------------------------------------------------------------------
