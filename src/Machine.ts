
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

import { Env, MaybeEnv } from './Env'
import {
    DEBUG,
    createBaseEnvironment
} from './Runtime'

export type Operation = [ string, Types.Expr[] ]

export class Machine {
    public env : Env;

    constructor (env? : MaybeEnv) {
        this.env = env ?? this.createRootEnvironment()
    }

    run (expr : Types.Expr) : Types.Expr {

        let queue : Operation[] = [
            [ 'HALT', [] ],
            [ 'EVAL', [ expr ] ]
        ];

        let result;

        while (queue.length > 0) {
            console.log('__ TICK '+('_'.repeat(72)));

            let operation = queue.pop() as Operation;

            if (this.step(operation, this.env, queue)) {
                console.log('='.repeat(80));
                console.log(` %ENV :`, DEBUG.DUMP(this.env));
                console.log('QUEUE :', queue.map(([op, args]) => `${op}[${
                    args.map(DEBUG.SHOW).join(', ')
                }]`).join('; '));
                console.log('-'.repeat(80));
            } else {
                let [ op, exprs ] = operation;

                console.log('!! HALT '+('_'.repeat(72)));
                console.log(` %ENV :`, DEBUG.DUMP(this.env));
                console.log('QUEUE :', queue.map(([op, args]) => `${op}[${
                    args.map(DEBUG.SHOW).join(', ')
                }]`).join('; '));
                console.log('^OPER :', `${op}[${exprs.map(DEBUG.SHOW).join(', ')}]`);
                console.log('-'.repeat(80));

                result = exprs.shift() as Types.Expr;
                break;
            }
        }

        return result ?? AST.Nil();
    }

    step (operation : Operation, env : Env, queue : Operation[]) : boolean {
        let [ op, exprs ] = operation;

        let caller = queue.at(-1) as Operation;

        switch (op) {
        case 'EVAL':
            let [ expr, ...rest ] = exprs;
            queue.push( this.evaluate( expr as Types.Expr, env ) );
            if (rest.length > 0) queue.push([ 'JUST', rest ])
            return true;
        case 'EHEAD':
            let cons = exprs.at(0) as Types.Cons;
            queue.push(
                [ 'CALL?', [ cons.tail ] ],
                [ 'EVAL',  [ cons.head ] ],
            );
            return true;
        case 'APPLY':
            let [ callable, ...evaledArgs ] = exprs;
            queue.push( this.apply( callable as Types.Callable, evaledArgs, env ) );
            return true;
        case 'CALL?':
            let [ args, call ] = exprs;
            if (Util.Type.isCallable(call)) {
                queue.push(
                    [ 'APPLY', [ call as Types.Callable ] ],
                    [ 'EVAL',  [ args as Types.Expr ] ],
                );
            } else {
                queue.push(
                    [ 'JUST', [ call as Types.Expr ] ],
                    [ 'EVAL', [ args as Types.Expr ] ],
                );
            }
            return true;
        case 'JUST':
            caller[1].push( ...exprs );
            return true;
        case 'HALT':
            return false;
        default:
            throw new Error(`Unrecognized op (${op}`);
        }
    }


    evaluate (expr : Types.Expr, env : Env) : Operation {
        console.log(`>> EVAL [${expr.type}]`, DEBUG.SHOW(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  :
            return [ 'JUST', [ expr ] ];
        case 'SYM'   :
            return [ 'JUST', [ env.lookup(expr) ] ];
        case 'CONS'  :
            return [ 'EHEAD', [ expr ] ]
        case 'NIL'   :
            return [ 'JUST', [ expr ] ]
        default:
            throw new Error('FUCK!');
        }
    }

    apply (call : Types.Callable, args : Types.Expr[], env : Env) : Operation {
        console.log(`>> APPLY ${DEBUG.SHOW(call)} -> `, args.map(DEBUG.SHOW));

        const makeLocalEnv = () => {
            let params = Util.List.flatten(call.params);
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                Util.Type.assertSym(param);
                if (arg == undefined) throw new Error('BAD ARG!');
                localE.assign( param, arg as Types.Expr );
            }
            console.log(`(local) %ENV :`, DEBUG.DUMP(localE));
            return localE;
        }

        switch (call.type) {
        case 'FEXPR':
            return [ 'EVAL', [ call.body( args, env ) ] ];
        case 'NATIVE':
            return [ 'JUST', [ call.body( makeLocalEnv() ) ] ];
        case 'LAMBDA':
            return [ 'CALL', [ call.body ] ]; // BROKEN!
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    createRootEnvironment () : Env {
        let env = createBaseEnvironment();

        // TODO

        return env;
    }
}
