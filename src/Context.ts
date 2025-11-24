
import type * as Types from './Types'
import type { Env } from './Env'

export type Evaluator = (expr : Types.Expr) => Types.Expr

export class Context {
    public scope : Env[]        = [];
    public stack : Types.Expr[] = [];

    public evaluate : Evaluator;

    constructor(env : Env, evaluator : Evaluator) {
        this.scope.push(env);
        this.evaluate = evaluator;
    }

    get env () : Env {
        if (this.scope.length == 0) throw new Error('ENV STACK UNDERFLOW!');
        return this.scope.at(-1) as Env;
    }

    enterScope (env : Env) : Env {
        let local = env.derive();
        this.scope.push(local);
        return local;
    }

    leaveScope () : void {
        this.scope.pop();
    }
}
