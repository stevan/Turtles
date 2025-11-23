
import type * as Types from './Types'
import type { Env } from './Env'

export type Evaluator = (expr : Types.Expr, ctx : Context) => Types.Expr

export class Context {
    public scope : Env[]        = [];
    public stack : Types.Expr[] = [];

    public evaluator : Evaluator;

    constructor(env : Env, evaluator : Evaluator) {
        this.scope.push(env);
        this.evaluator = evaluator;
    }

    get env () : Env {
        if (this.scope.length == 0) throw new Error('ENV STACK UNDERFLOW!');
        return this.scope.at(-1) as Env;
    }

    enterScope () : Env {
        let env = this.env.derive();
        this.scope.push(env);
        return env;
    }

    leaveScope () : void {
        this.scope.pop();
    }

    evaluate (expr : Types.Expr) : Types.Expr {
        return this.evaluator(expr, this);
    }
}
