package MScheme.machine;


import MScheme.environment.Environment;

import MScheme.expressions.SExpr;
import MScheme.expressions.SFunction;
import MScheme.expressions.functions.EvalFunc;

import MScheme.exceptions.SException;


class MutableContext extends Context
{
    protected MutableContext(
        Continuation continuation,
        Environment  environment
    ) {
        super(
            continuation,
            environment
        );
    }


    protected void setContinuation(
        Continuation continuation
    ) {
        _continuation = continuation;
    }


    protected void setEnvironment(
        Environment environment
    )
    {
        _environment = environment;
    }


    protected void setContext(
        Context context
    ) {
        _continuation = context._continuation;
        _environment  = context._environment;
    }
}


public class Machine extends MutableContext
{
    public Machine(
        Environment environment
    ) {
        super(
            Continuation.HALT,
            environment
        );
    }


    private Continuation pop()
    {
        Continuation continuation = getContinuation();
        setContext(continuation);
        return continuation;
    }


    public void push(
        SFunction function
    ) {
        setContinuation(
            new Continuation(
                getContinuation(),
                getEnvironment(),
                function
            )
        );
    }


    public void push(
        SFunction   function,
        Environment environment
    ) {
        setContinuation(
            new Continuation(
                getContinuation(),
                environment,
                function
            )
        );
    }


    public SExpr evaluate(
        SExpr sexpr
    ) throws SException {
        Values accu  = new Values(sexpr);

        push(EvalFunc.INSTANCE);

        for (;;) {
            Continuation cont = pop();

            if (cont == Continuation.HALT) {
                return accu.at(0);
            }

            accu = cont.invoke(this, accu);
        }
    }
}
