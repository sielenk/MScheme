package MScheme.machine;

import MScheme.machine.Values;
import MScheme.exceptions.SException;
import MScheme.environment.Environment;
import MScheme.expressions.functions.Function;


class Context
{
    private Continuation _continuation;
    private Environment  _environment;


    protected Context(
        ContinuationStack stack,
        Environment       environment
    ) {
        _continuation = stack.getTop();
        _environment  = environment;
    }

    public Continuation getContinuation()
    {
        return _continuation;
    }

    public Environment getEnvironment()
    {
        return _environment;
    }
}


class Continuation extends Context
{
    private Function _function;

    /** push a new continuation on the given machine
     */
    Continuation(
        ContinuationStack stack,
        Environment       environment,
        Function          function
    ) {
        super(stack, environment);
        stack.setTop(this);
        _function = function;
    }


    /** pop the current continuation and call its associated function */
    Values invoke(
        ContinuationStack stack,
        Values            arguments
    ) throws SException {
        stack.setTop(getContinuation());

        return _function.call(
            stack,
            getEnvironment(),
            arguments
        );
    }
}
