package MScheme.machine;


import MScheme.util.Values;
import MScheme.exceptions.SException;
import MScheme.environment.Environment;
import MScheme.expressions.SFunction;


class Continuation extends Context
{
    private int       _depth;
    private SFunction _function;


    public final static Continuation HALT
        = new Continuation();


    private Continuation()
    {
        super(null, null);
        _depth    = 0;
        _function = null;
    }


    Continuation(
        Continuation continuation,
        Environment  environment,
        SFunction    function
    ) {
        super(
            continuation,
            environment
        );
        _depth    = continuation._depth + 1;
        _function = function;
    }


    Values invoke(
        Machine machine,
        Values  arguments
    ) throws SException {
        System.err.print("[" + _depth + "] ");

        return _function.call(
            machine,
            arguments
        );
    }
}
