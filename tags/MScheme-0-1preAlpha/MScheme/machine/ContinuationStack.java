package MScheme.machine;


import MScheme.expressions.functions.Function;
import MScheme.environment.Environment;


public class ContinuationStack
{
    private Continuation _halt;
    private Continuation _continuation;


    protected ContinuationStack(
        Environment environment
    ) {
        _halt = _continuation = new Continuation(
            this,
            environment,
            null
        );
    }


    protected void setTop(Continuation continuation)
    {
        _continuation = continuation;
    }


    protected Continuation getTop()
    {
        return _continuation;
    }


    protected boolean isEmpty()
    {
        return _halt == _continuation;
    }


    public void push(
        Environment environment,
        Function    function
    ) {
        new Continuation(
            this,
            environment,
            function
        );
    }
}
