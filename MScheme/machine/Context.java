package MScheme.machine;


import MScheme.environment.Environment;


class Context
{
    protected Continuation _continuation;
    protected Environment  _environment;


    protected Context(
        Continuation continuation,
        Environment  environment
    ) {
        _continuation = continuation;
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
