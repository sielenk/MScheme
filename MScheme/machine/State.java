package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.environment.Environment;
import MScheme.functions.UnaryFunction;


class AbortContinuation
    extends Continuation
{
    private Value _result;

    AbortContinuation(State state)
    { super(state); _result = null; }

    Value getResult()
    { return _result; }

    protected Code execute(State state, Value evaluationResult)
    { _result = evaluationResult; return null; }
}


public class State
{
    private Continuation _continuation;
    private Environment  _environment;
    final private AbortContinuation _abort;

    State(Environment environment)
    {
        _continuation = null;
        _environment  = environment;
        _abort        = new AbortContinuation(this);
    }

    Value getResult()
    { return _abort.getResult(); }
    
    boolean isHalt()
    { return _abort.getResult() != null; }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }
    
    Continuation getContinuation()
    { return _continuation; }


    public UnaryFunction getCurrentContinuation()
    { return _continuation.getFunction(); }

    public Environment getEnvironment()
    { return _environment; }

    public void setEnvironment(Environment newEnvironment)
    { _environment = newEnvironment; }    
}
