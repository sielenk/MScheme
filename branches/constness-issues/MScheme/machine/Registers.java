package MScheme.machine;

import MScheme.environment.Environment;
import MScheme.functions.UnaryFunction;


public class Registers
{
    public final static String id
        = "$Id$";

    private Continuation _continuation;
    private Environment  _environment;

    Registers(Environment environment)
    {
        _continuation = null;
        _environment  = environment;
    }

    Registers(Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;    
    }

    void assign(Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;    
    }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }
    
    Continuation getContinuation()
    { return _continuation; }


    public UnaryFunction getCurrentContinuation()
    { return new ContinuationFunction(_continuation); }

    public Environment getEnvironment()
    { return _environment; }

    public void setEnvironment(Environment newEnvironment)
    { _environment = newEnvironment; }    
}
