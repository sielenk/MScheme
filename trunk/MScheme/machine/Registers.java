package MScheme.machine;

import MScheme.environment.Environment;
import MScheme.functions.UnaryFunction;


/**
 * The state of a scheme machine.
 */
public final class Registers
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";

    private Continuation _continuation;
    private Environment  _environment;

    
    /** */
    Registers(Environment environment)
    {
        _continuation = null;
        _environment  = environment;
    }

    /** Creates a copy of other. */
    Registers(final Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;    
    }

    /** This is an assignment operator. */
    void assign(final Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;    
    }

    void setContinuation(Continuation newContinuation)
    { _continuation = newContinuation; }
    
    Continuation getContinuation()
    { return _continuation; }


    /** Returns the current continuation value. */
    public UnaryFunction getCurrentContinuation()
    { return new ContinuationFunction(_continuation); }

    /** Returns the current environment. */
    public Environment getEnvironment()
    { return _environment; }

    /** Changes the current environment. */
    public void setEnvironment(Environment newEnvironment)
    { _environment = newEnvironment; }    
}
