package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import MScheme.values.*;
import MScheme.code.*;
import MScheme.exceptions.*;
import MScheme.functions.UnaryFunction;
import MScheme.environment.*;


class ContinuationFunction
    extends UnaryFunction
{
    private final Continuation _continuation;
    
    ContinuationFunction(Continuation continuation)
    { _continuation = continuation; }


    private CodeList dynamicWind(
        Continuation source,
        Continuation destination
    )
    {
        CodeList     result = CodeList.create();
        Continuation from   = source;
        Continuation to     = destination;
        
        while (from != to) {
            Continuation newFrom = from;
            Continuation newTo  = to;
        
            if (from.getLevel() >= to.getLevel()) {
                newFrom = from.getParent();
            }
            
            if (to.getLevel() >= from.getLevel()) {
                newTo = to.getParent();
            }
            
            from = newFrom;
            to   = newTo;
        }
        
        return result;
    }


    // implementation of Value
    
    public void put(Writer destination, boolean doDisplay)
        throws IOException
    { destination.write("[continuation]"); }
    
    
    // implementation of UnaryFunction
    
    protected Code checkedCall(Machine machine, Value argument)
        throws SchemeException
    {
        CodeList frames = dynamicWind(
            machine.getContinuation(),
            _continuation
        );
            
        return _continuation.invoke(machine, argument);
    }
}


abstract class Continuation
{
    final private int                _level;
    final private DynamicEnvironment _capturedEnvironment;
    final private Continuation       _capturedContinuation;


    Continuation(Machine machine)
    {
        _capturedEnvironment  = machine.getEnvironment();
        _capturedContinuation = machine.getContinuation();

        _level =
            (_capturedContinuation != null)
            ? _capturedContinuation._level + 1
            : 0;

        machine.setContinuation(this);
    }

    final int getLevel()
    { return _level; }

    final Continuation getParent()
    { return _capturedContinuation; }
    

    final public Code invoke(Machine machine, Value value)
        throws SchemeException
    {
        machine.setEnvironment (_capturedEnvironment);
        machine.setContinuation(_capturedContinuation);
        
        return internalInvoke(machine, value);
    }
 

    abstract Code internalInvoke(Machine machine, Value value)
        throws SchemeException;
        
        
    final public UnaryFunction getFunction()
    { return new ContinuationFunction(this); }
}

