package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import java.util.Stack;

import MScheme.environment.DynamicEnvironment;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.functions.UnaryFunction;

import MScheme.exceptions.SchemeException;


class ContinuationFunction
    extends UnaryFunction
{
    private final Continuation _continuation;
    
    ContinuationFunction(Continuation continuation)
    { _continuation = continuation; }


    private void dynamicWind(
        Machine      machine,
        Continuation source,
        Continuation destination
    )
    {
        Stack        stack  = new Stack();
        Continuation from   = source;
        Continuation to     = destination;
        
        while (from != to) {
            Continuation newFrom = from;
            Continuation newTo   = to;

            if (from.getLevel() >= to.getLevel()) {
                newFrom = from.getParent();
                from.leave(machine);
            }

            if (to.getLevel() >= from.getLevel()) {
                newTo = to.getParent();
                stack.push(to);
            }

            from = newFrom;
            to   = newTo;
        }

        while (!stack.isEmpty()) {
            ((Continuation)stack.pop()).enter(machine);
        }
    }


    // implementation of Value

    public void write(Writer destination)
        throws IOException
    { destination.write("[continuation]"); }


    // implementation of UnaryFunction
    
    protected Code checkedCall(Machine machine, Value argument)
        throws SchemeException
    {
        Continuation source      = machine.getContinuation();
        Continuation destination = _continuation;
        
        machine.setContinuation(destination);
        new ValueContinuation(machine, argument);

        dynamicWind(machine, source, destination);

        return machine.handleResult(null);
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
    
    protected void leave(Machine machine) { }
    protected void enter(Machine machine) { }

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

