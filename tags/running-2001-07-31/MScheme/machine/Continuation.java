package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import java.util.Stack;

import MScheme.environment.DynamicEnvironment;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.functions.UnaryFunction;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Continuation
    extends UnaryFunction
{
    final private int                _level;
    final private DynamicEnvironment _capturedEnvironment;
    final private Continuation       _capturedContinuation;


    protected Continuation(Machine machine)
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
        throws RuntimeError, TypeError
    {
        machine.setEnvironment (_capturedEnvironment);
        machine.setContinuation(_capturedContinuation);
        
        return execute(machine, value);
    }
 

    abstract protected Code execute(
        Machine machine,
        Value   value
    ) throws RuntimeError, TypeError;
        
        
    private static void dynamicWind(
        Machine      machine,
        Continuation source,
        Continuation destination
    )
    {
        Continuation   from   = source;
        Continuation   to     = destination;
        Continuation[] stack  = new Continuation[to.getLevel()];
        int            sp     = 0;
        
        while (from != to) {
            Continuation newFrom = from;
            Continuation newTo   = to;

            if (from.getLevel() >= to.getLevel()) {
                newFrom = from.getParent();
                from.leave(machine);
            }

            if (to.getLevel() >= from.getLevel()) {
                newTo = to.getParent();
                stack[sp++] = to;
            }

            from = newFrom;
            to   = newTo;
        }

        while (sp > 0) {
            stack[--sp].enter(machine);
        }
    }


    // implementation of Value

    public void write(Writer destination)
        throws IOException
    { destination.write("[continuation]"); }


    // implementation of UnaryFunction
    
    protected Code checkedCall(Machine machine, Value argument)
    {
        Continuation source      = machine.getContinuation();
        Continuation destination = this;
        
        machine.setContinuation(destination);
        new ValueContinuation(machine, argument);

        dynamicWind(machine, source, destination);

        // this null will end up in a CallThunkContinuation
        // created by dynamicWind or in the ValueContinuation
        // created above ... both ignore the given value
        return new Literal(null);
    }
}
