package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.Environment;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.code.Sequence;
import MScheme.values.Value;
import MScheme.functions.UnaryFunction;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


class ContinuationFunction
    extends UnaryFunction
{
    final private Continuation _continuation;

    ContinuationFunction(Continuation continuation)
    { _continuation = continuation; }

    private static CodeList dynamicWind(
        Continuation source,
        Continuation destination,
        CodeList     tail
    )
    {
        CodeList       sequence = tail;
        Continuation   from     = source;
        Continuation   to       = destination;
        Continuation[] stack    = new Continuation[from.getLevel()];
        int            sp       = 0;

        while (from != to) {
            Continuation newFrom = from;
            Continuation newTo   = to;

            if (from.getLevel() >= to.getLevel()) {
                newFrom     = from.getParent();
                stack[sp++] = from;
            }

            if (to.getLevel() >= from.getLevel()) {
                newTo    = to.getParent();
                sequence = to.dynamicWindEnter(sequence);
            }

            from = newFrom;
            to   = newTo;
        }

        while (sp > 0) {
            sequence = stack[--sp].dynamicWindLeave(sequence);
        }
    
        return sequence;
    }


    // implementation of Value

    public void write(Writer destination)
        throws IOException
    { destination.write("[continuation]"); }


    // implementation of UnaryFunction
    
    protected Code checkedCall(Machine machine, Value argument)
    {
        Continuation source      = machine.getContinuation();
        Continuation destination = _continuation;
        
        machine.setContinuation(destination);

        return Sequence.create(
            dynamicWind(
                source,
                destination,
                CodeList.create(
                    argument.getLiteral()
                )
            )
        );
    }
}


public abstract class Continuation
{
    final private int          _level;
    final private Environment  _capturedEnvironment;
    final private Continuation _capturedContinuation;


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
    
    final UnaryFunction getFunction()
    { return new ContinuationFunction(this); }

    final Code invoke(Machine machine, Value value)
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

    protected CodeList dynamicWindLeave(CodeList sequence)
    { return sequence; }
    
    protected CodeList dynamicWindEnter(CodeList sequence)
    { return sequence; }
}
