package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;
import MScheme.code.Sequence;
import MScheme.functions.UnaryFunction;


class ContinuationFunction
    extends UnaryFunction
{
    public final static String id
        = "$Id$";

    private final Continuation _continuation;

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
    
    protected Code checkedCall(Registers registers, Value argument)
    {
        Continuation source      = registers.getContinuation();
        Continuation destination = _continuation;
        
        registers.setContinuation(destination);

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
