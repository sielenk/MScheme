package MScheme.machine;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;
import MScheme.code.Sequence;
import MScheme.functions.UnaryFunction;


/**
 * Continuations are first class objects in scheme. They
 * seem to behave like unary functions. But if such a
 * function is called, continuations waiting to be invoked
 * are skipped.
 */
final class ContinuationFunction
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final Continuation _continuation;

    ContinuationFunction(Continuation continuation)
    {
        _continuation = continuation;
    }

    private static CodeList dynamicWind(
        Continuation source,
        Continuation destination,
        CodeList     tail
    )
    {
        CodeList       sequence = tail;
        Continuation   from     = source;
        Continuation   to       = destination;
        Continuation[] stack    = new Continuation[Continuation.getLevel(from)];
        int            sp       = 0;

        while (from != to)
        {
            final int fromLevel = Continuation.getLevel(from);
            final int   toLevel = Continuation.getLevel(to  );

            if (fromLevel >= toLevel)
            {
                stack[sp++] = from;
                from        = from.getParent();
            }

            if (toLevel >= fromLevel)
            {
                sequence = to.dynamicWindEnter(sequence);
                to       = to.getParent();
            }
        }

        while (sp > 0)
        {
            sequence = stack[--sp].dynamicWindLeave(sequence);
        }

        return sequence;
    }


    // implementation of Value

    public void write(Writer destination)
    throws IOException
    {
        destination.write(
            "#[continuation]"
        );
    }

    public void display(Writer destination)
    throws IOException
    {
        destination.write(
            "#[continuation\n"
            + _continuation.toString()
            + "]"
        );
    }


    // implementation of UnaryFunction

    /**
     * 
     *
     * @returns  the code created during the stack unwinding.
     */
    protected Code checkedCall(Registers state, Value argument)
    {
        Continuation source      = state.getContinuation();
        Continuation destination = _continuation;

        state.setContinuation(destination);

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
