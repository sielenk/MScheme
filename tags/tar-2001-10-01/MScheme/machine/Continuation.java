package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.CodeList;

import MScheme.exceptions.SchemeException;


/**
 * The base class for all continuations.
 */
public abstract class Continuation
{
    /** The CVS id of the file containing this class. */
    public final static String id
    = "$Id$";


    private final int       _level;
    private final Registers _capturedState;


    /**
     * Initializes a new continuation and pushes it on the stack.
     * Since it is pushed here, the result of the creation
     * by calling <code>new</code> is usually ignored.
     * <p>
     * @param state  the current state of the scheme machine.
     */
    protected Continuation(Registers state)
    {
        _capturedState = new Registers(state);
        _level =
            (getParent() != null)
            ? getParent()._level + 1
            : 0;

        state.setContinuation(this);
    }


    /**
     * Returns the content of the captured continuation register.
     */
    final Continuation getParent()
    {
        return _capturedState.getContinuation();
    }

    /**
     * Returns the depth of a continuation in the stack.
     */
    final int getLevel()
    {
        return _level;
    }

    /**
     *
     */
    CodeList dynamicWindLeave(CodeList sequence)
    {
        return sequence;
    }

    /**
     *
     */
    CodeList dynamicWindEnter(CodeList sequence)
    {
        return sequence;
    }


    /**
     * Restores the captured state and calls {@link #execute}.
     * The parameters are just passed on.
     * <p>
     * @return  the result of the call to {@link #execute}.
     */
    final Code invoke(Registers state, Value result)
    throws SchemeException
    {
        state.assign(_capturedState);
        return execute(state, result);
    }

    /**
     * Implements the concrete behaviour of the continuation.
     */
    protected abstract Code execute(Registers state, Value result)
    throws SchemeException;


    protected abstract String debugString();

    public final String toString()
    {
        StringBuffer buffer = new StringBuffer();

        for (
            Continuation current = this;
            current != null;
            current = current.getParent()
        )
        {
            buffer.append(current.debugString());
            buffer.append('\n');
        }

        return buffer.toString();
    }
}
